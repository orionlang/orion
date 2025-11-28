// Work-stealing scheduler for Orion
// Based on Go's GMP model: G (goroutines/tasks), M (OS threads), P (processors/workers)

const std = @import("std");
const Atomic = std.atomic.Value;
const Thread = std.Thread;
const Mutex = std.Thread.Mutex;
const Condition = std.Thread.Condition;

// Task handle (coroutine pointer from LLVM)
pub const Task = *anyopaque;

// Coroutine wrapper functions (generated in compiled code, wrapping LLVM intrinsics)
// For tests, we provide stub implementations since the real ones come from compiled programs
const builtin = @import("builtin");

const orion_coro_done = if (builtin.is_test)
    struct {
        fn f(_: Task) bool {
            return true; // Stub: always done for tests
        }
    }.f
else
    @extern(*const fn (Task) callconv(.c) bool, .{ .name = "orion_coro_done" }).*;

const orion_coro_resume = if (builtin.is_test)
    struct {
        fn f(_: Task) void {} // Stub: no-op for tests
    }.f
else
    @extern(*const fn (Task) callconv(.c) void, .{ .name = "orion_coro_resume" }).*;

// Work-stealing deque for per-worker local queues
// Lock-free for push/pop by owner, lock for steal by others
pub const WorkDeque = struct {
    const CAPACITY = 256;

    tasks: [CAPACITY]Task = undefined,
    top: Atomic(usize) = Atomic(usize).init(0), // Owner pops from top
    bottom: Atomic(usize) = Atomic(usize).init(0), // Owner pushes to bottom
    mutex: Mutex = .{}, // For stealing

    // Push task (called by owner only)
    pub fn push(self: *WorkDeque, task: Task) bool {
        const b = self.bottom.load(.acquire);
        const t = self.top.load(.acquire);

        if (b - t >= CAPACITY) {
            return false; // Full
        }

        self.tasks[b % CAPACITY] = task;
        self.bottom.store(b + 1, .release);
        return true;
    }

    // Pop task (called by owner only)
    pub fn pop(self: *WorkDeque) ?Task {
        var b = self.bottom.load(.acquire);
        if (b == 0) return null;

        b -= 1;
        self.bottom.store(b, .release);

        const t = self.top.load(.acquire);
        if (t <= b) {
            // Non-empty
            const task = self.tasks[b % CAPACITY];
            if (t == b) {
                // Last element, race with stealers
                if (self.top.cmpxchgStrong(t, t + 1, .acq_rel, .acquire) != null) {
                    // Lost race, queue is empty
                    self.bottom.store(t + 1, .release);
                    return null;
                }
                self.bottom.store(t + 1, .release);
            }
            return task;
        } else {
            // Empty
            self.bottom.store(t, .release);
            return null;
        }
    }

    // Steal task (called by other workers)
    pub fn steal(self: *WorkDeque) ?Task {
        self.mutex.lock();
        defer self.mutex.unlock();

        const t = self.top.load(.acquire);
        const b = self.bottom.load(.acquire);

        if (t >= b) return null; // Empty

        const task = self.tasks[t % CAPACITY];
        if (self.top.cmpxchgStrong(t, t + 1, .acq_rel, .acquire) != null) {
            return null; // Lost race
        }
        return task;
    }

    pub fn len(self: *WorkDeque) usize {
        const t = self.top.load(.acquire);
        const b = self.bottom.load(.acquire);
        return if (b > t) b - t else 0;
    }
};

// Worker (P in Go's model)
pub const Worker = struct {
    id: usize,
    local_queue: WorkDeque = .{},
    thread: ?Thread = null,
    running: Atomic(bool) = Atomic(bool).init(false),
    current_task: ?Task = null,

    pub fn run(self: *Worker) void {
        self.running.store(true, .release);

        while (global.running.load(.acquire)) {
            // Try to get work
            if (self.findWork()) |task| {
                self.current_task = task;
                self.executeTask(task);
                self.current_task = null;
            } else {
                // No work, park briefly
                std.Thread.sleep(100_000); // 100µs
            }
        }

        self.running.store(false, .release);
    }

    fn findWork(self: *Worker) ?Task {
        // 1. Try local queue first
        if (self.local_queue.pop()) |task| {
            return task;
        }

        // 2. Try global queue
        if (global.popGlobal()) |task| {
            return task;
        }

        // 3. Try to steal from other workers
        return self.trySteal();
    }

    fn trySteal(self: *Worker) ?Task {
        const num_workers = global.num_workers.load(.acquire);
        if (num_workers <= 1) return null;

        // Simple round-robin starting from next worker
        const start = (self.id + 1) % num_workers;

        for (0..num_workers) |i| {
            const victim_id = (start + i) % num_workers;
            if (victim_id == self.id) continue;

            if (global.workers[victim_id]) |victim| {
                if (victim.local_queue.steal()) |task| {
                    return task;
                }
            }
        }
        return null;
    }

    fn executeTask(self: *Worker, task: Task) void {
        _ = self;

        // Check if already done
        if (llvm_coro_done(task)) return;

        // Resume the coroutine
        llvm_coro_resume(task);

        // If not done after resume, it yielded - re-enqueue
        if (!llvm_coro_done(task)) {
            spawn(task);
        }
    }
};

// Wrapper functions calling extern wrappers (generated in compiled code)
fn llvm_coro_done(task: Task) bool {
    return orion_coro_done(task);
}

fn llvm_coro_resume(task: Task) void {
    orion_coro_resume(task);
}

// Global scheduler state
const GlobalState = struct {
    const MAX_WORKERS = 64;
    const GLOBAL_QUEUE_SIZE = 4096;

    workers: [MAX_WORKERS]?*Worker = [_]?*Worker{null} ** MAX_WORKERS,
    num_workers: Atomic(usize) = Atomic(usize).init(0),
    running: Atomic(bool) = Atomic(bool).init(false),

    // Global run queue (for tasks without affinity)
    global_queue: [GLOBAL_QUEUE_SIZE]Task = undefined,
    global_head: Atomic(usize) = Atomic(usize).init(0),
    global_tail: Atomic(usize) = Atomic(usize).init(0),
    global_mutex: Mutex = .{},

    // Current worker for this thread (thread-local would be better)
    current_worker_id: Atomic(usize) = Atomic(usize).init(0),

    pub fn pushGlobal(self: *GlobalState, task: Task) bool {
        self.global_mutex.lock();
        defer self.global_mutex.unlock();

        const tail = self.global_tail.load(.acquire);
        const head = self.global_head.load(.acquire);

        if (tail - head >= GLOBAL_QUEUE_SIZE) {
            return false; // Full
        }

        self.global_queue[tail % GLOBAL_QUEUE_SIZE] = task;
        self.global_tail.store(tail + 1, .release);
        return true;
    }

    pub fn popGlobal(self: *GlobalState) ?Task {
        self.global_mutex.lock();
        defer self.global_mutex.unlock();

        const head = self.global_head.load(.acquire);
        const tail = self.global_tail.load(.acquire);

        if (head >= tail) return null; // Empty

        const task = self.global_queue[head % GLOBAL_QUEUE_SIZE];
        self.global_head.store(head + 1, .release);
        return task;
    }
};

var global: GlobalState = .{};
var worker_storage: [GlobalState.MAX_WORKERS]Worker = undefined;

// Determine number of workers (like GOMAXPROCS)
fn getNumWorkers() usize {
    // Default to number of CPU cores, capped at MAX_WORKERS
    const cpus = std.Thread.getCpuCount() catch 4;
    return @min(cpus, GlobalState.MAX_WORKERS);
}

// Public API (exported to C)

pub export fn init() void {
    if (global.running.load(.acquire)) return; // Already initialized

    const num = getNumWorkers();
    global.num_workers.store(num, .release);
    global.running.store(true, .release);

    // Initialize workers
    for (0..num) |i| {
        worker_storage[i] = Worker{ .id = i };
        global.workers[i] = &worker_storage[i];
    }

    // Start worker threads (except worker 0 which is the main thread)
    for (1..num) |i| {
        if (global.workers[i]) |worker| {
            worker.thread = Thread.spawn(.{}, Worker.run, .{worker}) catch null;
        }
    }
}

pub export fn spawn(task: Task) void {
    if (!global.running.load(.acquire)) {
        init();
    }

    // Try to push to current worker's local queue first
    const worker_id = global.current_worker_id.load(.acquire);
    if (global.workers[worker_id]) |worker| {
        if (worker.local_queue.push(task)) {
            return;
        }
    }

    // Fallback to global queue
    _ = global.pushGlobal(task);
}

pub export fn yield() void {
    // Get current task and re-enqueue it
    const worker_id = global.current_worker_id.load(.acquire);
    if (global.workers[worker_id]) |worker| {
        if (worker.current_task) |task| {
            spawn(task);
        }
    }
}

pub export fn run() void {
    if (!global.running.load(.acquire)) return;

    // Main thread runs as worker 0
    if (global.workers[0]) |worker| {
        global.current_worker_id.store(0, .release);

        // Run until no work remains anywhere
        while (true) {
            if (worker.findWork()) |task| {
                worker.current_task = task;
                worker.executeTask(task);
                worker.current_task = null;
            } else {
                // Check if any worker has work
                var has_work = false;
                const num = global.num_workers.load(.acquire);
                for (0..num) |i| {
                    if (global.workers[i]) |w| {
                        if (w.local_queue.len() > 0) {
                            has_work = true;
                            break;
                        }
                    }
                }

                const head = global.global_head.load(.acquire);
                const tail = global.global_tail.load(.acquire);
                if (tail > head) has_work = true;

                if (!has_work) break;

                std.Thread.sleep(10_000); // 10µs
            }
        }
    }
}

pub export fn run_until(target: Task) void {
    if (!global.running.load(.acquire)) return;

    // Main thread runs as worker 0
    if (global.workers[0]) |worker| {
        global.current_worker_id.store(0, .release);

        // Run until target task is done
        while (!llvm_coro_done(target)) {
            if (worker.findWork()) |task| {
                worker.current_task = task;
                worker.executeTask(task);
                worker.current_task = null;
            } else {
                // Brief sleep to avoid spinning
                std.Thread.sleep(10_000); // 10µs
            }
        }
    }
}

pub export fn shutdown() void {
    global.running.store(false, .release);

    // Wait for worker threads to finish
    const num = global.num_workers.load(.acquire);
    for (1..num) |i| {
        if (global.workers[i]) |worker| {
            if (worker.thread) |thread| {
                thread.join();
            }
        }
    }

    global.num_workers.store(0, .release);
}

// Tests
test "work deque push/pop" {
    var deque = WorkDeque{};

    const task1: Task = @ptrFromInt(0x1000);
    const task2: Task = @ptrFromInt(0x2000);

    try std.testing.expect(deque.push(task1));
    try std.testing.expect(deque.push(task2));
    try std.testing.expectEqual(@as(usize, 2), deque.len());

    try std.testing.expectEqual(task2, deque.pop());
    try std.testing.expectEqual(task1, deque.pop());
    try std.testing.expectEqual(@as(?Task, null), deque.pop());
}

test "work deque steal" {
    var deque = WorkDeque{};

    const task1: Task = @ptrFromInt(0x1000);
    const task2: Task = @ptrFromInt(0x2000);

    _ = deque.push(task1);
    _ = deque.push(task2);

    // Steal takes from opposite end
    try std.testing.expectEqual(task1, deque.steal());
    try std.testing.expectEqual(task2, deque.steal());
    try std.testing.expectEqual(@as(?Task, null), deque.steal());
}
