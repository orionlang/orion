# Plan: Go-Style Concurrency for Orion

This document outlines the steps to evolve Orion's concurrency model from cooperative coroutines to a Go-like goroutine model.

## Current State

Orion currently has:
- `spawn { body }` - creates LLVM stackless coroutine, returns Task[T]
- `@yield()` - explicit suspension point
- `@join(task, @type(T))` - drives coroutine to completion, returns result
- Linear types enforce Task[T] must be joined exactly once
- No runtime scheduler - @join directly calls coro.resume in a loop

## Target State (Go-like)

Go's model provides:
- Lightweight goroutines multiplexed onto OS threads (M:N)
- Preemptive scheduling at safe points
- Channels for communication (not shared memory)
- Fire-and-forget spawning with `go func()`
- Automatic yielding on blocking operations

## Design Decisions

### Keep from Current Model
- **Structured concurrency** - tasks scoped to async blocks (safer than Go)
- **Linear types** - static enforcement of task lifecycle
- **Explicit spawn syntax** - clearer than Go's `go` prefix

### Adopt from Go
- **Runtime scheduler** - manage task queue, work stealing
- **Channels** - typed, buffered/unbuffered communication
- **Automatic yield points** - at function calls, channel ops, allocations
- **Select statement** - multiplex channel operations

### Diverge from Go
- **Keep @join** - but make it scheduler-aware (yield while waiting)
- **No global goroutines** - all tasks within async scopes
- **Optional parallelism** - single-threaded by default, opt-in multi-threading

---

## Phase 1: Runtime Scheduler

### 1.1 Task Queue Data Structure

Create a runtime task queue to hold ready coroutines.

```
// In stdlib or as compiler-generated runtime
type TaskQueue = {
    tasks: ptr,      // Array of coroutine handles
    count: u64,
    capacity: u64,
    head: u64,       // For round-robin
}
```

**Implementation:**
- Add `orion_scheduler_init()` - initialize global scheduler
- Add `orion_scheduler_spawn(handle: ptr)` - enqueue task
- Add `orion_scheduler_yield()` - save current, resume next
- Add `orion_scheduler_run()` - run until all tasks complete

### 1.2 Modify spawn Codegen

Change spawn to register with scheduler instead of returning handle directly.

```
// Current: spawn creates coro and returns handle
let task = spawn { ... }

// New: spawn registers with scheduler, still returns handle for join
let task = spawn { ... }  // internally calls orion_scheduler_spawn
```

**Codegen changes:**
- After `call ptr @orion_coro_N()`, emit `call void @orion_scheduler_spawn(ptr %handle)`
- Task handle still returned for @join tracking

### 1.3 Modify @yield Codegen

Change @yield to return control to scheduler, not just suspend.

```
// Current: @yield suspends, @join resumes
// New: @yield returns to scheduler, scheduler picks next task
```

**Codegen changes:**
- @yield calls `orion_scheduler_yield()` instead of just coro.suspend
- Scheduler saves current task state, resumes next ready task

### 1.4 Modify async Block Codegen

Async blocks should run scheduler at exit.

```
let result = async {
    spawn { ... }
    spawn { ... }
    // At end: run scheduler until all spawned tasks complete
}
```

**Codegen changes:**
- Track async scope depth
- At async block exit, emit `call void @orion_scheduler_run()`

---

## Phase 2: Channels

### 2.1 Channel Type

Add `Chan[T]` as a built-in generic type.

```
type Chan[T] = {
    buffer: ptr,
    capacity: u64,
    count: u64,
    head: u64,
    tail: u64,
    send_waiters: ptr,   // Tasks blocked on send
    recv_waiters: ptr,   // Tasks blocked on recv
    closed: bool,
}
```

### 2.2 Channel Operations

**Create:**
```
let ch: Chan[i32] = @channel(10)  // buffered, capacity 10
let ch: Chan[i32] = @channel(0)   // unbuffered (synchronous)
```

**Send:**
```
ch <- value      // blocks if buffer full (unbuffered: blocks until recv)
```

**Receive:**
```
let value: i32 = <-ch   // blocks if buffer empty
```

**Close:**
```
@close(ch)       // signals no more values
```

### 2.3 Channel Codegen

**Send operation:**
1. Check if buffer has space (or receiver waiting for unbuffered)
2. If yes: write value, wake receiver if any
3. If no: add current task to send_waiters, yield to scheduler

**Receive operation:**
1. Check if buffer has data (or sender waiting for unbuffered)
2. If yes: read value, wake sender if any
3. If no: add current task to recv_waiters, yield to scheduler

### 2.4 Implement select Statement

Full select with channel operations:

```
select {
    value <- ch1 => { /* received value from ch1 */ }
    ch2 <- 42 => { /* sent 42 to ch2 */ }
    default => { /* non-blocking: no channel ready */ }
}
```

**Implementation:**
1. Poll all channels for readiness
2. If any ready: execute that arm
3. If none ready and no default: register on all wait queues, yield
4. If none ready and default: execute default arm

---

## Phase 3: Automatic Yield Points

### 3.1 Function Call Yield Points

Insert yield checks at function calls (Go's approach).

**Option A: Cooperative check**
- At function entry, check if yield requested
- Scheduler sets yield flag when task runs too long

**Option B: Counter-based**
- Decrement counter at each function call
- Yield when counter hits zero, reset counter

### 3.2 Allocation Yield Points

Yield on memory allocation (natural safe point).

```
// In malloc wrapper
fn orion_alloc(size: u64) ptr {
    @yield_check()  // might yield to scheduler
    return @malloc(size)
}
```

### 3.3 Channel Operation Yield Points

Already covered - channel ops naturally yield when blocking.

---

## Phase 4: Multi-threaded Runtime (Optional)

### 4.1 Thread Pool

Create worker threads that pull from shared task queue.

```
type Scheduler = {
    global_queue: TaskQueue,
    workers: []*Worker,
    num_workers: u64,
}

type Worker = {
    local_queue: TaskQueue,  // Work stealing
    thread: ptr,
    current_task: ptr,
}
```

### 4.2 Work Stealing

When a worker's local queue is empty:
1. Try to steal from global queue
2. Try to steal from other workers' local queues

### 4.3 Synchronization

- Lock-free global queue (or fine-grained locking)
- Per-worker local queues (no locking needed)
- Atomic operations for task state transitions

---

## Phase 5: Integration and Polish

### 5.1 Update Type System

- `Chan[T]` as first-class type with linearity rules
- Channel must be closed or passed (no leaking)
- Select arms type-checked for channel compatibility

### 5.2 Error Handling

- Panic propagation across tasks
- Channel close detection (`value, ok := <-ch` pattern)
- Deadlock detection (optional, runtime check)

### 5.3 Debugging Support

- Task naming for debugging
- Stack traces across async boundaries
- Channel buffer inspection

---

## Implementation Order

| Step | Description | Complexity | Dependencies |
|------|-------------|------------|--------------|
| 1.1 | Task queue data structure | Low | None |
| 1.2 | Scheduler spawn integration | Medium | 1.1 |
| 1.3 | Scheduler-aware @yield | Medium | 1.1, 1.2 |
| 1.4 | Async block scheduler run | Low | 1.1-1.3 |
| 2.1 | Channel type definition | Medium | None |
| 2.2 | Basic channel ops (send/recv) | High | 2.1, 1.x |
| 2.3 | Channel codegen | High | 2.2 |
| 2.4 | Full select implementation | High | 2.3 |
| 3.1 | Function call yield points | Medium | 1.x |
| 3.2 | Allocation yield points | Low | 1.x |
| 4.x | Multi-threaded runtime | Very High | 1.x, 2.x |
| 5.x | Polish and integration | Medium | All above |

---

## Risks and Mitigations

### Risk: Complexity explosion
**Mitigation:** Implement in phases, keep single-threaded scheduler working first

### Risk: Performance regression
**Mitigation:** Benchmark at each phase, optimize hot paths (scheduler loop, channel ops)

### Risk: Breaking existing code
**Mitigation:** Keep @join working, add scheduler as enhancement not replacement

### Risk: Deadlocks with channels
**Mitigation:** Consider deadlock detection in debug builds, clear documentation

---

## Success Criteria

1. **Phase 1 complete:** Multiple spawned tasks interleave via scheduler
2. **Phase 2 complete:** Channel send/recv works, select dispatches correctly
3. **Phase 3 complete:** Long-running tasks yield automatically
4. **Phase 4 complete:** Tasks run on multiple OS threads
5. **Overall:** Can port simple Go concurrent programs to Orion
