# Orion Concurrency Model: Structured Concurrency + Linear Channels

## Overview

Orion's concurrency model combines **structured concurrency** (tasks cannot outlive their spawning scope) with **linear channels** (ownership transfer on send). This design leverages Orion's linear type system to prevent common concurrency bugs at compile time.

## Core Principles

1. **No leaked tasks** - Every spawned task must complete before its scope exits
2. **Ownership transfer** - Sending on a channel transfers ownership; no shared mutable state
3. **No function coloring** - Avoid async/sync split where possible
4. **Explicit over implicit** - Concurrency boundaries are visible in the code

---

## Primitives

### 1. Task Scopes (`async` blocks)

A scope that can spawn concurrent tasks. All spawned tasks must complete before the scope exits.

```orion
let result = async {
    let task1 = spawn { compute_a() }
    let task2 = spawn { compute_b() }

    // Both tasks run concurrently
    let a = task1.join()
    let b = task2.join()

    return a + b
}
// Here: task1 and task2 are guaranteed complete
```

**Semantics:**
- `spawn` creates a `Task[T]` handle
- `Task.join()` blocks until completion, returns `Result[T, TaskError]`
- Exiting scope without joining is a **compile error** (linear Task handle must be consumed)
- If a task panics, other tasks in scope are cancelled

### 2. Channels (`Chan[T]`)

Channels are the primary communication mechanism between tasks. They enforce **ownership transfer** - sending a value moves it.

```orion
type Chan[T] = struct {
    // internal buffer, synchronization
}

instance Chan[T] {
    // Create buffered channel
    new: fn(capacity: i64) Chan[T]

    // Create unbuffered (rendezvous) channel
    rendezvous: fn() Chan[T]

    // Send transfers ownership of value (consumes `value`)
    send: fn(self: &Chan[T], value: T) Result[(), ChanError]

    // Receive takes ownership
    recv: fn(self: &Chan[T]) Result[T, ChanError]

    // Close the channel
    close: fn(self: Chan[T]) ()
}
```

**Linear Channel Semantics:**

```orion
let ch: Chan[Buffer] = Chan.new(1)

async {
    let task = spawn {
        let buf = Buffer.new(1024)  // buf owned here
        ch.send(buf)                // ownership transferred INTO channel
        // buf is NO LONGER valid here - compile error if used
    }

    let received = ch.recv()  // ownership transferred OUT of channel
    // received is now owned by this scope

    task.join()
}
```

### 3. Select (Multi-channel wait)

Wait on multiple channel operations:

```orion
let result = select {
    recv(ch1) -> val { handle_ch1(val) }
    recv(ch2) -> val { handle_ch2(val) }
    send(ch3, data) -> () { handle_sent() }
    default { handle_none_ready() }  // optional
}
```

---

## Structured Concurrency Rules

### Rule 1: Tasks Cannot Escape Their Scope

```orion
// INVALID - compile error
fn bad() Task[i64] {
    async {
        return spawn { 42 }  // ERROR: Task cannot escape async scope
    }
}

// VALID
fn good() i64 {
    async {
        let t = spawn { 42 }
        return t.join()  // Task consumed within scope
    }
}
```

### Rule 2: All Tasks Must Be Joined

```orion
// INVALID - compile error
async {
    let t = spawn { work() }
    // ERROR: linear Task[T] not consumed
}

// VALID
async {
    let t = spawn { work() }
    let _ = t.join()  // explicitly consume
}
```

### Rule 3: Nested Scopes

```orion
async {
    let outer_task = spawn {
        async {
            let inner = spawn { compute() }
            inner.join()
        }
        // inner scope complete, inner task done
    }
    outer_task.join()
}
// all tasks complete
```

---

## Cancellation

Tasks can be cancelled via their handle. Cancellation is **cooperative** - tasks check for cancellation at yield points.

```orion
async {
    let task = spawn {
        while work_remaining() {
            do_chunk()
            // implicit cancellation check at loop boundaries
        }
    }

    // Cancel after timeout
    if !task.join_timeout(Duration.seconds(5)) {
        task.cancel()  // request cancellation
        task.join()    // still must join to consume handle
    }
}
```

---

## Error Handling

Errors in concurrent code use `Result[T, E]`:

```orion
type TaskError =
    | Cancelled
    | Panicked(str)
    | JoinError

type ChanError =
    | Closed
    | Full      // for try_send
    | Empty     // for try_recv

async {
    let task = spawn {
        may_fail()?  // propagate errors
    }

    match task.join() {
        Ok(value) -> use(value),
        Err(TaskError.Panicked(msg)) -> log_error(msg),
        Err(TaskError.Cancelled) -> log("cancelled"),
    }
}
```

---

## Common Patterns

### Fan-out / Fan-in

```orion
fn parallel_map[T, U](items: Vec[T], f: fn(T) U) Vec[U] {
    let results: Chan[U] = Chan.new(items.len())

    async {
        // Fan-out: spawn task per item
        let tasks = items.iter().map(fn(item) {
            spawn {
                results.send(f(item))
            }
        }).collect()

        // Join all tasks
        for task in tasks {
            task.join()
        }

        results.close()
    }

    // Fan-in: collect results
    results.drain().collect()
}
```

### Worker Pool

```orion
fn worker_pool[T, U](
    work: Chan[T],
    results: Chan[U],
    f: fn(T) U,
    num_workers: i64
) {
    async {
        let workers = (0..num_workers).map(fn(_) {
            spawn {
                for item in work.iter() {
                    results.send(f(item))
                }
            }
        }).collect()

        for w in workers {
            w.join()
        }
    }
}
```

### Timeout Pattern

```orion
fn with_timeout[T](duration: Duration, f: fn() T) Result[T, TimeoutError] {
    let result_ch: Chan[T] = Chan.rendezvous()
    let timeout_ch: Chan[()] = Chan.rendezvous()

    async {
        let work = spawn { result_ch.send(f()) }
        let timer = spawn {
            sleep(duration)
            timeout_ch.send(())
        }

        let result = select {
            recv(result_ch) -> val { Ok(val) }
            recv(timeout_ch) -> _ { Err(TimeoutError) }
        }

        work.cancel()
        timer.cancel()
        work.join()
        timer.join()

        result
    }
}
```

---

## Implementation Notes

### Runtime Requirements

1. **Task scheduler** - Cooperative M:N scheduler (M tasks on N OS threads)
2. **Work stealing** - For load balancing across threads
3. **Channel implementation** - Lock-free MPMC queue for buffered channels

### LLVM Considerations

- Tasks as stackful coroutines (simpler) or stackless (more efficient)
- `llvm.coro.*` intrinsics available for coroutine transforms
- Alternative: lightweight threads via `makecontext`/`swapcontext` on Unix

### Linear Type Integration

The compiler must track:
- `Task[T]` handles are linear (must be joined exactly once)
- `Chan[T]` send consumes the value
- Scope exit requires all tasks joined

---

## Comparison with Other Models

| Feature | Orion | Go | Rust | Erlang |
|---------|-------|-----|------|--------|
| Task lifetime | Scoped | Unscoped | Scoped (spawn) | Unscoped |
| Communication | Linear channels | Channels | Channels, shared state | Messages |
| Cancellation | Cooperative | Context | Cooperative | Kill signals |
| Memory safety | Linear types | GC | Borrow checker | GC + isolation |
| Function coloring | No | No | Yes (async) | No |

---

## Open Questions

1. **Thread pinning** - Should tasks be able to pin to specific OS threads for FFI?
2. **Blocking FFI** - How to handle blocking C calls without stalling the scheduler?
3. **Priority** - Task priorities? Probably not initially.
4. **Fairness** - Strict fairness guarantees or best-effort?

---

## Stdlib Modules

```
std/
  async/
    task.or      -- Task[T], spawn, join
    chan.or      -- Chan[T], send, recv, select
    scope.or     -- async blocks, structured concurrency
    time.or      -- sleep, timeout, Duration
    sync.or      -- Mutex, WaitGroup (escape hatches)
```
