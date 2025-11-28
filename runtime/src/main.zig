// Orion Runtime Library
// Go-style work-stealing scheduler with M:N threading

const std = @import("std");
const builtin = @import("builtin");
const Atomic = std.atomic.Value;

// Re-export scheduler API
pub const scheduler = @import("scheduler.zig");

// C ABI exports for linking with compiled Orion programs
pub const orion_scheduler_init = scheduler.init;
pub const orion_scheduler_spawn = scheduler.spawn;
pub const orion_scheduler_yield = scheduler.yield;
pub const orion_scheduler_run = scheduler.run;
pub const orion_scheduler_run_until = scheduler.run_until;
pub const orion_scheduler_shutdown = scheduler.shutdown;

// Export with C calling convention for LLVM IR
comptime {
    @export(&scheduler.init, .{ .name = "orion_scheduler_init", .linkage = .strong });
    @export(&scheduler.spawn, .{ .name = "orion_scheduler_spawn", .linkage = .strong });
    @export(&scheduler.yield, .{ .name = "orion_scheduler_yield", .linkage = .strong });
    @export(&scheduler.run, .{ .name = "orion_scheduler_run", .linkage = .strong });
    @export(&scheduler.run_until, .{ .name = "orion_scheduler_run_until", .linkage = .strong });
    @export(&scheduler.shutdown, .{ .name = "orion_scheduler_shutdown", .linkage = .strong });
}

test "runtime basics" {
    scheduler.init();
    defer scheduler.shutdown();
}
