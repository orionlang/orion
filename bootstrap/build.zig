const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Orion compiler module
    const mod = b.addModule("orion", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
    });

    // Orion compiler executable
    const exe = b.addExecutable(.{
        .name = "orion",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "orion", .module = mod },
            },
        }),
    });
    b.installArtifact(exe);

    // Runtime library - static (liborion_runtime.a)
    const runtime_static = b.addLibrary(.{
        .name = "orion_runtime",
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path("../runtime/src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    b.installArtifact(runtime_static);

    // Runtime library - shared (liborion_runtime.so / .dylib)
    const runtime_shared = b.addLibrary(.{
        .name = "orion_runtime",
        .linkage = .dynamic,
        .root_module = b.createModule(.{
            .root_source_file = b.path("../runtime/src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    b.installArtifact(runtime_shared);

    // Run step
    const run_step = b.step("run", "Run the compiler");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // Tests - compiler
    const mod_tests = b.addTest(.{
        .root_module = mod,
    });
    const run_mod_tests = b.addRunArtifact(mod_tests);

    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });
    const run_exe_tests = b.addRunArtifact(exe_tests);

    // Tests - runtime
    const runtime_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("../runtime/src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    const run_runtime_tests = b.addRunArtifact(runtime_tests);

    // Test step runs all tests
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_mod_tests.step);
    test_step.dependOn(&run_exe_tests.step);
    test_step.dependOn(&run_runtime_tests.step);
}
