const std = @import("std");
const builtin = @import("builtin");

pub const TargetTriple = struct {
    arch: []const u8,
    vendor: []const u8,
    os: []const u8,

    pub fn toLLVMTriple(self: TargetTriple, allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{s}-{s}-{s}", .{ self.arch, self.vendor, self.os });
    }

    pub fn getTargetInfo(self: TargetTriple) TargetInfo {
        const pointer_width: u32 = if (std.mem.indexOf(u8, self.arch, "64") != null)
            64
        else if (std.mem.indexOf(u8, self.arch, "32") != null or
            std.mem.eql(u8, self.arch, "arm") or
            std.mem.eql(u8, self.arch, "i386") or
            std.mem.eql(u8, self.arch, "i686"))
            32
        else
            64; // Default to 64-bit

        return TargetInfo{
            .pointer_width = pointer_width,
            .native_int_type = if (pointer_width == 64) "i64" else "i32",
            .is_windows = std.mem.indexOf(u8, self.os, "windows") != null,
            .is_macos = std.mem.indexOf(u8, self.os, "darwin") != null,
            .is_linux = std.mem.indexOf(u8, self.os, "linux") != null,
        };
    }
};

pub const TargetInfo = struct {
    pointer_width: u32,
    native_int_type: []const u8,
    is_windows: bool,
    is_macos: bool,
    is_linux: bool,
};

pub fn detectHostTriple() TargetTriple {
    const arch = switch (builtin.cpu.arch) {
        .x86_64 => "x86_64",
        .aarch64 => "aarch64",
        .arm => "arm",
        .riscv64 => "riscv64",
        else => "unknown",
    };

    return TargetTriple{
        .arch = arch,
        .vendor = switch (builtin.os.tag) {
            .macos => "apple",
            .linux => "pc",
            .windows => "pc",
            else => "unknown",
        },
        .os = switch (builtin.os.tag) {
            .linux => "linux-gnu",
            .macos => "darwin",
            .windows => "windows-msvc",
            else => "unknown",
        },
    };
}

pub fn parseTargetTriple(allocator: std.mem.Allocator, triple_str: []const u8) !TargetTriple {
    var parts = std.mem.splitSequence(u8, triple_str, "-");

    const arch = parts.next() orelse return error.InvalidTargetTriple;
    const vendor = parts.next() orelse return error.InvalidTargetTriple;

    // OS might have multiple parts (e.g., "linux-gnu" or "windows-msvc")
    const os_start = parts.rest();
    if (os_start.len == 0) return error.InvalidTargetTriple;

    return TargetTriple{
        .arch = try allocator.dupe(u8, arch),
        .vendor = try allocator.dupe(u8, vendor),
        .os = try allocator.dupe(u8, os_start),
    };
}
