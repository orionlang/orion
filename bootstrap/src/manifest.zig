const std = @import("std");

pub const PackageInfo = struct {
    name: []const u8,
    version: []const u8,
};

pub const BinConfig = struct {
    entrypoint: []const u8,
};

pub const LibConfig = struct {
    entrypoint: []const u8,
};

pub const Dependency = struct {
    name: []const u8,
    path: []const u8,
};

pub const Manifest = struct {
    package: PackageInfo,
    bin: ?BinConfig,
    lib: ?LibConfig,
    dependencies: []Dependency,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Manifest) void {
        self.allocator.free(self.package.name);
        self.allocator.free(self.package.version);
        if (self.bin) |bin| {
            self.allocator.free(bin.entrypoint);
        }
        if (self.lib) |lib| {
            self.allocator.free(lib.entrypoint);
        }
        for (self.dependencies) |dep| {
            self.allocator.free(dep.name);
            self.allocator.free(dep.path);
        }
        self.allocator.free(self.dependencies);
    }
};

pub fn parseManifest(allocator: std.mem.Allocator, content: []const u8) !Manifest {
    // Simple TOML parser for our subset
    var package_name: ?[]const u8 = null;
    var package_version: ?[]const u8 = null;
    var bin_entrypoint: ?[]const u8 = null;
    var lib_entrypoint: ?[]const u8 = null;
    var dependencies: std.ArrayList(Dependency) = .empty;
    errdefer dependencies.deinit(allocator);

    var current_section: enum { none, package, bin, lib, dependencies } = .none;
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Skip empty lines and comments
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Check for section headers
        if (std.mem.startsWith(u8, trimmed, "[")) {
            if (std.mem.eql(u8, trimmed, "[package]")) {
                current_section = .package;
            } else if (std.mem.eql(u8, trimmed, "[bin]")) {
                current_section = .bin;
            } else if (std.mem.eql(u8, trimmed, "[lib]")) {
                current_section = .lib;
            } else if (std.mem.eql(u8, trimmed, "[dependencies]")) {
                current_section = .dependencies;
            }
            continue;
        }

        // Parse key = value pairs
        if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
            const key = std.mem.trim(u8, trimmed[0..eq_pos], " \t");
            var value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");

            // Remove quotes from value
            if (value.len >= 2 and value[0] == '"' and value[value.len - 1] == '"') {
                value = value[1 .. value.len - 1];
            }

            switch (current_section) {
                .package => {
                    if (std.mem.eql(u8, key, "name")) {
                        package_name = try allocator.dupe(u8, value);
                    } else if (std.mem.eql(u8, key, "version")) {
                        package_version = try allocator.dupe(u8, value);
                    }
                },
                .bin => {
                    if (std.mem.eql(u8, key, "entrypoint")) {
                        bin_entrypoint = try allocator.dupe(u8, value);
                    }
                },
                .lib => {
                    if (std.mem.eql(u8, key, "entrypoint")) {
                        lib_entrypoint = try allocator.dupe(u8, value);
                    }
                },
                .dependencies => {
                    // Parse: mylib = { path = "../mylib" }
                    if (std.mem.indexOf(u8, value, "path")) |path_start| {
                        if (std.mem.indexOf(u8, value[path_start..], "=")) |eq| {
                            const path_value_start = path_start + eq + 1;
                            var path_value = std.mem.trim(u8, value[path_value_start..], " \t}");

                            // Remove quotes
                            if (path_value.len >= 2 and path_value[0] == '"' and path_value[path_value.len - 1] == '"') {
                                path_value = path_value[1 .. path_value.len - 1];
                            }

                            try dependencies.append(allocator, .{
                                .name = try allocator.dupe(u8, key),
                                .path = try allocator.dupe(u8, path_value),
                            });
                        }
                    }
                },
                .none => {},
            }
        }
    }

    // Validate required fields
    if (package_name == null) {
        if (package_version) |v| allocator.free(v);
        if (bin_entrypoint) |b| allocator.free(b);
        if (lib_entrypoint) |l| allocator.free(l);
        return error.MissingPackageName;
    }
    if (package_version == null) {
        if (package_name) |n| allocator.free(n);
        if (bin_entrypoint) |b| allocator.free(b);
        if (lib_entrypoint) |l| allocator.free(l);
        return error.MissingPackageVersion;
    }

    return Manifest{
        .package = .{
            .name = package_name.?,
            .version = package_version.?,
        },
        .bin = if (bin_entrypoint) |ep| BinConfig{ .entrypoint = ep } else null,
        .lib = if (lib_entrypoint) |ep| LibConfig{ .entrypoint = ep } else null,
        .dependencies = try dependencies.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

test "parse binary project manifest" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "mycompiler"
        \\version = "0.1.0"
        \\
        \\[bin]
        \\entrypoint = "src/main.or"
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqualStrings("mycompiler", parsed.package.name);
    try testing.expectEqualStrings("0.1.0", parsed.package.version);
    try testing.expect(parsed.bin != null);
    try testing.expectEqualStrings("src/main.or", parsed.bin.?.entrypoint);
    try testing.expect(parsed.lib == null);
    try testing.expectEqual(@as(usize, 0), parsed.dependencies.len);
}

test "parse library project manifest" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "mylib"
        \\version = "0.1.0"
        \\
        \\[lib]
        \\entrypoint = "src/lib.or"
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqualStrings("mylib", parsed.package.name);
    try testing.expectEqualStrings("0.1.0", parsed.package.version);
    try testing.expect(parsed.bin == null);
    try testing.expect(parsed.lib != null);
    try testing.expectEqualStrings("src/lib.or", parsed.lib.?.entrypoint);
    try testing.expectEqual(@as(usize, 0), parsed.dependencies.len);
}

test "parse binary and library project manifest" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "mycompiler"
        \\version = "0.1.0"
        \\
        \\[bin]
        \\entrypoint = "src/main.or"
        \\
        \\[lib]
        \\entrypoint = "src/lib.or"
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqualStrings("mycompiler", parsed.package.name);
    try testing.expectEqualStrings("0.1.0", parsed.package.version);
    try testing.expect(parsed.bin != null);
    try testing.expectEqualStrings("src/main.or", parsed.bin.?.entrypoint);
    try testing.expect(parsed.lib != null);
    try testing.expectEqualStrings("src/lib.or", parsed.lib.?.entrypoint);
    try testing.expectEqual(@as(usize, 0), parsed.dependencies.len);
}

test "parse manifest with single dependency" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "mycompiler"
        \\version = "0.1.0"
        \\
        \\[bin]
        \\entrypoint = "src/main.or"
        \\
        \\[dependencies]
        \\mylib = { path = "../mylib" }
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqualStrings("mycompiler", parsed.package.name);
    try testing.expectEqual(@as(usize, 1), parsed.dependencies.len);
    try testing.expectEqualStrings("mylib", parsed.dependencies[0].name);
    try testing.expectEqualStrings("../mylib", parsed.dependencies[0].path);
}

test "parse manifest with multiple dependencies" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "mycompiler"
        \\version = "0.1.0"
        \\
        \\[bin]
        \\entrypoint = "src/main.or"
        \\
        \\[dependencies]
        \\mylib = { path = "../mylib" }
        \\another = { path = "/absolute/path/lib" }
        \\relative = { path = "./local/lib" }
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqual(@as(usize, 3), parsed.dependencies.len);
    try testing.expectEqualStrings("mylib", parsed.dependencies[0].name);
    try testing.expectEqualStrings("../mylib", parsed.dependencies[0].path);
    try testing.expectEqualStrings("another", parsed.dependencies[1].name);
    try testing.expectEqualStrings("/absolute/path/lib", parsed.dependencies[1].path);
    try testing.expectEqualStrings("relative", parsed.dependencies[2].name);
    try testing.expectEqualStrings("./local/lib", parsed.dependencies[2].path);
}

test "parse minimal manifest" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "minimal"
        \\version = "1.0.0"
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqualStrings("minimal", parsed.package.name);
    try testing.expectEqualStrings("1.0.0", parsed.package.version);
    try testing.expect(parsed.bin == null);
    try testing.expect(parsed.lib == null);
    try testing.expectEqual(@as(usize, 0), parsed.dependencies.len);
}

test "parse manifest with comments and whitespace" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\# This is a comment
        \\[package]
        \\name = "myproject"
        \\version = "0.1.0"
        \\
        \\# Binary configuration
        \\[bin]
        \\entrypoint = "src/main.or"
        \\
        \\
        \\# Dependencies
        \\[dependencies]
        \\# External library
        \\mylib = { path = "../mylib" }
    ;

    var parsed = try parseManifest(allocator, test_toml);
    defer parsed.deinit();

    try testing.expectEqualStrings("myproject", parsed.package.name);
    try testing.expectEqualStrings("0.1.0", parsed.package.version);
    try testing.expect(parsed.bin != null);
    try testing.expectEqualStrings("src/main.or", parsed.bin.?.entrypoint);
    try testing.expectEqual(@as(usize, 1), parsed.dependencies.len);
}

test "missing package name returns error" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\version = "0.1.0"
    ;

    const result = parseManifest(allocator, test_toml);
    try testing.expectError(error.MissingPackageName, result);
}

test "missing package version returns error" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const test_toml =
        \\[package]
        \\name = "myproject"
    ;

    const result = parseManifest(allocator, test_toml);
    try testing.expectError(error.MissingPackageVersion, result);
}
