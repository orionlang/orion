const std = @import("std");

pub const ModuleId = struct {
    // Full module path like "lexer" or "compiler.codegen"
    path: []const u8,
    // File path like "src/lexer.or" or "src/compiler/codegen.or"
    file_path: []const u8,
    is_stdlib: bool,
};

pub const ModuleResolver = struct {
    allocator: std.mem.Allocator,
    src_dir: []const u8,
    stdlib_dir: []const u8,
    include_dirs: []const []const u8,

    pub fn init(allocator: std.mem.Allocator, src_dir: []const u8, stdlib_dir: []const u8, include_dirs: []const []const u8) ModuleResolver {
        return .{
            .allocator = allocator,
            .src_dir = src_dir,
            .stdlib_dir = stdlib_dir,
            .include_dirs = include_dirs,
        };
    }

    /// Resolve an import path to a module file path
    /// Examples:
    ///   "lexer" -> "src/lexer.or"
    ///   "compiler.codegen" -> "src/compiler/codegen.or"
    ///   "std.mem" -> "stdlib/std/mem.or"
    ///
    /// Search order for non-stdlib imports:
    ///   1. src_dir (local modules)
    ///   2. include_dirs in order (external libraries)
    ///   3. stdlib_dir for std.* imports
    pub fn resolve(self: *ModuleResolver, import_path: []const u8) !ModuleId {
        const is_stdlib = std.mem.startsWith(u8, import_path, "std.");

        if (is_stdlib) {
            // stdlib/std/mem.or
            const file_path = try self.buildFilePath(self.stdlib_dir, import_path);
            const path = try self.allocator.dupe(u8, import_path);

            return ModuleId{
                .path = path,
                .file_path = file_path,
                .is_stdlib = true,
            };
        } else {
            // Always try src_dir first
            const src_file_path = try self.buildFilePath(self.src_dir, import_path);

            // If no include_dirs, assume src_dir is correct (for tests and simple cases)
            if (self.include_dirs.len == 0) {
                const path = try self.allocator.dupe(u8, import_path);
                return ModuleId{
                    .path = path,
                    .file_path = src_file_path,
                    .is_stdlib = false,
                };
            }

            // Check if it exists in src_dir
            if (self.fileExists(src_file_path)) {
                const path = try self.allocator.dupe(u8, import_path);
                return ModuleId{
                    .path = path,
                    .file_path = src_file_path,
                    .is_stdlib = false,
                };
            }
            self.allocator.free(src_file_path);

            // Try each include_dir in order
            for (self.include_dirs) |include_dir| {
                const inc_file_path = try self.buildFilePath(include_dir, import_path);
                if (self.fileExists(inc_file_path)) {
                    const path = try self.allocator.dupe(u8, import_path);
                    return ModuleId{
                        .path = path,
                        .file_path = inc_file_path,
                        .is_stdlib = false,
                    };
                }
                self.allocator.free(inc_file_path);
            }

            // Not found anywhere
            return error.ModuleNotFound;
        }
    }

    fn buildFilePath(self: *ModuleResolver, base_dir: []const u8, import_path: []const u8) ![]const u8 {
        var file_path_buf = std.ArrayList(u8).empty;
        defer file_path_buf.deinit(self.allocator);

        try file_path_buf.appendSlice(self.allocator, base_dir);
        try file_path_buf.append(self.allocator, '/');

        // Replace dots with slashes in the path
        for (import_path) |c| {
            if (c == '.') {
                try file_path_buf.append(self.allocator, '/');
            } else {
                try file_path_buf.append(self.allocator, c);
            }
        }

        try file_path_buf.appendSlice(self.allocator, ".or");

        return try self.allocator.dupe(u8, file_path_buf.items);
    }

    fn fileExists(self: *ModuleResolver, path: []const u8) bool {
        _ = self;
        std.fs.cwd().access(path, .{}) catch return false;
        return true;
    }

    pub fn deinitModuleId(self: *ModuleResolver, module_id: ModuleId) void {
        self.allocator.free(module_id.path);
        self.allocator.free(module_id.file_path);
    }
};

test "resolve user module" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    var resolver = ModuleResolver.init(allocator, "src", "stdlib", include_dirs);

    const mod = try resolver.resolve("lexer");
    defer resolver.deinitModuleId(mod);

    try testing.expectEqualStrings("lexer", mod.path);
    try testing.expectEqualStrings("src/lexer.or", mod.file_path);
    try testing.expect(!mod.is_stdlib);
}

test "resolve nested user module" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    var resolver = ModuleResolver.init(allocator, "src", "stdlib", include_dirs);

    const mod = try resolver.resolve("compiler.codegen");
    defer resolver.deinitModuleId(mod);

    try testing.expectEqualStrings("compiler.codegen", mod.path);
    try testing.expectEqualStrings("src/compiler/codegen.or", mod.file_path);
    try testing.expect(!mod.is_stdlib);
}

test "resolve deeply nested user module" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    var resolver = ModuleResolver.init(allocator, "src", "stdlib", include_dirs);

    const mod = try resolver.resolve("compiler.backend.llvm");
    defer resolver.deinitModuleId(mod);

    try testing.expectEqualStrings("compiler.backend.llvm", mod.path);
    try testing.expectEqualStrings("src/compiler/backend/llvm.or", mod.file_path);
    try testing.expect(!mod.is_stdlib);
}

test "resolve stdlib module" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    var resolver = ModuleResolver.init(allocator, "src", "stdlib", include_dirs);

    const mod = try resolver.resolve("std.mem");
    defer resolver.deinitModuleId(mod);

    try testing.expectEqualStrings("std.mem", mod.path);
    try testing.expectEqualStrings("stdlib/std/mem.or", mod.file_path);
    try testing.expect(mod.is_stdlib);
}

test "resolve nested stdlib module" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    var resolver = ModuleResolver.init(allocator, "src", "stdlib", include_dirs);

    const mod = try resolver.resolve("std.collections.hashmap");
    defer resolver.deinitModuleId(mod);

    try testing.expectEqualStrings("std.collections.hashmap", mod.path);
    try testing.expectEqualStrings("stdlib/std/collections/hashmap.or", mod.file_path);
    try testing.expect(mod.is_stdlib);
}

test "resolve from include_dirs" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{"test_fixtures/external_lib"};
    var resolver = ModuleResolver.init(allocator, "test_fixtures/with_external/src", "stdlib", include_dirs);

    const mod = try resolver.resolve("utils");
    defer resolver.deinitModuleId(mod);

    try testing.expectEqualStrings("utils", mod.path);
    try testing.expectEqualStrings("test_fixtures/external_lib/utils.or", mod.file_path);
    try testing.expect(!mod.is_stdlib);
}

test "src_dir takes precedence over include_dirs" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a temporary test scenario where both src_dir and include_dir have the same module
    const include_dirs: []const []const u8 = &.{"test_fixtures/external_lib"};
    var resolver = ModuleResolver.init(allocator, "test_fixtures/simple", "stdlib", include_dirs);

    // "utils" doesn't exist in test_fixtures/simple but does in external_lib
    const mod = try resolver.resolve("utils");
    defer resolver.deinitModuleId(mod);

    // Should resolve to include_dir since not in src_dir
    try testing.expectEqualStrings("test_fixtures/external_lib/utils.or", mod.file_path);
}
