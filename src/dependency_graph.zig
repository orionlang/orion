const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ImportDecl = @import("parser.zig").ImportDecl;
const ModuleResolver = @import("module.zig").ModuleResolver;
const ModuleId = @import("module.zig").ModuleId;

pub const DependencyGraph = struct {
    allocator: std.mem.Allocator,
    resolver: ModuleResolver,
    // Map from module path to list of dependencies
    edges: std.StringHashMap(std.ArrayList([]const u8)),
    // Set of all discovered modules
    modules: std.StringHashMap(ModuleId),

    pub fn init(allocator: std.mem.Allocator, resolver: ModuleResolver) DependencyGraph {
        return .{
            .allocator = allocator,
            .resolver = resolver,
            .edges = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
            .modules = std.StringHashMap(ModuleId).init(allocator),
        };
    }

    pub fn deinit(self: *DependencyGraph) void {
        var edge_iter = self.edges.iterator();
        while (edge_iter.next()) |entry| {
            for (entry.value_ptr.items) |dep| {
                self.allocator.free(dep);
            }
            entry.value_ptr.deinit(self.allocator);
        }
        self.edges.deinit();

        var module_iter = self.modules.iterator();
        while (module_iter.next()) |entry| {
            self.resolver.deinitModuleId(entry.value_ptr.*);
        }
        self.modules.deinit();
    }

    /// Discover all modules starting from an entrypoint file
    pub fn discover(self: *DependencyGraph, entrypoint_path: []const u8) !void {
        // Read and parse the entrypoint to get its module path
        const source = try std.fs.cwd().readFileAlloc(self.allocator, entrypoint_path, 1024 * 1024);
        defer self.allocator.free(source);

        // Derive module name from file path
        // e.g., "test_multi/src/main.or" with resolver.src_dir="test_multi/src" -> "main"
        //       "test_multi/src/foo/bar.or" -> "foo.bar"
        const module_name = blk: {
            // Get basename without extension
            const basename = std.fs.path.basename(entrypoint_path);
            const name_no_ext = if (std.mem.endsWith(u8, basename, ".or"))
                basename[0 .. basename.len - 3]
            else
                basename;
            break :blk name_no_ext;
        };

        try self.discoverModule(module_name, entrypoint_path);
    }

    pub fn discoverModule(self: *DependencyGraph, module_path: []const u8, file_path: []const u8) !void {
        // Skip if already discovered
        if (self.modules.contains(module_path)) return;

        // Read and parse the module
        const source = try std.fs.cwd().readFileAlloc(self.allocator, file_path, 1024 * 1024);
        defer self.allocator.free(source);

        var lexer = Lexer.init(source);
        var tokens = try lexer.tokenize(self.allocator);
        defer tokens.deinit(self.allocator);

        var parser = Parser.init(tokens.items, self.allocator);
        var ast = try parser.parse();
        defer ast.deinit(self.allocator);

        // Register this module
        const mod_id = ModuleId{
            .path = try self.allocator.dupe(u8, module_path),
            .file_path = try self.allocator.dupe(u8, file_path),
            .is_stdlib = std.mem.startsWith(u8, module_path, "std."),
        };
        try self.modules.put(mod_id.path, mod_id);

        // Extract dependencies from imports
        var dependencies: std.ArrayList([]const u8) = .empty;
        errdefer dependencies.deinit(self.allocator);

        for (ast.imports.items) |import_decl| {
            const import_path = switch (import_decl.item) {
                .module => |path| path,
                .specific => |spec| spec.path,
            };

            // Resolve the import to a module
            const dep_module = try self.resolver.resolve(import_path);
            defer self.resolver.deinitModuleId(dep_module);

            // Add to dependencies
            const dep_path = try self.allocator.dupe(u8, dep_module.path);
            try dependencies.append(self.allocator, dep_path);

            // Recursively discover the dependency
            try self.discoverModule(dep_module.path, dep_module.file_path);
        }

        try self.edges.put(mod_id.path, dependencies);
    }

    /// Detect if there are any circular dependencies
    pub fn detectCycles(self: *DependencyGraph) !?[]const []const u8 {
        var visited = std.StringHashMap(void).init(self.allocator);
        defer visited.deinit();

        var rec_stack = std.StringHashMap(void).init(self.allocator);
        defer rec_stack.deinit();

        var path: std.ArrayList([]const u8) = .empty;
        defer path.deinit(self.allocator);

        var module_iter = self.modules.keyIterator();
        while (module_iter.next()) |module_path| {
            if (try self.hasCycleDFS(module_path.*, &visited, &rec_stack, &path)) {
                // Found a cycle, return the path
                return try path.toOwnedSlice(self.allocator);
            }
        }

        return null;
    }

    fn hasCycleDFS(
        self: *DependencyGraph,
        module: []const u8,
        visited: *std.StringHashMap(void),
        rec_stack: *std.StringHashMap(void),
        path: *std.ArrayList([]const u8),
    ) !bool {
        if (rec_stack.contains(module)) {
            // Found a cycle
            try path.append(self.allocator, module);
            return true;
        }

        if (visited.contains(module)) {
            return false;
        }

        try visited.put(module, {});
        try rec_stack.put(module, {});
        try path.append(self.allocator, module);

        if (self.edges.get(module)) |deps| {
            for (deps.items) |dep| {
                if (try self.hasCycleDFS(dep, visited, rec_stack, path)) {
                    return true;
                }
            }
        }

        _ = rec_stack.remove(module);
        _ = path.pop();

        return false;
    }

    /// Perform topological sort to get compilation order
    /// Returns array of arrays, where each inner array contains modules that can be compiled in parallel
    pub fn topologicalSort(self: *DependencyGraph) ![]std.ArrayList([]const u8) {
        // Calculate in-degrees for all modules
        // If module A depends on module B (A imports B), then A has in-degree+1
        // Modules with in-degree 0 have no dependencies and can be compiled first
        var in_degree = std.StringHashMap(usize).init(self.allocator);
        defer in_degree.deinit();

        var module_iter = self.modules.keyIterator();
        while (module_iter.next()) |module| {
            try in_degree.put(module.*, 0);
        }

        // For each module, count how many things it depends on
        var edge_iter = self.edges.iterator();
        while (edge_iter.next()) |entry| {
            // entry.key_ptr is the module
            // entry.value_ptr.items are its dependencies
            const current = in_degree.get(entry.key_ptr.*) orelse 0;
            try in_degree.put(entry.key_ptr.*, current + entry.value_ptr.items.len);
        }

        // Find all modules with in-degree 0 (no dependencies)
        var queue = std.ArrayList([]const u8).empty;
        defer queue.deinit(self.allocator);

        var degree_iter = in_degree.iterator();
        while (degree_iter.next()) |entry| {
            if (entry.value_ptr.* == 0) {
                try queue.append(self.allocator, entry.key_ptr.*);
            }
        }

        var levels = std.ArrayList(std.ArrayList([]const u8)).empty;
        errdefer {
            for (levels.items) |*level| {
                level.deinit(self.allocator);
            }
            levels.deinit(self.allocator);
        }

        while (queue.items.len > 0) {
            // Current level: all modules with in-degree 0
            var current_level = std.ArrayList([]const u8).empty;
            errdefer current_level.deinit(self.allocator);

            // Add all current queue items to this level
            for (queue.items) |module| {
                try current_level.append(self.allocator, module);
            }

            try levels.append(self.allocator, current_level);

            // Process this level and update in-degrees
            var next_queue = std.ArrayList([]const u8).empty;

            for (queue.items) |module| {
                // For each module in current level, find modules that depend on it
                // and decrement their in-degree
                var edge_check = self.edges.iterator();
                while (edge_check.next()) |entry| {
                    // Check if entry.key_ptr depends on module
                    for (entry.value_ptr.items) |dep| {
                        if (std.mem.eql(u8, dep, module)) {
                            // entry.key_ptr depends on module, so decrement its degree
                            const current_degree = in_degree.get(entry.key_ptr.*).?;
                            const new_degree = current_degree - 1;
                            try in_degree.put(entry.key_ptr.*, new_degree);

                            if (new_degree == 0) {
                                // Check if already in next_queue to avoid duplicates
                                var already_added = false;
                                for (next_queue.items) |item| {
                                    if (std.mem.eql(u8, item, entry.key_ptr.*)) {
                                        already_added = true;
                                        break;
                                    }
                                }
                                if (!already_added) {
                                    try next_queue.append(self.allocator, entry.key_ptr.*);
                                }
                            }
                        }
                    }
                }
            }

            queue.deinit(self.allocator);
            queue = next_queue;
        }

        return try levels.toOwnedSlice(self.allocator);
    }
};

test "discover simple dependency graph" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(allocator, "test_fixtures/simple", "stdlib", include_dirs);
    var graph = DependencyGraph.init(allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/simple/main.or");

    // Should have discovered main and lexer
    try testing.expect(graph.modules.contains("main"));
    try testing.expect(graph.modules.contains("lexer"));

    // main depends on lexer
    const main_deps = graph.edges.get("main").?;
    try testing.expectEqual(@as(usize, 1), main_deps.items.len);
    try testing.expectEqualStrings("lexer", main_deps.items[0]);

    // lexer has no dependencies
    const lexer_deps = graph.edges.get("lexer").?;
    try testing.expectEqual(@as(usize, 0), lexer_deps.items.len);
}

test "discover nested dependency graph" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(allocator, "test_fixtures/nested", "stdlib", include_dirs);
    var graph = DependencyGraph.init(allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/nested/main.or");

    // Should have discovered main, lexer, and parser
    try testing.expect(graph.modules.contains("main"));
    try testing.expect(graph.modules.contains("lexer"));
    try testing.expect(graph.modules.contains("parser"));

    // main depends on lexer and parser
    const main_deps = graph.edges.get("main").?;
    try testing.expectEqual(@as(usize, 2), main_deps.items.len);

    // parser depends on lexer
    const parser_deps = graph.edges.get("parser").?;
    try testing.expectEqual(@as(usize, 1), parser_deps.items.len);
    try testing.expectEqualStrings("lexer", parser_deps.items[0]);

    // lexer has no dependencies
    const lexer_deps = graph.edges.get("lexer").?;
    try testing.expectEqual(@as(usize, 0), lexer_deps.items.len);
}

test "detect circular dependency" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(allocator, "test_fixtures/cycle", "stdlib", include_dirs);
    var graph = DependencyGraph.init(allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/cycle/main.or");

    // Should detect a cycle
    const cycle = try graph.detectCycles();
    try testing.expect(cycle != null);

    if (cycle) |c| {
        defer allocator.free(c);
        // Cycle should be: a -> b -> a
        try testing.expect(c.len >= 2);
        // The cycle contains 'a' and 'b'
        var has_a = false;
        var has_b = false;
        for (c) |module| {
            if (std.mem.eql(u8, module, "a")) has_a = true;
            if (std.mem.eql(u8, module, "b")) has_b = true;
        }
        try testing.expect(has_a);
        try testing.expect(has_b);
    }
}

test "no cycle in valid graph" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(allocator, "test_fixtures/nested", "stdlib", include_dirs);
    var graph = DependencyGraph.init(allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/nested/main.or");

    // Should not detect a cycle
    const cycle = try graph.detectCycles();
    try testing.expect(cycle == null);
}

test "topological sort simple graph" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(allocator, "test_fixtures/simple", "stdlib", include_dirs);
    var graph = DependencyGraph.init(allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/simple/main.or");

    const levels = try graph.topologicalSort();
    defer {
        for (levels) |*level| {
            level.deinit(allocator);
        }
        allocator.free(levels);
    }

    // Should have 2 levels:
    // Level 0: lexer (no dependencies)
    // Level 1: main (depends on lexer)
    try testing.expectEqual(@as(usize, 2), levels.len);

    // Level 0 should have lexer
    try testing.expectEqual(@as(usize, 1), levels[0].items.len);
    try testing.expectEqualStrings("lexer", levels[0].items[0]);

    // Level 1 should have main
    try testing.expectEqual(@as(usize, 1), levels[1].items.len);
    try testing.expectEqualStrings("main", levels[1].items[0]);
}

test "topological sort nested graph" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(allocator, "test_fixtures/nested", "stdlib", include_dirs);
    var graph = DependencyGraph.init(allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/nested/main.or");

    const levels = try graph.topologicalSort();
    defer {
        for (levels) |*level| {
            level.deinit(allocator);
        }
        allocator.free(levels);
    }

    // Should have 3 levels:
    // Level 0: lexer (no dependencies)
    // Level 1: parser (depends on lexer)
    // Level 2: main (depends on lexer and parser)
    try testing.expectEqual(@as(usize, 3), levels.len);

    // Level 0 should have lexer
    try testing.expectEqual(@as(usize, 1), levels[0].items.len);
    try testing.expectEqualStrings("lexer", levels[0].items[0]);

    // Level 1 should have parser
    try testing.expectEqual(@as(usize, 1), levels[1].items.len);
    try testing.expectEqualStrings("parser", levels[1].items[0]);

    // Level 2 should have main
    try testing.expectEqual(@as(usize, 1), levels[2].items.len);
    try testing.expectEqualStrings("main", levels[2].items[0]);
}
