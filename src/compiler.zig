const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const parser_module = @import("parser.zig");
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;
const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;
const ModuleResolver = @import("module.zig").ModuleResolver;
const TargetTriple = @import("target.zig").TargetTriple;

const AST = parser_module.AST;
const ImportDecl = parser_module.ImportDecl;
const ExternFunctionDecl = parser_module.ExternFunctionDecl;
const TypeDef = parser_module.TypeDef;
const ClassDef = parser_module.ClassDef;
const InstanceDecl = parser_module.InstanceDecl;
const FunctionDecl = parser_module.FunctionDecl;

pub const CompileOptions = struct {
    allocator: std.mem.Allocator,
    input_source: []const u8,
    input_path: []const u8,
    src_dir: []const u8,
    include_dirs: []const []const u8,
    target: TargetTriple,
};

/// Common compilation pipeline used by both main.zig and tests
/// Loads prelude, discovers modules, merges ASTs, typechecks, and generates LLVM IR
pub fn compileProgram(options: CompileOptions) ![]const u8 {
    const allocator = options.allocator;

    // Load and parse stdlib prelude
    const prelude_path = "stdlib/prelude.or";
    const prelude_source = try std.fs.cwd().readFileAlloc(allocator, prelude_path, 1024 * 1024);
    defer allocator.free(prelude_source);

    var prelude_lexer = Lexer.init(prelude_source);
    var prelude_tokens = try prelude_lexer.tokenize(allocator);
    defer prelude_tokens.deinit(allocator);

    var prelude_parser = Parser.init(prelude_tokens.items, allocator);
    var prelude_ast = try prelude_parser.parse();
    defer {
        prelude_ast.imports.deinit(allocator);
        prelude_ast.type_defs.deinit(allocator);
        prelude_ast.class_defs.deinit(allocator);
        prelude_ast.instances.deinit(allocator);
        prelude_ast.functions.deinit(allocator);
    }

    // Discover all modules via dependency graph
    var resolver = ModuleResolver.init(allocator, options.src_dir, "stdlib", options.include_dirs);
    var dep_graph = DependencyGraph.init(allocator, resolver);
    defer dep_graph.deinit();

    // First, discover prelude's imports (stdlib modules like std.string)
    for (prelude_ast.imports.items) |import_decl| {
        const import_path = switch (import_decl.item) {
            .module => |m| m,
            .specific => |s| s.path,
        };
        const mod_id = try resolver.resolve(import_path);
        defer resolver.deinitModuleId(mod_id);
        try dep_graph.discoverModule(mod_id.path, mod_id.file_path);
    }

    // Then discover user's imports
    try dep_graph.discover(options.input_path);

    // Check for circular dependencies
    if (try dep_graph.detectCycles()) |cycle| {
        defer allocator.free(cycle);
        std.debug.print("Error: Circular dependency detected:\n", .{});
        for (cycle, 0..) |module, idx| {
            std.debug.print("  {s}", .{module});
            if (idx < cycle.len - 1) {
                std.debug.print(" → ", .{});
            }
        }
        std.debug.print("\n", .{});
        return error.CircularDependency;
    }

    // Get topologically sorted compilation order
    const levels = try dep_graph.topologicalSort();
    defer {
        for (levels) |*level| {
            level.deinit(allocator);
        }
        allocator.free(levels);
    }

    // Load and parse all modules in dependency order
    var module_asts = std.ArrayList(AST).empty;
    defer {
        for (module_asts.items) |*mod_ast| {
            mod_ast.imports.deinit(allocator);
            mod_ast.type_defs.deinit(allocator);
            mod_ast.class_defs.deinit(allocator);
            mod_ast.instances.deinit(allocator);
            mod_ast.functions.deinit(allocator);
        }
        module_asts.deinit(allocator);
    }

    var module_sources = std.ArrayList([]const u8).empty;
    defer {
        for (module_sources.items) |source| {
            allocator.free(source);
        }
        module_sources.deinit(allocator);
    }

    for (levels) |level| {
        for (level.items) |module_path| {
            const mod_id = dep_graph.modules.get(module_path) orelse continue;

            const source = try std.fs.cwd().readFileAlloc(allocator, mod_id.file_path, 1024 * 1024);
            try module_sources.append(allocator, source);

            var lexer = Lexer.init(source);
            var tokens = try lexer.tokenize(allocator);
            defer tokens.deinit(allocator);

            var parser = Parser.init(tokens.items, allocator);
            const mod_ast = try parser.parse();
            try module_asts.append(allocator, mod_ast);
        }
    }

    // Parse user source (main file)
    var user_lexer = Lexer.init(options.input_source);
    var user_tokens = try user_lexer.tokenize(allocator);
    defer user_tokens.deinit(allocator);

    var user_parser = Parser.init(user_tokens.items, allocator);
    var user_ast = try user_parser.parse();
    defer {
        // Shallow cleanup - items are transferred to merged ast
        user_ast.imports.deinit(allocator);
        user_ast.extern_functions.deinit(allocator);
        user_ast.type_defs.deinit(allocator);
        user_ast.class_defs.deinit(allocator);
        user_ast.instances.deinit(allocator);
        user_ast.functions.deinit(allocator);
    }

    // Merge all ASTs: prelude + modules + user
    var ast = AST{
        .imports = std.ArrayList(ImportDecl).empty,
        .extern_functions = std.ArrayList(ExternFunctionDecl).empty,
        .type_defs = std.ArrayList(TypeDef).empty,
        .class_defs = std.ArrayList(ClassDef).empty,
        .instances = std.ArrayList(InstanceDecl).empty,
        .functions = std.ArrayList(FunctionDecl).empty,
    };
    defer ast.deinit(allocator);

    // Add stdlib items first
    try ast.imports.appendSlice(allocator, prelude_ast.imports.items);
    try ast.extern_functions.appendSlice(allocator, prelude_ast.extern_functions.items);
    try ast.type_defs.appendSlice(allocator, prelude_ast.type_defs.items);
    try ast.class_defs.appendSlice(allocator, prelude_ast.class_defs.items);
    try ast.instances.appendSlice(allocator, prelude_ast.instances.items);
    try ast.functions.appendSlice(allocator, prelude_ast.functions.items);

    // Add discovered modules
    for (module_asts.items) |mod_ast| {
        try ast.imports.appendSlice(allocator, mod_ast.imports.items);
        try ast.extern_functions.appendSlice(allocator, mod_ast.extern_functions.items);
        try ast.type_defs.appendSlice(allocator, mod_ast.type_defs.items);
        try ast.class_defs.appendSlice(allocator, mod_ast.class_defs.items);
        try ast.instances.appendSlice(allocator, mod_ast.instances.items);
        try ast.functions.appendSlice(allocator, mod_ast.functions.items);
    }

    // Add user's main file
    try ast.imports.appendSlice(allocator, user_ast.imports.items);
    try ast.extern_functions.appendSlice(allocator, user_ast.extern_functions.items);
    try ast.type_defs.appendSlice(allocator, user_ast.type_defs.items);
    try ast.class_defs.appendSlice(allocator, user_ast.class_defs.items);
    try ast.instances.appendSlice(allocator, user_ast.instances.items);
    try ast.functions.appendSlice(allocator, user_ast.functions.items);

    // Typecheck
    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    // Generate LLVM IR
    var codegen = Codegen.init(allocator, options.target);
    defer codegen.deinit();
    return try codegen.generate(&ast);
}
