const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const AST = parser_module.AST;
const TypeDef = parser_module.TypeDef;
const ClassDef = parser_module.ClassDef;
const InstanceDecl = parser_module.InstanceDecl;
const FunctionDecl = parser_module.FunctionDecl;
const ExternFunctionDecl = parser_module.ExternFunctionDecl;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;
const target_module = @import("target.zig");
const TargetTriple = target_module.TargetTriple;
const ModuleResolver = @import("module.zig").ModuleResolver;
const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: orion <input.or> [options]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  -S                  Stop after generating LLVM IR (.ll)\n", .{});
        std.debug.print("  -c                  Stop after generating object file (.o)\n", .{});
        std.debug.print("  --target <triple>   Target triple (default: host)\n", .{});
        std.debug.print("  -I, --include <dir> Add directory to module search path\n", .{});
        return error.MissingInputFile;
    }

    const input_path = args[1];

    var stop_at_ir = false;
    var stop_at_object = false;
    var target_triple_str: ?[]const u8 = null;
    var include_dirs = std.ArrayList([]const u8).empty;
    defer include_dirs.deinit(allocator);

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-S")) {
            stop_at_ir = true;
        } else if (std.mem.eql(u8, args[i], "-c")) {
            stop_at_object = true;
        } else if (std.mem.eql(u8, args[i], "--target")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: --target requires an argument\n", .{});
                return error.MissingTargetTriple;
            }
            target_triple_str = args[i];
        } else if (std.mem.eql(u8, args[i], "-I") or std.mem.eql(u8, args[i], "--include")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: -I/--include requires an argument\n", .{});
                return error.MissingIncludeDir;
            }
            try include_dirs.append(allocator, args[i]);
        }
    }

    // Detect or parse target triple
    const target_triple = if (target_triple_str) |triple_str|
        try target_module.parseTargetTriple(allocator, triple_str)
    else
        target_module.detectHostTriple();
    defer {
        if (target_triple_str != null) {
            allocator.free(target_triple.arch);
            allocator.free(target_triple.vendor);
            allocator.free(target_triple.os);
        }
    }

    // Load and parse stdlib prelude
    const prelude_path = "stdlib/prelude.or";
    const prelude_source = std.fs.cwd().readFileAlloc(allocator, prelude_path, 1024 * 1024) catch |err| {
        std.debug.print("Warning: Could not load stdlib prelude: {}\n", .{err});
        std.debug.print("Continuing without stdlib...\n", .{});
        // Create empty prelude AST
        const empty_ast = AST{
            .imports = std.ArrayList(parser_module.ImportDecl).empty,
            .extern_functions = std.ArrayList(ExternFunctionDecl).empty,
            .type_defs = std.ArrayList(TypeDef).empty,
            .class_defs = std.ArrayList(ClassDef).empty,
            .instances = std.ArrayList(InstanceDecl).empty,
            .functions = std.ArrayList(FunctionDecl).empty,
        };
        _ = empty_ast;
        @panic("TODO: handle missing stdlib");
    };
    defer allocator.free(prelude_source);

    var prelude_lexer = Lexer.init(prelude_source);
    var prelude_tokens = try prelude_lexer.tokenize(allocator);
    defer prelude_tokens.deinit(allocator);

    var prelude_parser = Parser.init(prelude_tokens.items, allocator);
    var prelude_ast = try prelude_parser.parse();
    defer {
        // Note: prelude AST items are merged into main ast, so we only free the ArrayList containers
        prelude_ast.imports.deinit(allocator);
        prelude_ast.extern_functions.deinit(allocator);
        prelude_ast.type_defs.deinit(allocator);
        prelude_ast.class_defs.deinit(allocator);
        prelude_ast.instances.deinit(allocator);
        prelude_ast.functions.deinit(allocator);
    }

    // Derive src directory from input path
    // e.g., "test_multi/src/main.or" -> "test_multi/src"
    //       "src/main.or" -> "src"
    //       "main.or" -> "."
    const src_dir = blk: {
        if (std.fs.path.dirname(input_path)) |dir| {
            break :blk dir;
        }
        break :blk ".";
    };

    // Discover all modules via dependency graph
    var resolver = ModuleResolver.init(allocator, src_dir, "stdlib", include_dirs.items);
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

    try dep_graph.discover(input_path);

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

    // For bootstrap simplicity: load and parse all modules in order, merge ASTs
    // TODO: Full compiler should compile modules separately
    var module_asts = std.ArrayList(AST).empty;
    defer {
        // Note: module AST items are merged into main ast, so we only free the ArrayList containers
        // The actual items (functions, types, etc.) will be freed when main ast is deinit'd
        for (module_asts.items) |*mod_ast| {
            mod_ast.imports.deinit(allocator);
            mod_ast.extern_functions.deinit(allocator);
            mod_ast.type_defs.deinit(allocator);
            mod_ast.class_defs.deinit(allocator);
            mod_ast.instances.deinit(allocator);
            mod_ast.functions.deinit(allocator);
        }
        module_asts.deinit(allocator);
    }

    // Track module sources so they aren't freed while ASTs still reference them
    var module_sources = std.ArrayList([]const u8).empty;
    defer {
        for (module_sources.items) |source| {
            allocator.free(source);
        }
        module_sources.deinit(allocator);
    }

    // Load and parse each module in dependency order
    for (levels) |level| {
        for (level.items) |module_path| {
            const mod_id = dep_graph.modules.get(module_path) orelse continue;

            const source = try std.fs.cwd().readFileAlloc(allocator, mod_id.file_path, 1024 * 1024);
            try module_sources.append(allocator, source); // Keep source alive

            var lexer = Lexer.init(source);
            var tokens = try lexer.tokenize(allocator);
            defer tokens.deinit(allocator);

            var parser = Parser.init(tokens.items, allocator);
            const mod_ast = try parser.parse();
            try module_asts.append(allocator, mod_ast);
        }
    }

    // Merge stdlib and all discovered module ASTs
    var ast = AST{
        .imports = std.ArrayList(parser_module.ImportDecl).empty,
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

    // Add all discovered modules in dependency order
    for (module_asts.items) |mod_ast| {
        try ast.imports.appendSlice(allocator, mod_ast.imports.items);
        try ast.extern_functions.appendSlice(allocator, mod_ast.extern_functions.items);
        try ast.type_defs.appendSlice(allocator, mod_ast.type_defs.items);
        try ast.class_defs.appendSlice(allocator, mod_ast.class_defs.items);
        try ast.instances.appendSlice(allocator, mod_ast.instances.items);
        try ast.functions.appendSlice(allocator, mod_ast.functions.items);
    }

    // Type checker
    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    // Generate LLVM IR
    var codegen = Codegen.init(allocator, target_triple);
    defer codegen.deinit();
    const llvm_ir = try codegen.generate(&ast);
    defer allocator.free(llvm_ir);

    // Write LLVM IR file
    const ir_path = try std.fmt.allocPrint(allocator, "{s}.ll", .{input_path});
    defer allocator.free(ir_path);
    try std.fs.cwd().writeFile(.{ .sub_path = ir_path, .data = llvm_ir });

    if (stop_at_ir) {
        std.debug.print("Generated LLVM IR: {s}\n", .{ir_path});
        return;
    }

    // Generate object file using llc
    const obj_path = try std.fmt.allocPrint(allocator, "{s}.o", .{input_path});
    defer allocator.free(obj_path);

    try generateObjectFile(allocator, ir_path, obj_path, target_triple);

    if (stop_at_object) {
        std.debug.print("Generated object file: {s}\n", .{obj_path});
        return;
    }

    // Link executable with libc
    const exe_path = try getExecutablePath(allocator, input_path);
    defer allocator.free(exe_path);

    try linkExecutable(allocator, obj_path, exe_path, target_triple);

    std.debug.print("Generated executable: {s}\n", .{exe_path});
}

fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8, tool_name: []const u8, err: anyerror) !void {
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        std.debug.print("{s} failed:\n{s}\n", .{ tool_name, result.stderr });
        return err;
    }
}

fn generateObjectFile(allocator: std.mem.Allocator, ir_path: []const u8, obj_path: []const u8, target: TargetTriple) !void {
    const target_triple_str = try target.toLLVMTriple(allocator);
    defer allocator.free(target_triple_str);

    const mtriple_arg = try std.fmt.allocPrint(allocator, "-mtriple={s}", .{target_triple_str});
    defer allocator.free(mtriple_arg);

    try runCommand(
        allocator,
        &[_][]const u8{ "llc", "-filetype=obj", mtriple_arg, ir_path, "-o", obj_path },
        "llc",
        error.ObjectGenerationFailed,
    );
}

fn linkExecutable(allocator: std.mem.Allocator, obj_path: []const u8, exe_path: []const u8, target: TargetTriple) !void {
    const target_info = target.getTargetInfo();

    if (target_info.is_linux) {
        // Linux: use gcc as linker driver
        // gcc finds crt1.o, crti.o, crtn.o, and links with glibc
        try runCommand(
            allocator,
            &[_][]const u8{ "gcc", obj_path, "-o", exe_path, "-no-pie" },
            "gcc",
            error.LinkingFailed,
        );
    } else if (target_info.is_macos) {
        // macOS: use clang as linker driver
        // clang finds crt1.o and links with libSystem.dylib
        try runCommand(
            allocator,
            &[_][]const u8{ "clang", obj_path, "-o", exe_path },
            "clang",
            error.LinkingFailed,
        );
    } else if (target_info.is_windows) {
        // Windows: try clang-cl first (MSVC-compatible clang), fall back to MinGW
        // clang-cl links with UCRT (Universal C Runtime)
        // MinGW links with msvcrt.dll or its own runtime

        const fe_flag = try std.fmt.allocPrint(allocator, "/Fe{s}", .{exe_path});
        defer allocator.free(fe_flag);

        const clang_cl_result = runCommand(
            allocator,
            &[_][]const u8{ "clang-cl", obj_path, fe_flag },
            "clang-cl",
            error.LinkingFailed,
        );

        if (clang_cl_result) {
            // clang-cl succeeded
        } else |_| {
            // clang-cl failed, try MinGW gcc
            try runCommand(
                allocator,
                &[_][]const u8{ "x86_64-w64-mingw32-gcc", obj_path, "-o", exe_path },
                "x86_64-w64-mingw32-gcc",
                error.LinkingFailed,
            );
        }
    } else {
        // Fallback: try gcc as linker driver
        try runCommand(
            allocator,
            &[_][]const u8{ "gcc", obj_path, "-o", exe_path },
            "gcc",
            error.LinkingFailed,
        );
    }
}

fn getExecutablePath(allocator: std.mem.Allocator, input_path: []const u8) ![]const u8 {
    if (std.mem.lastIndexOf(u8, input_path, ".")) |dot_idx| {
        return try allocator.dupe(u8, input_path[0..dot_idx]);
    }
    return try allocator.dupe(u8, input_path);
}
