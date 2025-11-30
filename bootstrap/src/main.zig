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
const linearity_inference_module = @import("linearity_inference.zig");
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;
const target_module = @import("target.zig");
const TargetTriple = target_module.TargetTriple;
const ModuleResolver = @import("module.zig").ModuleResolver;
const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;
const manifest_module = @import("manifest.zig");
const Manifest = manifest_module.Manifest;

/// Simple signature structure for function/type declarations
/// Used for two-pass compilation: Pass 1 collects signatures, Pass 2 uses them
pub const Signature = struct {
    name: []const u8,
    is_public: bool,
    module_name: []const u8, // Which module this comes from
    kind: enum { function, type_def, class, instance }, // What kind of declaration
};

/// Collect function and type signatures from an AST (first pass of two-pass compilation)
fn collectSignatures(allocator: std.mem.Allocator, ast: *const AST, module_name: []const u8, signatures: *std.ArrayList(Signature)) !void {
    // Collect function signatures
    for (ast.functions.items) |func| {
        try signatures.append(allocator, .{
            .name = func.name,
            .is_public = func.is_public,
            .module_name = module_name,
            .kind = .function,
        });
    }

    // Collect type definitions
    for (ast.type_defs.items) |typedef| {
        try signatures.append(allocator, .{
            .name = typedef.name,
            .is_public = typedef.is_public,
            .module_name = module_name,
            .kind = .type_def,
        });
    }

    // Collect class definitions
    for (ast.class_defs.items) |classdef| {
        try signatures.append(allocator, .{
            .name = classdef.name,
            .is_public = classdef.is_public,
            .module_name = module_name,
            .kind = .class,
        });
    }

    // Collect instance declarations
    for (ast.instances.items) |instance| {
        try signatures.append(allocator, .{
            .name = instance.class_name, // Use class name as identifier
            .is_public = instance.is_public,
            .module_name = module_name,
            .kind = .instance,
        });
    }
}

/// Resolve the stdlib directory relative to the executable
fn getStdlibDirectory(allocator: std.mem.Allocator, exe_path: []const u8) ![]const u8 {
    // Get the directory containing the executable
    const exe_dir = std.fs.path.dirname(exe_path) orelse ".";

    // Stdlib is 3 levels up from bin/orion: ../../../stdlib
    // We build the path relative to the exe directory
    const stdlib_path = try std.fs.path.join(allocator, &.{ exe_dir, "../../../stdlib" });

    return stdlib_path;
}

/// Build command handler: `orion build [--release] [--lib]`
fn buildCommand(allocator: std.mem.Allocator, _: []const u8, build_args: []const []const u8) !void {
    // Look for orion.toml in current directory or parent directories
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = try std.fs.cwd().realpath(".", &cwd_buf);

    // Start search from current directory
    var manifest_path: ?[]const u8 = null;
    var search_path = try allocator.dupe(u8, cwd);
    defer allocator.free(search_path);

    // Look for orion.toml
    while (true) {
        const candidate = try std.fmt.allocPrint(allocator, "{s}/orion.toml", .{search_path});
        defer allocator.free(candidate);

        if (std.fs.openFileAbsolute(candidate, .{})) |file| {
            file.close();
            manifest_path = try allocator.dupe(u8, candidate);
            break;
        } else |_| {
            // Not found, try parent
        }

        // Try parent directory
        if (std.mem.lastIndexOf(u8, search_path, "/")) |last_slash| {
            if (last_slash == 0) break; // At root
            const parent = try allocator.dupe(u8, search_path[0..last_slash]);
            allocator.free(search_path);
            search_path = parent;
        } else {
            break;
        }
    }

    if (manifest_path == null) {
        std.debug.print("Error: orion.toml not found\n", .{});
        return error.ManifestNotFound;
    }
    defer allocator.free(manifest_path.?);

    // Parse manifest
    const manifest_content = std.fs.cwd().readFileAlloc(allocator, manifest_path.?, 1024 * 1024) catch |err| {
        std.debug.print("Error: Could not read manifest: {}\n", .{err});
        return err;
    };
    defer allocator.free(manifest_content);

    var manifest = manifest_module.parseManifest(allocator, manifest_content) catch |err| {
        std.debug.print("Error: Could not parse manifest: {}\n", .{err});
        return err;
    };
    defer manifest.deinit();

    // Parse build command arguments
    var is_release = false;
    var build_lib = false;

    for (build_args) |arg| {
        if (std.mem.eql(u8, arg, "--release")) {
            is_release = true;
        } else if (std.mem.eql(u8, arg, "--lib")) {
            build_lib = true;
        } else {
            std.debug.print("Error: Unknown build option: {s}\n", .{arg});
            return error.UnknownBuildOption;
        }
    }

    // Determine target based on --lib flag
    const target_is_lib = build_lib;

    // Get entrypoint
    var entrypoint: ?[]const u8 = null;
    if (target_is_lib) {
        if (manifest.lib) |lib| {
            entrypoint = lib.entrypoint;
        } else {
            std.debug.print("Error: No [lib] section in manifest\n", .{});
            return error.NoLibTarget;
        }
    } else {
        if (manifest.bin) |bin| {
            entrypoint = bin.entrypoint;
        } else {
            std.debug.print("Error: No [bin] section in manifest\n", .{});
            return error.NoBinTarget;
        }
    }

    // Extract project root directory from manifest path
    var project_root: []const u8 = "";
    if (std.mem.lastIndexOf(u8, manifest_path.?, "/")) |last_slash| {
        project_root = try allocator.dupe(u8, manifest_path.?[0..last_slash]);
    } else {
        project_root = try allocator.dupe(u8, ".");
    }
    defer allocator.free(project_root);

    // Determine build mode and output directory
    const build_mode = if (is_release) "release" else "debug";
    std.debug.print("Building {s} ({s} mode)...\n", .{ manifest.package.name, build_mode });

    // Re-invoke orion to compile the entrypoint
    // This reuses the existing single-file compilation pipeline
    const exe_path = std.fs.selfExePathAlloc(allocator) catch "/usr/bin/orion";
    defer allocator.free(exe_path);

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ exe_path, entrypoint.?, "--build-mode", build_mode },
        .cwd = project_root,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n", .{result.stderr});
        return error.CompilationFailed;
    }

    const output_name = std.fs.path.basename(entrypoint.?);
    if (std.mem.lastIndexOf(u8, output_name, ".")) |dot_idx| {
        std.debug.print("Generated executable: {s}/{s}\n", .{ project_root, output_name[0..dot_idx] });
    } else {
        std.debug.print("Generated executable: {s}/{s}\n", .{ project_root, output_name });
    }
}

/// Search upward from input_path for orion.toml
/// Returns the path to orion.toml if found, null otherwise
fn findProjectRoot(allocator: std.mem.Allocator, input_path: []const u8) !?[]const u8 {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;

    // Get absolute path of the directory containing input_path
    var search_dir = try std.fs.cwd().openDir(
        std.fs.path.dirname(input_path) orelse ".",
        .{}
    );
    defer search_dir.close();

    const current_path_slice = try search_dir.realpath(".", &path_buf);
    var current_path = try allocator.dupe(u8, current_path_slice);
    defer allocator.free(current_path);

    // Walk upward looking for orion.toml
    while (true) {
        // Check if orion.toml exists in current_path
        const manifest_path = try std.fmt.allocPrint(allocator, "{s}/orion.toml", .{current_path});
        if (std.fs.openFileAbsolute(manifest_path, .{})) |file| {
            file.close();
            // Found orion.toml - return the path (allocator will own it now)
            return manifest_path;
        } else |_| {
            // No orion.toml in this directory - free the attempted path
            allocator.free(manifest_path);
        }

        // Try to go up one directory by finding the last slash
        if (std.mem.lastIndexOf(u8, current_path, "/")) |last_slash| {
            if (last_slash == 0) {
                // Already at root
                return null;
            }
            // Create new path going up one level
            const parent_path = try allocator.dupe(u8, current_path[0..last_slash]);
            allocator.free(current_path);
            current_path = parent_path;
        } else {
            // No slash found, already at root
            return null;
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Get stdlib directory relative to executable (argv[0])
    const stdlib_dir = try getStdlibDirectory(allocator, args[0]);
    defer allocator.free(stdlib_dir);

    if (args.len < 2) {
        std.debug.print("Usage: orion <input.or> [options]\n", .{});
        std.debug.print("       orion build [--release] [--lib]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  -S                  Stop after generating LLVM IR (.ll)\n", .{});
        std.debug.print("  -c                  Stop after generating object file (.o)\n", .{});
        std.debug.print("  --target <triple>   Target triple (default: host)\n", .{});
        std.debug.print("  -I, --include <dir> Add directory to module search path\n", .{});
        return error.MissingInputFile;
    }

    // Handle help flag
    if (std.mem.eql(u8, args[1], "-h") or std.mem.eql(u8, args[1], "--help")) {
        std.debug.print("Usage: orion <input.or> [options]\n", .{});
        std.debug.print("       orion build [--release] [--lib]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  -S                  Stop after generating LLVM IR (.ll)\n", .{});
        std.debug.print("  -c                  Stop after generating object file (.o)\n", .{});
        std.debug.print("  --target <triple>   Target triple (default: host)\n", .{});
        std.debug.print("  -I, --include <dir> Add directory to module search path\n", .{});
        return;
    }

    // Handle "build" subcommand
    if (std.mem.eql(u8, args[1], "build")) {
        return try buildCommand(allocator, stdlib_dir, args[2..]);
    }

    var input_path: []const u8 = args[1];
    var manifest: ?Manifest = null;
    defer if (manifest) |*m| m.deinit();
    var project_root: []const u8 = ""; // Will be set if manifest is found

    // Try to find and parse orion.toml
    if (try findProjectRoot(allocator, input_path)) |manifest_path| {
        defer allocator.free(manifest_path);

        // Extract project root directory (parent of orion.toml)
        if (std.mem.lastIndexOf(u8, manifest_path, "/")) |last_slash| {
            project_root = try allocator.dupe(u8, manifest_path[0..last_slash]);
        } else {
            project_root = try allocator.dupe(u8, ".");
        }

        const manifest_content = std.fs.cwd().readFileAlloc(allocator, manifest_path, 1024 * 1024) catch |err| {
            std.debug.print("Error: Could not read manifest from {s}: {}\n", .{ manifest_path, err });
            allocator.free(project_root);
            return err;
        };
        defer allocator.free(manifest_content);

        manifest = manifest_module.parseManifest(allocator, manifest_content) catch |err| {
            std.debug.print("Error: Could not parse manifest: {}\n", .{err});
            allocator.free(project_root);
            return err;
        };

        // Use entrypoint from manifest if it has a bin target
        if (manifest.?.bin) |bin| {
            input_path = bin.entrypoint;
        }
    }
    defer if (project_root.len > 0) allocator.free(project_root);

    var stop_at_ir = false;
    var stop_at_object = false;
    var target_triple_str: ?[]const u8 = null;
    var build_mode_str: []const u8 = "debug"; // Default to debug mode
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
        } else if (std.mem.eql(u8, args[i], "--build-mode")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: --build-mode requires an argument\n", .{});
                return error.MissingBuildMode;
            }
            build_mode_str = args[i];
        } else if (std.mem.eql(u8, args[i], "-I") or std.mem.eql(u8, args[i], "--include")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: -I/--include requires an argument\n", .{});
                return error.MissingIncludeDir;
            }
            try include_dirs.append(allocator, args[i]);
        }
    }

    // Add manifest dependencies to include paths
    if (manifest) |m| {
        for (m.dependencies) |dep| {
            try include_dirs.append(allocator, dep.path);
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
    const prelude_path = try std.fs.path.join(allocator, &.{ stdlib_dir, "prelude.or" });
    defer allocator.free(prelude_path);

    const prelude_source = std.fs.cwd().readFileAlloc(allocator, prelude_path, 1024 * 1024) catch |err| {
        std.debug.print("Error: Could not load stdlib prelude from {s}: {}\n", .{ prelude_path, err });
        return err;
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
    var resolver = ModuleResolver.init(allocator, src_dir, stdlib_dir, include_dirs.items);
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

    // Linearity inference (Phase 1)
    var inference_engine = linearity_inference_module.LinearityInferenceEngine.init(allocator);
    defer inference_engine.deinit();
    try inference_engine.inferAll(&ast);

    // Type checker (Phase 2)
    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    // Generate LLVM IR
    var codegen = Codegen.init(allocator, target_triple);
    defer codegen.deinit();
    // Don't generate _start entry point if compiling to object only
    codegen.generate_start = !stop_at_object;
    const llvm_ir = try codegen.generate(&ast);
    defer allocator.free(llvm_ir);

    // Determine output paths
    // If project root is set, put executable there, intermediate files in build/ (mirroring src structure)
    var executable_dir: []const u8 = "";
    var intermediate_base: []const u8 = "";
    var exe_base: []const u8 = "";
    defer {
        if (executable_dir.len > 0) allocator.free(executable_dir);
        if (intermediate_base.len > 0) allocator.free(intermediate_base);
        if (exe_base.len > 0) allocator.free(exe_base);
    }

    const input_filename = std.fs.path.basename(input_path);

    if (project_root.len > 0) {
        // Executable goes in project root
        executable_dir = try allocator.dupe(u8, project_root);

        // Intermediate files go in build/{debug|release}/, mirroring the source directory structure
        const input_dirname = std.fs.path.dirname(input_path) orelse ".";
        const build_base = try std.fmt.allocPrint(allocator, "{s}/build/{s}", .{ project_root, build_mode_str });
        defer allocator.free(build_base);

        // Create full build path mirroring input structure
        intermediate_base = try std.fmt.allocPrint(allocator, "{s}/{s}/{s}", .{ build_base, input_dirname, input_filename });

        // Create build directory structure if it doesn't exist
        if (std.fs.path.dirname(intermediate_base)) |dir| {
            std.fs.cwd().makePath(dir) catch |err| {
                std.debug.print("Warning: Could not create build directory structure: {}\n", .{err});
            };
        }

        // Executable goes to project root with just the filename
        exe_base = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ executable_dir, input_filename });
    } else {
        // No manifest: put everything in current directory (for backward compatibility)
        executable_dir = try allocator.dupe(u8, ".");
        intermediate_base = try allocator.dupe(u8, input_path);
        exe_base = try allocator.dupe(u8, input_path);
    }

    // Write LLVM IR file
    const ir_path = try std.fmt.allocPrint(allocator, "{s}.ll", .{intermediate_base});
    defer allocator.free(ir_path);
    try std.fs.cwd().writeFile(.{ .sub_path = ir_path, .data = llvm_ir });

    if (stop_at_ir) {
        std.debug.print("Generated LLVM IR: {s}\n", .{ir_path});
        return;
    }

    // Run LLVM opt passes for coroutine lowering
    const opt_ir_path = try std.fmt.allocPrint(allocator, "{s}.opt.ll", .{intermediate_base});
    defer allocator.free(opt_ir_path);

    try runCoroutinePasses(allocator, ir_path, opt_ir_path);

    // Generate object file using llc
    const obj_path = try std.fmt.allocPrint(allocator, "{s}.o", .{intermediate_base});
    defer allocator.free(obj_path);

    try generateObjectFile(allocator, opt_ir_path, obj_path, target_triple);

    if (stop_at_object) {
        std.debug.print("Generated object file: {s}\n", .{obj_path});
        return;
    }

    // Link executable with libc
    const exe_path = try getExecutablePath(allocator, exe_base);
    defer allocator.free(exe_path);

    try linkExecutable(allocator, &[_][]const u8{obj_path}, exe_path, target_triple, codegen.needs_scheduler);

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

fn runCoroutinePasses(allocator: std.mem.Allocator, ir_path: []const u8, opt_ir_path: []const u8) !void {
    // Run LLVM opt with coroutine passes: coro-early, coro-split, coro-elide, coro-cleanup
    // These transform presplitcoroutine functions into state machines
    try runCommand(
        allocator,
        &[_][]const u8{
            "opt",
            "-S", // Output as text IR for debugging
            "-passes=coro-early,coro-split,coro-elide,coro-cleanup",
            ir_path,
            "-o",
            opt_ir_path,
        },
        "opt",
        error.CoroutinePassesFailed,
    );
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

fn linkExecutable(allocator: std.mem.Allocator, obj_paths: []const []const u8, exe_path: []const u8, target: TargetTriple, link_runtime: bool) !void {
    const target_info = target.getTargetInfo();

    // Find the runtime library path (relative to compiler location or installed)
    // For now, use a path relative to the project root
    const runtime_lib = "zig-out/lib/liborion_runtime.a";

    if (target_info.is_linux) {
        // Linux: use lld directly with our generated _start
        // No crt files needed - we generate _start in the LLVM IR
        var argv = std.ArrayList([]const u8).empty;
        defer argv.deinit(allocator);

        try argv.append(allocator, "ld.lld");
        try argv.append(allocator, "-o");
        try argv.append(allocator, exe_path);

        // Add all object files
        for (obj_paths) |obj_path| {
            try argv.append(allocator, obj_path);
        }

        if (link_runtime) {
            try argv.append(allocator, runtime_lib);
        }

        try argv.append(allocator, "-L/usr/lib");
        try argv.append(allocator, "-lc");
        try argv.append(allocator, "-lpthread");
        try argv.append(allocator, "--dynamic-linker=/lib64/ld-linux-x86-64.so.2");

        try runCommand(allocator, argv.items, "ld.lld", error.LinkingFailed);
    } else if (target_info.is_macos) {
        // macOS: use clang as linker driver
        var argv = std.ArrayList([]const u8).empty;
        defer argv.deinit(allocator);

        try argv.append(allocator, "clang");

        // Add all object files
        for (obj_paths) |obj_path| {
            try argv.append(allocator, obj_path);
        }

        if (link_runtime) {
            try argv.append(allocator, runtime_lib);
            try argv.append(allocator, "-lpthread");
        }

        try argv.append(allocator, "-o");
        try argv.append(allocator, exe_path);

        try runCommand(allocator, argv.items, "clang", error.LinkingFailed);
    } else if (target_info.is_windows) {
        // Windows: try clang-cl first, fall back to MinGW gcc
        const fe_flag = try std.fmt.allocPrint(allocator, "/Fe{s}", .{exe_path});
        defer allocator.free(fe_flag);

        var argv = std.ArrayList([]const u8).empty;
        defer argv.deinit(allocator);

        try argv.append(allocator, "clang-cl");
        for (obj_paths) |obj_path| {
            try argv.append(allocator, obj_path);
        }
        try argv.append(allocator, fe_flag);

        if (runCommand(allocator, argv.items, "clang-cl", error.LinkingFailed)) {
            // clang-cl succeeded
        } else |_| {
            // clang-cl failed, try MinGW gcc
            var argv_gcc = std.ArrayList([]const u8).empty;
            defer argv_gcc.deinit(allocator);

            try argv_gcc.append(allocator, "x86_64-w64-mingw32-gcc");
            for (obj_paths) |obj_path| {
                try argv_gcc.append(allocator, obj_path);
            }
            try argv_gcc.append(allocator, "-o");
            try argv_gcc.append(allocator, exe_path);

            try runCommand(allocator, argv_gcc.items, "x86_64-w64-mingw32-gcc", error.LinkingFailed);
        }
    } else {
        // Fallback: try gcc as linker driver
        var argv = std.ArrayList([]const u8).empty;
        defer argv.deinit(allocator);

        try argv.append(allocator, "gcc");
        for (obj_paths) |obj_path| {
            try argv.append(allocator, obj_path);
        }

        if (link_runtime) {
            try argv.append(allocator, runtime_lib);
            try argv.append(allocator, "-lpthread");
        }

        try argv.append(allocator, "-o");
        try argv.append(allocator, exe_path);

        try runCommand(allocator, argv.items, "gcc", error.LinkingFailed);
    }
}

fn linkLibrary(allocator: std.mem.Allocator, obj_paths: []const []const u8, lib_path: []const u8, target: TargetTriple, lib_type: enum { static, shared }) !void {
    const target_info = target.getTargetInfo();

    if (lib_type == .static) {
        // Static library: use ar rcs
        var argv = std.ArrayList([]const u8).empty;
        defer argv.deinit(allocator);

        try argv.append(allocator, "ar");
        try argv.append(allocator, "rcs");
        try argv.append(allocator, lib_path);

        for (obj_paths) |obj_path| {
            try argv.append(allocator, obj_path);
        }

        try runCommand(allocator, argv.items, "ar", error.LibraryGenerationFailed);
    } else {
        // Shared library
        var argv = std.ArrayList([]const u8).empty;
        defer argv.deinit(allocator);

        if (target_info.is_linux) {
            try argv.append(allocator, "ld.lld");
            try argv.append(allocator, "-shared");
            try argv.append(allocator, "-o");
            try argv.append(allocator, lib_path);

            for (obj_paths) |obj_path| {
                try argv.append(allocator, obj_path);
            }

            try argv.append(allocator, "-lc");
            try runCommand(allocator, argv.items, "ld.lld", error.LibraryGenerationFailed);
        } else if (target_info.is_macos) {
            try argv.append(allocator, "clang");
            try argv.append(allocator, "-dynamiclib");
            try argv.append(allocator, "-o");
            try argv.append(allocator, lib_path);

            for (obj_paths) |obj_path| {
                try argv.append(allocator, obj_path);
            }

            try runCommand(allocator, argv.items, "clang", error.LibraryGenerationFailed);
        } else {
            // Fallback for other platforms
            try argv.append(allocator, "gcc");
            try argv.append(allocator, "-shared");
            try argv.append(allocator, "-o");
            try argv.append(allocator, lib_path);

            for (obj_paths) |obj_path| {
                try argv.append(allocator, obj_path);
            }

            try runCommand(allocator, argv.items, "gcc", error.LibraryGenerationFailed);
        }
    }
}

fn getExecutablePath(allocator: std.mem.Allocator, input_path: []const u8) ![]const u8 {
    if (std.mem.lastIndexOf(u8, input_path, ".")) |dot_idx| {
        return try allocator.dupe(u8, input_path[0..dot_idx]);
    }
    return try allocator.dupe(u8, input_path);
}
