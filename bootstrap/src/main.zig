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
const typechecker_module = @import("typechecker.zig");
const TypeChecker = typechecker_module.TypeChecker;
const ExternalSignature = typechecker_module.ExternalSignature;
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

/// Check if a module path is part of the standard library (starts with "std.")
fn isStdlibModule(module_path: []const u8) bool {
    return std.mem.startsWith(u8, module_path, "std.");
}

/// Get the cache directory for compiled stdlib artifacts
fn getStdlibCacheDir(allocator: std.mem.Allocator, target_triple: TargetTriple) ![]const u8 {
    const home = std.process.getEnvVarOwned(allocator, "HOME") catch {
        return try allocator.dupe(u8, "/tmp/.orion/stdlib");
    };
    defer allocator.free(home);

    const target_str = try target_triple.toLLVMTriple(allocator);
    defer allocator.free(target_str);

    return try std.fmt.allocPrint(allocator, "{s}/.orion/stdlib/{s}", .{ home, target_str });
}

/// Compile all stdlib modules into a single stdlib.a archive
fn compileStdlib(
    allocator: std.mem.Allocator,
    stdlib_modules: []const []const u8,
    module_asts: std.StringHashMap(AST),
    target_triple: TargetTriple,
    _: []const u8,
) ![]const u8 {
    if (stdlib_modules.len == 0) {
        return try allocator.dupe(u8, "");
    }

    const cache_dir = try getStdlibCacheDir(allocator, target_triple);
    defer allocator.free(cache_dir);

    // Create cache directory if it doesn't exist
    std.fs.cwd().makePath(cache_dir) catch |err| {
        std.debug.print("Warning: Could not create stdlib cache directory: {}\n", .{err});
    };

    const stdlib_archive_path = try std.fmt.allocPrint(allocator, "{s}/stdlib.a", .{cache_dir});

    // Check if cached stdlib.a already exists
    // For now, always recompile (Phase 0 can add hash validation later)
    const cached = std.fs.cwd().openFile(stdlib_archive_path, .{}) catch null;
    if (cached) |f| {
        f.close();
        std.debug.print("Using cached stdlib: {s}\n", .{stdlib_archive_path});
        return stdlib_archive_path;
    }

    std.debug.print("Compiling stdlib to {s}...\n", .{stdlib_archive_path});

    // Create combined AST for all stdlib modules
    var combined_ast = AST{
        .imports = std.ArrayList(parser_module.ImportDecl).empty,
        .extern_functions = std.ArrayList(ExternFunctionDecl).empty,
        .type_defs = std.ArrayList(TypeDef).empty,
        .class_defs = std.ArrayList(ClassDef).empty,
        .instances = std.ArrayList(InstanceDecl).empty,
        .functions = std.ArrayList(FunctionDecl).empty,
    };
    defer {
        combined_ast.imports.deinit(allocator);
        combined_ast.extern_functions.deinit(allocator);
        combined_ast.type_defs.deinit(allocator);
        combined_ast.class_defs.deinit(allocator);
        combined_ast.instances.deinit(allocator);
        combined_ast.functions.deinit(allocator);
    }

    // Combine all stdlib modules
    for (stdlib_modules) |stdlib_module_path| {
        if (module_asts.get(stdlib_module_path)) |mod_ast| {
            try combined_ast.imports.appendSlice(allocator, mod_ast.imports.items);
            try combined_ast.extern_functions.appendSlice(allocator, mod_ast.extern_functions.items);
            try combined_ast.type_defs.appendSlice(allocator, mod_ast.type_defs.items);
            try combined_ast.class_defs.appendSlice(allocator, mod_ast.class_defs.items);
            try combined_ast.instances.appendSlice(allocator, mod_ast.instances.items);
            try combined_ast.functions.appendSlice(allocator, mod_ast.functions.items);
        }
    }

    // Linearity inference (must run BEFORE type checking so annotations are inferred)
    var inference_engine = linearity_inference_module.LinearityInferenceEngine.init(allocator);
    defer inference_engine.deinit();
    try inference_engine.inferAll(&combined_ast);

    // Type check
    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.check(&combined_ast);

    // Generate LLVM IR (stdlib doesn't have _start)
    var codegen = Codegen.init(allocator, target_triple);
    defer codegen.deinit();
    codegen.generate_start = false;
    const llvm_ir = try codegen.generate(&combined_ast);
    defer allocator.free(llvm_ir);

    // Write LLVM IR to temporary file
    const ir_path = try std.fmt.allocPrint(allocator, "{s}/stdlib.ll", .{cache_dir});
    defer allocator.free(ir_path);
    try std.fs.cwd().writeFile(.{ .sub_path = ir_path, .data = llvm_ir });

    // Run coroutine passes
    const opt_ir_path = try std.fmt.allocPrint(allocator, "{s}/stdlib.opt.ll", .{cache_dir});
    defer allocator.free(opt_ir_path);
    try runCoroutinePasses(allocator, ir_path, opt_ir_path);

    // Generate object file
    const obj_path = try std.fmt.allocPrint(allocator, "{s}/stdlib.o", .{cache_dir});
    defer allocator.free(obj_path);
    try generateObjectFile(allocator, opt_ir_path, obj_path, target_triple);

    // Create archive from object file
    var ar_argv = std.ArrayList([]const u8).empty;
    defer ar_argv.deinit(allocator);
    try ar_argv.append(allocator, "ar");
    try ar_argv.append(allocator, "rcs");
    try ar_argv.append(allocator, stdlib_archive_path);
    try ar_argv.append(allocator, obj_path);

    try runCommand(allocator, ar_argv.items, "ar", error.ArchiveCreationFailed);

    std.debug.print("Created stdlib archive: {s}\n", .{stdlib_archive_path});
    return stdlib_archive_path;
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
        .max_output_bytes = 64 * 1024 * 1024,  // 64 MB buffer for compilation output
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Check if process exited with error or received signal
    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                if (result.stderr.len > 0) {
                    std.debug.print("Compilation failed:\n{s}\n", .{result.stderr});
                }
                return error.CompilationFailed;
            }
        },
        .Signal => |sig| {
            std.debug.print("Compilation process killed by signal {d}\n", .{sig});
            if (result.stderr.len > 0) {
                std.debug.print("Output: {s}\n", .{result.stderr});
            }
            return error.CompilationFailed;
        },
        else => {
            std.debug.print("Compilation process terminated abnormally\n", .{});
            if (result.stderr.len > 0) {
                std.debug.print("Output: {s}\n", .{result.stderr});
            }
            return error.CompilationFailed;
        },
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

    // Track prelude module dependencies for inclusion in all compiled modules
    var prelude_module_paths = std.ArrayList([]const u8).empty;
    defer {
        for (prelude_module_paths.items) |path| {
            allocator.free(path);
        }
        prelude_module_paths.deinit(allocator);
    }

    // First, discover prelude's imports (stdlib modules like std.string)
    for (prelude_ast.imports.items) |import_decl| {
        const import_path = switch (import_decl.item) {
            .module => |m| m,
            .specific => |s| s.path,
        };
        const mod_id = try resolver.resolve(import_path);
        defer resolver.deinitModuleId(mod_id);
        try prelude_module_paths.append(allocator, try allocator.dupe(u8, mod_id.path));
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

    // Two-pass compilation: parse all modules, then compile each separately
    // PASS 1: Load and parse all modules in dependency order
    var module_asts = std.StringHashMap(AST).init(allocator);
    defer {
        var iter = module_asts.iterator();
        while (iter.next()) |entry| {
            var mod_ast = entry.value_ptr.*;
            mod_ast.imports.deinit(allocator);
            mod_ast.extern_functions.deinit(allocator);
            mod_ast.type_defs.deinit(allocator);
            mod_ast.class_defs.deinit(allocator);
            mod_ast.instances.deinit(allocator);
            mod_ast.functions.deinit(allocator);
        }
        module_asts.deinit();
    }

    // Track module sources so they aren't freed while ASTs still reference them
    var module_sources = std.ArrayList([]const u8).empty;
    defer {
        for (module_sources.items) |source| {
            allocator.free(source);
        }
        module_sources.deinit(allocator);
    }

    // Collect module names in dependency order for Pass 2
    var module_order = std.ArrayList([]const u8).empty;
    defer module_order.deinit(allocator);

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

            try module_asts.put(try allocator.dupe(u8, module_path), mod_ast);
            try module_order.append(allocator, try allocator.dupe(u8, module_path));
        }
    }

    // PASS 2: Compile each module separately
    // First, separate stdlib and user modules
    var stdlib_modules = std.ArrayList([]const u8).empty;
    defer stdlib_modules.deinit(allocator);
    var user_modules = std.ArrayList([]const u8).empty;
    defer user_modules.deinit(allocator);

    for (module_order.items) |module_path| {
        if (isStdlibModule(module_path)) {
            try stdlib_modules.append(allocator, module_path);
        } else {
            try user_modules.append(allocator, module_path);
        }
    }

    // Compile stdlib (if any stdlib modules exist)
    const stdlib_archive = try compileStdlib(allocator, stdlib_modules.items, module_asts, target_triple, build_mode_str);
    defer allocator.free(stdlib_archive);

    var object_files = std.ArrayList([]const u8).empty;
    defer object_files.deinit(allocator);

    var all_needs_scheduler = false;
    var entrypoint_module_name: ?[]const u8 = null;

    // Determine entrypoint module name from input_path
    if (!std.mem.eql(u8, input_path, ".")) {
        // Extract the module name from the path structure
        // For now, track it separately
        entrypoint_module_name = input_path;
    }

    // Only compile user modules (stdlib already compiled to stdlib.a)
    for (user_modules.items) |module_path| {
        const mod_ast = module_asts.get(module_path) orelse continue;
        const is_entrypoint = (entrypoint_module_name != null and std.mem.eql(u8, module_path, entrypoint_module_name.?)) or
                             std.mem.eql(u8, module_path, input_path);

        // Create a combined AST with prelude + current module + other modules
        var combined_ast = AST{
            .imports = std.ArrayList(parser_module.ImportDecl).empty,
            .extern_functions = std.ArrayList(ExternFunctionDecl).empty,
            .type_defs = std.ArrayList(TypeDef).empty,
            .class_defs = std.ArrayList(ClassDef).empty,
            .instances = std.ArrayList(InstanceDecl).empty,
            .functions = std.ArrayList(FunctionDecl).empty,
        };

        // Add prelude type/class definitions (not functions or instances - they're in stdlib.a)
        // Instances are omitted because their method implementations are in stdlib.a, not user code
        try combined_ast.imports.appendSlice(allocator, prelude_ast.imports.items);
        try combined_ast.extern_functions.appendSlice(allocator, prelude_ast.extern_functions.items);
        try combined_ast.type_defs.appendSlice(allocator, prelude_ast.type_defs.items);
        try combined_ast.class_defs.appendSlice(allocator, prelude_ast.class_defs.items);
        // Note: prelude instances (with method implementations) are in stdlib.a, not embedded in user modules

        // Add prelude's dependencies (stdlib modules like std.string, std.alloc)
        // Only include type definitions, not functions or instances (they're in stdlib.a)
        // Skip current module if it happens to be a prelude dependency
        for (prelude_module_paths.items) |prelude_dep_path| {
            if (std.mem.eql(u8, prelude_dep_path, module_path)) continue;

            if (module_asts.get(prelude_dep_path)) |prelude_dep_ast| {
                try combined_ast.imports.appendSlice(allocator, prelude_dep_ast.imports.items);
                try combined_ast.extern_functions.appendSlice(allocator, prelude_dep_ast.extern_functions.items);
                try combined_ast.type_defs.appendSlice(allocator, prelude_dep_ast.type_defs.items);
                try combined_ast.class_defs.appendSlice(allocator, prelude_dep_ast.class_defs.items);
                // Note: stdlib instances (with method implementations) are in stdlib.a, not embedded in user modules
            }
        }

        // Add current module
        try combined_ast.imports.appendSlice(allocator, mod_ast.imports.items);
        try combined_ast.extern_functions.appendSlice(allocator, mod_ast.extern_functions.items);
        try combined_ast.type_defs.appendSlice(allocator, mod_ast.type_defs.items);
        try combined_ast.class_defs.appendSlice(allocator, mod_ast.class_defs.items);
        try combined_ast.instances.appendSlice(allocator, mod_ast.instances.items);
        try combined_ast.functions.appendSlice(allocator, mod_ast.functions.items);

        // Add type definitions from all other modules (but not their function implementations)
        var other_modules_iter = module_asts.iterator();
        while (other_modules_iter.next()) |entry| {
            const other_path = entry.key_ptr.*;

            // Skip current module (already added above)
            if (std.mem.eql(u8, other_path, module_path)) continue;

            // Skip ALL stdlib modules (std.*) since they're compiled to stdlib.a
            if (isStdlibModule(other_path)) continue;

            // Skip prelude dependencies (already added above)
            var is_prelude_dep = false;
            for (prelude_module_paths.items) |prelude_dep| {
                if (std.mem.eql(u8, other_path, prelude_dep)) {
                    is_prelude_dep = true;
                    break;
                }
            }
            if (is_prelude_dep) continue;

            const other_ast = entry.value_ptr.*;
            try combined_ast.type_defs.appendSlice(allocator, other_ast.type_defs.items);
            try combined_ast.class_defs.appendSlice(allocator, other_ast.class_defs.items);
            try combined_ast.instances.appendSlice(allocator, other_ast.instances.items);

            // Add function signatures from other modules as extern declarations
            // This allows referencing functions from other compiled modules
            for (other_ast.functions.items) |func| {
                // Mangle function name with module name from other_path
                const mod_basename = std.fs.path.basename(other_path);
                const mangled_name = if (std.mem.lastIndexOfScalar(u8, mod_basename, '.')) |dot_idx|
                    try std.fmt.allocPrint(allocator, "{s}__{s}", .{ mod_basename[0..dot_idx], func.name })
                else
                    try std.fmt.allocPrint(allocator, "{s}__{s}", .{ mod_basename, func.name });

                const extern_func = ExternFunctionDecl{
                    .name = mangled_name,
                    .params = func.params,
                    .return_type = func.return_type,
                };
                try combined_ast.extern_functions.append(allocator, extern_func);
            }
        }

        // Linearity inference (Phase 1)
        var inference_engine = linearity_inference_module.LinearityInferenceEngine.init(allocator);
        defer inference_engine.deinit();
        try inference_engine.inferAll(&combined_ast);

        // Type checker (Phase 2)
        var typechecker = TypeChecker.init(allocator);
        defer typechecker.deinit();
        try typechecker.check(&combined_ast);

        // Collect function names from current module for per-module compilation
        var module_func_names = std.ArrayList([]const u8).empty;
        defer module_func_names.deinit(allocator);
        for (mod_ast.functions.items) |func| {
            try module_func_names.append(allocator, try allocator.dupe(u8, func.name));
        }

        // Generate LLVM IR (only generate _start for entrypoint module)
        var codegen = Codegen.init(allocator, target_triple);
        defer codegen.deinit();
        codegen.generate_start = is_entrypoint;
        codegen.module_functions = module_func_names.items; // Only generate these functions

        // Set module name for function name mangling (extract from module path)
        // Only mangle non-entrypoint modules. Entrypoint module functions don't get mangled.
        // For example, "src/math.or" -> "math"
        if (!is_entrypoint) {
            const module_basename = std.fs.path.basename(module_path);
            if (std.mem.lastIndexOfScalar(u8, module_basename, '.')) |dot_idx| {
                codegen.module_name = try allocator.dupe(u8, module_basename[0..dot_idx]);
            } else {
                codegen.module_name = try allocator.dupe(u8, module_basename);
            }
        }

        const llvm_ir = try codegen.generate(&combined_ast);
        // Clean up module name after codegen
        if (codegen.module_name) |mod_name| allocator.free(mod_name);
        defer allocator.free(llvm_ir);

        // Cleanup module function names
        for (module_func_names.items) |name| {
            allocator.free(name);
        }

        all_needs_scheduler = all_needs_scheduler or codegen.needs_scheduler;

        // Determine output directory
        const mod_dir = std.fs.path.dirname(module_path) orelse ".";
        const mod_file = std.fs.path.basename(module_path);
        const ir_base = try std.fmt.allocPrint(allocator, "{s}/build/{s}/{s}/{s}", .{ project_root, build_mode_str, mod_dir, mod_file });
        defer allocator.free(ir_base);

        // Create directory structure
        if (std.fs.path.dirname(ir_base)) |dir| {
            std.fs.cwd().makePath(dir) catch |err| {
                std.debug.print("Warning: Could not create build directory: {}\n", .{err});
            };
        }

        // Write LLVM IR file
        const ir_path = try std.fmt.allocPrint(allocator, "{s}.ll", .{ir_base});
        defer allocator.free(ir_path);
        try std.fs.cwd().writeFile(.{ .sub_path = ir_path, .data = llvm_ir });

        // Run coroutine passes
        const opt_ir_path = try std.fmt.allocPrint(allocator, "{s}.opt.ll", .{ir_base});
        defer allocator.free(opt_ir_path);
        try runCoroutinePasses(allocator, ir_path, opt_ir_path);

        // Generate object file
        const obj_path = try std.fmt.allocPrint(allocator, "{s}.o", .{ir_base});
        try object_files.append(allocator, try allocator.dupe(u8, obj_path));
        try generateObjectFile(allocator, opt_ir_path, obj_path, target_triple);

        // Cleanup combined AST (don't deinit contents since they point to module_asts)
        combined_ast.imports.deinit(allocator);
        combined_ast.extern_functions.deinit(allocator);
        combined_ast.type_defs.deinit(allocator);
        combined_ast.class_defs.deinit(allocator);
        combined_ast.instances.deinit(allocator);
        combined_ast.functions.deinit(allocator);
    }

    // All modules compiled to .o files - now link them together
    if (stop_at_object) {
        std.debug.print("Generated {d} object files in build/{s}/\n", .{ object_files.items.len, build_mode_str });
        return;
    }

    // Determine executable path
    const input_filename = std.fs.path.basename(input_path);
    const exe_path = if (project_root.len > 0)
        try std.fmt.allocPrint(allocator, "{s}/{s}", .{ project_root, input_filename })
    else
        try allocator.dupe(u8, input_path);
    defer allocator.free(exe_path);

    // Link all object files into executable (with stdlib.a if available)
    try linkExecutable(allocator, object_files.items, exe_path, target_triple, all_needs_scheduler, stdlib_archive);

    std.debug.print("Generated executable: {s}\n", .{exe_path});
}

/// Compile a single module with external signatures (for two-pass compilation)
/// Returns the path to the generated .o file
fn compileModule(
    allocator: std.mem.Allocator,
    module_path: []const u8,
    is_entrypoint: bool,
    external_sigs: []const ExternalSignature,
    target_triple: TargetTriple,
    build_mode: []const u8,
    project_root: []const u8,
) ![]const u8 {
    // 1. Load source code
    const source = try std.fs.cwd().readFileAlloc(allocator, module_path, 1024 * 1024);
    defer allocator.free(source);

    // 2. Lex and parse
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(allocator);
    defer tokens.deinit(allocator);

    var parser = Parser.init(tokens.items, allocator);
    var ast = try parser.parse();
    defer {
        ast.imports.deinit(allocator);
        ast.extern_functions.deinit(allocator);
        ast.type_defs.deinit(allocator);
        ast.class_defs.deinit(allocator);
        ast.instances.deinit(allocator);
        ast.functions.deinit(allocator);
    }

    // 3. Linearity inference (must run BEFORE type checking so annotations are inferred)
    var inference_engine = linearity_inference_module.LinearityInferenceEngine.init(allocator);
    defer inference_engine.deinit();
    try inference_engine.inferAll(&ast);

    // 4. Type check with external signatures
    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.loadExternalSignatures(external_sigs);
    try typechecker.check(&ast);

    // 5. Generate LLVM IR (only entrypoint gets _start)
    var codegen = Codegen.init(allocator, target_triple);
    defer codegen.deinit();
    codegen.generate_start = is_entrypoint;
    const llvm_ir = try codegen.generate(&ast);
    defer allocator.free(llvm_ir);

    // 6. Determine output directory: build/{mode}/{module_dir}/
    const input_dirname = std.fs.path.dirname(module_path) orelse ".";
    const input_filename = std.fs.path.basename(module_path);
    const ir_base = try std.fmt.allocPrint(allocator, "{s}/build/{s}/{s}/{s}", .{ project_root, build_mode, input_dirname, input_filename });
    defer allocator.free(ir_base);

    // Create directory structure
    if (std.fs.path.dirname(ir_base)) |dir| {
        std.fs.cwd().makePath(dir) catch |err| {
            std.debug.print("Warning: Could not create build directory: {}\n", .{err});
        };
    }

    // 7. Write LLVM IR file
    const ir_path = try std.fmt.allocPrint(allocator, "{s}.ll", .{ir_base});
    defer allocator.free(ir_path);
    try std.fs.cwd().writeFile(.{ .sub_path = ir_path, .data = llvm_ir });

    // 8. Run coroutine passes
    const opt_ir_path = try std.fmt.allocPrint(allocator, "{s}.opt.ll", .{ir_base});
    defer allocator.free(opt_ir_path);
    try runCoroutinePasses(allocator, ir_path, opt_ir_path);

    // 9. Generate object file
    const obj_path = try std.fmt.allocPrint(allocator, "{s}.o", .{ir_base});
    defer allocator.free(obj_path);
    try generateObjectFile(allocator, opt_ir_path, obj_path, target_triple);

    // Return allocated copy of obj_path for caller to manage
    return try allocator.dupe(u8, obj_path);
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

fn linkExecutable(allocator: std.mem.Allocator, obj_paths: []const []const u8, exe_path: []const u8, target: TargetTriple, link_runtime: bool, stdlib_archive: []const u8) !void {
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

        // Add stdlib archive if it exists
        if (stdlib_archive.len > 0) {
            try argv.append(allocator, stdlib_archive);
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

        // Add stdlib archive if it exists
        if (stdlib_archive.len > 0) {
            try argv.append(allocator, stdlib_archive);
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

        // Add stdlib archive if it exists
        if (stdlib_archive.len > 0) {
            try argv.append(allocator, stdlib_archive);
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

            // Add stdlib archive if it exists
            if (stdlib_archive.len > 0) {
                try argv_gcc.append(allocator, stdlib_archive);
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
