const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const AST = parser_module.AST;
const TypeDef = parser_module.TypeDef;
const ClassDef = parser_module.ClassDef;
const InstanceDecl = parser_module.InstanceDecl;
const FunctionDecl = parser_module.FunctionDecl;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;
const target_module = @import("target.zig");
const TargetTriple = target_module.TargetTriple;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: orion <input.or> [options]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  -S                Stop after generating LLVM IR (.ll)\n", .{});
        std.debug.print("  -c                Stop after generating object file (.o)\n", .{});
        std.debug.print("  --target <triple> Target triple (default: host)\n", .{});
        return error.MissingInputFile;
    }

    const input_path = args[1];

    var stop_at_ir = false;
    var stop_at_object = false;
    var target_triple_str: ?[]const u8 = null;

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
        prelude_ast.type_defs.deinit(allocator);
        prelude_ast.class_defs.deinit(allocator);
        prelude_ast.instances.deinit(allocator);
        prelude_ast.functions.deinit(allocator);
    }

    // Load and parse user source
    const source = try std.fs.cwd().readFileAlloc(allocator, input_path, 1024 * 1024);
    defer allocator.free(source);

    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(allocator);
    defer tokens.deinit(allocator);

    var parser = Parser.init(tokens.items, allocator);
    var user_ast = try parser.parse();
    defer {
        user_ast.type_defs.deinit(allocator);
        user_ast.class_defs.deinit(allocator);
        user_ast.instances.deinit(allocator);
        user_ast.functions.deinit(allocator);
    }

    // Merge stdlib and user AST
    var ast = AST{
        .type_defs = std.ArrayList(TypeDef).empty,
        .class_defs = std.ArrayList(ClassDef).empty,
        .instances = std.ArrayList(InstanceDecl).empty,
        .functions = std.ArrayList(FunctionDecl).empty,
    };
    defer ast.deinit(allocator);

    // Add stdlib items first
    try ast.type_defs.appendSlice(allocator, prelude_ast.type_defs.items);
    try ast.class_defs.appendSlice(allocator, prelude_ast.class_defs.items);
    try ast.instances.appendSlice(allocator, prelude_ast.instances.items);
    try ast.functions.appendSlice(allocator, prelude_ast.functions.items);

    // Add user items
    try ast.type_defs.appendSlice(allocator, user_ast.type_defs.items);
    try ast.class_defs.appendSlice(allocator, user_ast.class_defs.items);
    try ast.instances.appendSlice(allocator, user_ast.instances.items);
    try ast.functions.appendSlice(allocator, user_ast.functions.items);

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
