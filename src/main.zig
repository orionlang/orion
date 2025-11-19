const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: orion <input.or> [-c | -S]\n", .{});
        std.debug.print("  -S: Stop after generating LLVM IR (.ll)\n", .{});
        std.debug.print("  -c: Stop after generating object file (.o)\n", .{});
        std.debug.print("  (no flag): Generate executable\n", .{});
        return error.MissingInputFile;
    }

    const input_path = args[1];

    const stop_at_ir = args.len > 2 and std.mem.eql(u8, args[2], "-S");
    const stop_at_object = args.len > 2 and std.mem.eql(u8, args[2], "-c");

    const source = try std.fs.cwd().readFileAlloc(allocator, input_path, 1024 * 1024);
    defer allocator.free(source);

    // Lexer
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(allocator);
    defer tokens.deinit(allocator);

    // Parser
    var parser = Parser.init(tokens.items, allocator);
    var ast = try parser.parse();
    defer ast.deinit(allocator);

    // Type checker
    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    // Generate LLVM IR
    var codegen = Codegen.init(allocator);
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

    try generateObjectFile(allocator, ir_path, obj_path);

    if (stop_at_object) {
        std.debug.print("Generated object file: {s}\n", .{obj_path});
        return;
    }

    // Link executable using lld
    const exe_path = try getExecutablePath(allocator, input_path);
    defer allocator.free(exe_path);

    try linkExecutable(allocator, obj_path, exe_path);

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

fn generateObjectFile(allocator: std.mem.Allocator, ir_path: []const u8, obj_path: []const u8) !void {
    try runCommand(
        allocator,
        &[_][]const u8{ "llc", "-filetype=obj", ir_path, "-o", obj_path },
        "llc",
        error.ObjectGenerationFailed,
    );
}

fn linkExecutable(allocator: std.mem.Allocator, obj_path: []const u8, exe_path: []const u8) !void {
    try runCommand(
        allocator,
        &[_][]const u8{ "ld.lld", obj_path, "-o", exe_path, "-e", "_start" },
        "ld.lld",
        error.LinkingFailed,
    );
}

fn getExecutablePath(allocator: std.mem.Allocator, input_path: []const u8) ![]const u8 {
    if (std.mem.lastIndexOf(u8, input_path, ".")) |dot_idx| {
        return try allocator.dupe(u8, input_path[0..dot_idx]);
    }
    return try allocator.dupe(u8, input_path);
}
