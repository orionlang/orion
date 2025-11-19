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
        std.debug.print("Usage: orion <input.or>\n", .{});
        return error.MissingInputFile;
    }

    const input_path = args[1];
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

    // Write output
    const output_path = try std.fmt.allocPrint(allocator, "{s}.ll", .{input_path});
    defer allocator.free(output_path);
    try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = llvm_ir });

    std.debug.print("Compiled {s} -> {s}\n", .{ input_path, output_path });
}
