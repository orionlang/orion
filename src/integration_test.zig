const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;

test "integration: compile simple function end-to-end" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return 42 }";

    // Lex
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    // Parse
    var parser = Parser.init(tokens.items, testing.allocator);
    var ast = try parser.parse();
    defer ast.deinit(testing.allocator);

    // Type check
    var typechecker = TypeChecker.init(testing.allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    // Generate LLVM IR
    var codegen = Codegen.init(testing.allocator);
    defer codegen.deinit();
    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    // Verify output contains expected patterns
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "entry:") != null);
}

test "integration: compile binary operations end-to-end" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return 10 + 20 * 3 }";

    // Lex
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    // Parse
    var parser = Parser.init(tokens.items, testing.allocator);
    var ast = try parser.parse();
    defer ast.deinit(testing.allocator);

    // Type check
    var typechecker = TypeChecker.init(testing.allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    // Generate LLVM IR
    var codegen = Codegen.init(testing.allocator);
    defer codegen.deinit();
    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    // Verify output contains expected patterns
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32 20, 3") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 10") != null);
}

test "integration: arithmetic operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 10 + 5 }",
        "fn f() -> I32 { return 10 - 5 }",
        "fn f() -> I32 { return 10 * 5 }",
        "fn f() -> I32 { return 10 / 5 }",
        "fn f() -> I32 { return 10 % 3 }",
    };

    for (cases) |source| {
        var lexer = Lexer.init(source);
        var tokens = try lexer.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var parser = Parser.init(tokens.items, testing.allocator);
        var ast = try parser.parse();
        defer ast.deinit(testing.allocator);

        var typechecker = TypeChecker.init(testing.allocator);
        defer typechecker.deinit();
        try typechecker.check(&ast);

        var codegen = Codegen.init(testing.allocator);
        defer codegen.deinit();
        const llvm_ir = try codegen.generate(&ast);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @f()") != null);
    }
}

test "integration: complex nested expressions" {
    const testing = std.testing;

    const source = "fn f() -> I32 { return ((1 + 2) * 3 - 4) / 5 }";

    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var parser = Parser.init(tokens.items, testing.allocator);
    var ast = try parser.parse();
    defer ast.deinit(testing.allocator);

    var typechecker = TypeChecker.init(testing.allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    var codegen = Codegen.init(testing.allocator);
    defer codegen.deinit();
    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    // Verify it generates valid LLVM IR with temporaries
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%t") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 1, 2") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "sub i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "sdiv i32") != null);
}
