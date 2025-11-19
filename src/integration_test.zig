const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;

fn compile(source: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(allocator);
    defer tokens.deinit(allocator);

    var parser = Parser.init(tokens.items, allocator);
    var ast = try parser.parse();
    defer ast.deinit(allocator);

    var typechecker = TypeChecker.init(allocator);
    defer typechecker.deinit();
    try typechecker.check(&ast);

    var codegen = Codegen.init(allocator);
    defer codegen.deinit();
    return try codegen.generate(&ast);
}

test "integration: compile simple function end-to-end" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return 42 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "entry:") != null);
}

test "integration: compile binary operations end-to-end" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return 10 + 20 * 3 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

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
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @f()") != null);
    }
}

test "integration: complex nested expressions" {
    const testing = std.testing;

    const source = "fn f() -> I32 { return ((1 + 2) * 3 - 4) / 5 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%t") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 1, 2") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "sub i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "sdiv i32") != null);
}

test "integration: unary operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> I32 { return -42 }",
        "fn f() -> I32 { return !1 }",
        "fn f() -> I32 { return ~0 }",
        "fn f() -> I32 { return --5 }",
        "fn f() -> I32 { return -5 + 3 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @f()") != null);
    }
}

test "integration: logical operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 5 && 3 }",
        "fn f() -> I32 { return 5 || 3 }",
        "fn f() -> I32 { return 1 && 0 }",
        "fn f() -> I32 { return 0 || 1 }",
        "fn f() -> Bool { return 5 && 3 }",
        "fn f() -> Bool { return 5 || 3 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp ne i32") != null);
    }
}

test "integration: comparison operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 5 == 3 }",
        "fn f() -> I32 { return 5 != 3 }",
        "fn f() -> I32 { return 5 < 3 }",
        "fn f() -> I32 { return 5 > 3 }",
        "fn f() -> I32 { return 5 <= 3 }",
        "fn f() -> I32 { return 5 >= 3 }",
        "fn f() -> Bool { return 5 == 3 }",
        "fn f() -> Bool { return 5 < 3 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp") != null);
    }
}

test "integration: bitwise operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 5 & 3 }",
        "fn f() -> I32 { return 5 | 3 }",
        "fn f() -> I32 { return 5 ^ 3 }",
        "fn f() -> I32 { return 5 << 2 }",
        "fn f() -> I32 { return 5 >> 2 }",
        "fn f() -> I32 { return ~5 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @f()") != null);
    }
}

test "integration: let binding without type annotation" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x = 42; return x }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32") != null);
}

test "integration: let binding with type annotation" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x: I32 = 10; return x }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 10") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32") != null);
}

test "integration: multiple let bindings" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x = 10; let y = 20; return x + y }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%x = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%y = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 10, ptr %x") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 20, ptr %y") != null);
}

test "integration: let binding with expression" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x = 5 + 3; return x * 2 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 5, 3") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32") != null);
}

test "integration: let binding with bool type" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x: Bool = 5 > 3; return x }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i1") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp sgt") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i1") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i1") != null);
}
