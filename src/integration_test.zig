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

test "integration: integer type annotations" {
    const testing = std.testing;

    // Test that different integer types parse and generate correct LLVM types
    // Only test types that accept I32 literals (>= 32 bits)
    const cases = [_]struct { source: []const u8, llvm_type: []const u8 }{
        .{ .source = "fn f() -> I32 { return 42 }", .llvm_type = "i32" },
        .{ .source = "fn f() -> I64 { return 42 }", .llvm_type = "i64" },
        .{ .source = "fn f() -> U32 { return 42 }", .llvm_type = "i32" },
        .{ .source = "fn f() -> U64 { return 42 }", .llvm_type = "i64" },
    };

    for (cases) |case| {
        const llvm_ir = try compile(case.source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        const expected_return = try std.fmt.allocPrint(testing.allocator, "ret {s}", .{case.llvm_type});
        defer testing.allocator.free(expected_return);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, expected_return) != null);
    }
}

test "integration: integer type conversions" {
    const testing = std.testing;

    // Test sign extension (i32 -> i64)
    const sext_source = "fn f() -> I64 { return 42 }";
    const sext_ir = try compile(sext_source, testing.allocator);
    defer testing.allocator.free(sext_ir);
    try testing.expect(std.mem.indexOf(u8, sext_ir, "sext i32") != null);
    try testing.expect(std.mem.indexOf(u8, sext_ir, "to i64") != null);

    // Test no conversion needed (i32 -> u32, same LLVM type)
    const noconv_source = "fn f() -> U32 { return 42 }";
    const noconv_ir = try compile(noconv_source, testing.allocator);
    defer testing.allocator.free(noconv_ir);
    try testing.expect(std.mem.indexOf(u8, noconv_ir, "ret i32 42") != null);
}

test "integration: let bindings with all integer types" {
    const testing = std.testing;

    const literal_cases = [_]struct { type_name: []const u8, llvm_type: []const u8 }{
        .{ .type_name = "I32", .llvm_type = "i32" },
        .{ .type_name = "I64", .llvm_type = "i64" },
        .{ .type_name = "U32", .llvm_type = "i32" },
        .{ .type_name = "U64", .llvm_type = "i64" },
    };

    for (literal_cases) |case| {
        const source = try std.fmt.allocPrint(testing.allocator, "fn f() -> {s} {{ let x: {s} = 10 + 5; return x }}", .{ case.type_name, case.type_name });
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        const expected_alloca = try std.fmt.allocPrint(testing.allocator, "alloca {s}", .{case.llvm_type});
        defer testing.allocator.free(expected_alloca);

        const expected_load = try std.fmt.allocPrint(testing.allocator, "load {s}", .{case.llvm_type});
        defer testing.allocator.free(expected_load);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, expected_alloca) != null);
        try testing.expect(std.mem.indexOf(u8, llvm_ir, expected_load) != null);
    }

    // Note: I8, I16, U8, U16 types cannot currently be tested with literals
    // because literals default to I32 and narrowing conversions are not implicit.
    // These types will be testable once we add:
    // - Function parameters (to pass values of these types)
    // - Typed literal syntax (like 42i8)
    // - Explicit cast syntax
}

test "integration: arithmetic operations with typed variables" {
    const testing = std.testing;

    const source = "fn f() -> I64 { let x: I64 = 10; let y: I64 = 20; return x + y }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i64") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i64") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i64") != null); // Operations use the variable type
}

test "integration: signed/unsigned operations" {
    const testing = std.testing;

    // Test division: sdiv for signed, udiv for unsigned
    const div_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "I32", .expected_inst = "sdiv" },
        .{ .type_name = "U32", .expected_inst = "udiv" },
    };

    for (div_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() -> {s} {{ let x: {s} = 10; let y: {s} = 2; return x / y }}", .{ case.type_name, case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }

    // Test modulo: srem for signed, urem for unsigned
    const mod_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "I32", .expected_inst = "srem" },
        .{ .type_name = "U32", .expected_inst = "urem" },
    };

    for (mod_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() -> {s} {{ let x: {s} = 10; let y: {s} = 3; return x % y }}", .{ case.type_name, case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }

    // Test comparisons: slt/sgt for signed, ult/ugt for unsigned
    const cmp_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "I32", .expected_inst = "icmp slt" },
        .{ .type_name = "U32", .expected_inst = "icmp ult" },
    };

    for (cmp_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() -> Bool {{ let x: {s} = 10; let y: {s} = 20; return x < y }}", .{ case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }

    // Test shift right: ashr for signed, lshr for unsigned
    const shift_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "I32", .expected_inst = "ashr" },
        .{ .type_name = "U32", .expected_inst = "lshr" },
    };

    for (shift_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() -> {s} {{ let x: {s} = 16; let y: {s} = 2; return x >> y }}", .{ case.type_name, case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }
}

test "integration: type extension conversions" {
    const testing = std.testing;

    // Test sign extension for signed types (I32 literal -> I64 return)
    const sext_source = "fn f() -> I64 { return 42 }";
    const sext_llvm = try compile(sext_source, testing.allocator);
    defer testing.allocator.free(sext_llvm);
    try testing.expect(std.mem.indexOf(u8, sext_llvm, "sext") != null);

    // Test zero extension: Bool (unsigned) -> any integer uses zext
    const zext_source = "fn f() -> I32 { return 5 > 3 }";
    const zext_llvm = try compile(zext_source, testing.allocator);
    defer testing.allocator.free(zext_llvm);
    try testing.expect(std.mem.indexOf(u8, zext_llvm, "zext") != null);
}

test "integration: implicit literal conversions" {
    const testing = std.testing;

    // I32 literal -> I64: sign extend (preserves sign of literal)
    const i32_to_i64 = "fn f() -> I64 { return 42 }";
    const ir1 = try compile(i32_to_i64, testing.allocator);
    defer testing.allocator.free(ir1);
    try testing.expect(std.mem.indexOf(u8, ir1, "sext i32 42 to i64") != null);

    // I32 literal -> U64: uses sext (treating literal as signed source)
    // This is safe for positive literals; negative literals would sign-extend
    const i32_to_u64 = "fn f() -> U64 { return 42 }";
    const ir2 = try compile(i32_to_u64, testing.allocator);
    defer testing.allocator.free(ir2);
    try testing.expect(std.mem.indexOf(u8, ir2, "sext i32 42 to i64") != null);

    // I32 literal -> U32: no conversion needed (same LLVM type)
    const i32_to_u32 = "fn f() -> U32 { return 42 }";
    const ir3 = try compile(i32_to_u32, testing.allocator);
    defer testing.allocator.free(ir3);
    try testing.expect(std.mem.indexOf(u8, ir3, "ret i32 42") != null);

    // I32 literal -> I8/U8: narrowing not allowed (would lose data)
    // These are compile errors now - no implicit narrowing conversions
}

test "integration: bool to integer type conversions" {
    const testing = std.testing;

    const cases = [_]struct { type_name: []const u8, llvm_type: []const u8 }{
        .{ .type_name = "I8", .llvm_type = "i8" },
        .{ .type_name = "I16", .llvm_type = "i16" },
        .{ .type_name = "I32", .llvm_type = "i32" },
        .{ .type_name = "I64", .llvm_type = "i64" },
        .{ .type_name = "U8", .llvm_type = "i8" },
        .{ .type_name = "U16", .llvm_type = "i16" },
        .{ .type_name = "U32", .llvm_type = "i32" },
        .{ .type_name = "U64", .llvm_type = "i64" },
    };

    for (cases) |case| {
        const source = try std.fmt.allocPrint(testing.allocator, "fn f() -> {s} {{ return 5 > 3 }}", .{case.type_name});
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        // Should have bool comparison
        try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp") != null);
        // Should have zext from i1 to target type
        try testing.expect(std.mem.indexOf(u8, llvm_ir, "zext i1") != null);
    }
}
