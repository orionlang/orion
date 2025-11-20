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

test "integration: logical operators on integers" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 5 && 3 }",
        "fn f() -> I32 { return 5 || 3 }",
        "fn f() -> I32 { return 1 && 0 }",
        "fn f() -> I32 { return 0 || 1 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp ne i32") != null);
    }
}

test "integration: logical operators on bools" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() -> Bool { return true && false }",
        "fn f() -> Bool { return true || false }",
        "fn f() -> Bool { return (5 > 3) && (2 < 4) }",
        "fn f() -> Bool { return (5 > 3) || (2 < 4) }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        // Bool logical operators don't need icmp conversion
        try testing.expect(std.mem.indexOf(u8, llvm_ir, "and i1") != null or std.mem.indexOf(u8, llvm_ir, "or i1") != null);
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

test "integration: function parameters" {
    const testing = std.testing;

    const source = "fn add(x: I32, y: I32) -> I32 { return x + y }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Check function signature
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @add(i32 %x, i32 %y)") != null);
    
    // Check parameters are allocated and stored
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%x.addr = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%y.addr = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 %x, ptr %x.addr") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 %y, ptr %y.addr") != null);
    
    // Check parameters are loaded and used
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32, ptr %x.addr") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32, ptr %y.addr") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32") != null);
}

test "integration: function with multiple parameter types" {
    const testing = std.testing;

    const source = "fn mix(a: I64, b: U32, c: I8) -> I64 { return a }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Check function signature has all types
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i64 @mix(i64 %a, i32 %b, i8 %c)") != null);
    
    // Check all parameters allocated
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%a.addr = alloca i64") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%b.addr = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%c.addr = alloca i8") != null);
}

test "integration: function with no parameters" {
    const testing = std.testing;

    const source = "fn get_answer() -> I32 { return 42 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Check function signature with empty parameter list
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @get_answer()") != null);
}

test "integration: parameter usage in expressions" {
    const testing = std.testing;

    const source = "fn compute(x: I32, y: I32) -> I32 { let z: I32 = x * y; return z + x }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Parameters should be loaded multiple times
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32, ptr %x.addr") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32, ptr %y.addr") != null);
    
    // Check operations
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32") != null);
}

test "integration: small integer type parameters" {
    const testing = std.testing;

    // Test I8 parameters
    const i8_source = "fn use_i8(x: I8, y: I8) -> I8 { return x }";
    const i8_ir = try compile(i8_source, testing.allocator);
    defer testing.allocator.free(i8_ir);
    try testing.expect(std.mem.indexOf(u8, i8_ir, "define i8 @use_i8(i8 %x, i8 %y)") != null);
    try testing.expect(std.mem.indexOf(u8, i8_ir, "%x.addr = alloca i8") != null);

    // Test I16 parameters
    const i16_source = "fn use_i16(a: I16) -> I16 { return a }";
    const i16_ir = try compile(i16_source, testing.allocator);
    defer testing.allocator.free(i16_ir);
    try testing.expect(std.mem.indexOf(u8, i16_ir, "define i16 @use_i16(i16 %a)") != null);
    try testing.expect(std.mem.indexOf(u8, i16_ir, "%a.addr = alloca i16") != null);

    // Test U8 parameters
    const u8_source = "fn use_u8(b: U8) -> U8 { return b }";
    const u8_ir = try compile(u8_source, testing.allocator);
    defer testing.allocator.free(u8_ir);
    try testing.expect(std.mem.indexOf(u8, u8_ir, "define i8 @use_u8(i8 %b)") != null);

    // Test U16 parameters
    const u16_source = "fn use_u16(c: U16) -> U16 { return c }";
    const u16_ir = try compile(u16_source, testing.allocator);
    defer testing.allocator.free(u16_ir);
    try testing.expect(std.mem.indexOf(u8, u16_ir, "define i16 @use_u16(i16 %c)") != null);
}

test "integration: operations with small integer parameters" {
    const testing = std.testing;

    // Test I8 arithmetic
    const i8_source = "fn add_i8(x: I8, y: I8) -> I8 { return x + y }";
    const i8_ir = try compile(i8_source, testing.allocator);
    defer testing.allocator.free(i8_ir);
    try testing.expect(std.mem.indexOf(u8, i8_ir, "add i8") != null);

    // Test U16 arithmetic
    const u16_source = "fn mul_u16(a: U16, b: U16) -> U16 { return a * b }";
    const u16_ir = try compile(u16_source, testing.allocator);
    defer testing.allocator.free(u16_ir);
    try testing.expect(std.mem.indexOf(u8, u16_ir, "mul i16") != null);

    // Test signed division on I16
    const i16_div = "fn div_i16(x: I16, y: I16) -> I16 { return x / y }";
    const i16_ir = try compile(i16_div, testing.allocator);
    defer testing.allocator.free(i16_ir);
    try testing.expect(std.mem.indexOf(u8, i16_ir, "sdiv i16") != null);

    // Test unsigned division on U8
    const u8_div = "fn div_u8(x: U8, y: U8) -> U8 { return x / y }";
    const u8_ir = try compile(u8_div, testing.allocator);
    defer testing.allocator.free(u8_ir);
    try testing.expect(std.mem.indexOf(u8, u8_ir, "udiv i8") != null);
}

test "integration: simple function call" {
    const testing = std.testing;

    const source =
        \\fn add(x: I32, y: I32) -> I32 { return x + y }
        \\fn main() -> I32 { return add(10, 20) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that add function exists with parameters
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @add(i32 %x, i32 %y)") != null);

    // Check that main calls add with arguments
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @add(i32 10, i32 20)") != null);
}

test "integration: function call with variables" {
    const testing = std.testing;

    const source =
        \\fn add(x: I32, y: I32) -> I32 { return x + y }
        \\fn main() -> I32 { let a: I32 = 5; let b: I32 = 7; return add(a, b) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that add is called (variables will be loaded first)
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @add(") != null);
}

test "integration: multiple function calls" {
    const testing = std.testing;

    const source =
        \\fn add(x: I32, y: I32) -> I32 { return x + y }
        \\fn mul(x: I32, y: I32) -> I32 { return x * y }
        \\fn main() -> I32 { return add(mul(2, 3), mul(4, 5)) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check both functions exist
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @add(") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @mul(") != null);

    // Check nested calls exist
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @mul(i32 2, i32 3)") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @mul(i32 4, i32 5)") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @add(") != null);
}

test "integration: function call with no arguments" {
    const testing = std.testing;

    const source =
        \\fn get_answer() -> I32 { return 42 }
        \\fn main() -> I32 { return get_answer() }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that get_answer is called with no arguments
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @get_answer()") != null);
}

test "integration: function call with different types" {
    const testing = std.testing;

    const source =
        \\fn use_i64(x: I64) -> I64 { return x }
        \\fn use_i32(x: I32) -> I32 { return x }
        \\fn main() -> I32 { let a: I64 = 100; return 0 }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signatures
    try testing.expect(std.mem.indexOf(u8, ir, "define i64 @use_i64(i64 %x)") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @use_i32(i32 %x)") != null);
}

test "integration: recursive function call" {
    const testing = std.testing;

    const source =
        \\fn factorial(n: I32) -> I32 { return n * factorial(n - 1) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that factorial calls itself
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @factorial(") != null);
}

test "integration: function call with small integer types" {
    const testing = std.testing;

    const source =
        \\fn add_i8(x: I8, y: I8) -> I8 { return x + y }
        \\fn main() -> I32 { return 0 }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature with I8 parameters
    try testing.expect(std.mem.indexOf(u8, ir, "define i8 @add_i8(i8 %x, i8 %y)") != null);
}

test "integration: function call in let binding" {
    const testing = std.testing;

    const source =
        \\fn mul(x: I32, y: I32) -> I32 { return x * y }
        \\fn main() -> I32 { let result: I32 = mul(6, 7); return result }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that mul is called
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @mul(i32 6, i32 7)") != null);
    // Check result is allocated and result is loaded
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "load i32") != null);
}

test "integration: function call in binary operation" {
    const testing = std.testing;

    const source =
        \\fn get_five() -> I32 { return 5 }
        \\fn get_ten() -> I32 { return 10 }
        \\fn main() -> I32 { return get_five() + get_ten() }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check both functions are called
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @get_five()") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @get_ten()") != null);
    // Check addition of results
    try testing.expect(std.mem.indexOf(u8, ir, "add i32") != null);
}

test "integration: function call with mixed arguments" {
    const testing = std.testing;

    const source =
        \\fn calc(a: I32, b: I32, c: I32) -> I32 { return a + b + c }
        \\fn main() -> I32 { let x: I32 = 10; return calc(x, 20, x) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @calc(i32 %a, i32 %b, i32 %c)") != null);
    // Check that calc is called
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @calc(") != null);
}

test "integration: function returning different small types" {
    const testing = std.testing;

    const source =
        \\fn get_u8(x: U8) -> U8 { return x }
        \\fn get_i16(x: I16) -> I16 { return x }
        \\fn get_u32(x: U32) -> U32 { return x }
        \\fn main() -> I32 { return 0 }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check all function signatures
    try testing.expect(std.mem.indexOf(u8, ir, "define i8 @get_u8(i8 %x)") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "define i16 @get_i16(i16 %x)") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @get_u32(i32 %x)") != null);
}

test "integration: bool literals" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if true { 1 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that bool literals work (br on i1 constant)
    try testing.expect(std.mem.indexOf(u8, ir, "br i1 1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
}

test "integration: simple if/else expression" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if true { 42 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check basic blocks are created
    try testing.expect(std.mem.indexOf(u8, ir, "br i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge") != null);
    // Check phi node
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
}

test "integration: if/else with condition" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if 10 > 5 { 1 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check comparison
    try testing.expect(std.mem.indexOf(u8, ir, "icmp sgt i32 10, 5") != null);
    // Check control flow
    try testing.expect(std.mem.indexOf(u8, ir, "br i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
}

test "integration: nested if expressions" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check multiple basic blocks
    try testing.expect(std.mem.indexOf(u8, ir, "then0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge1") != null);
    // Check multiple phi nodes
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "integration: if/elseif/else chain" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested structure created by elseif
    try testing.expect(std.mem.indexOf(u8, ir, "then0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge1") != null);
    // Check multiple phi nodes for nested structure
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "integration: multiple elseif chain" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if false { 1 } elseif false { 2 } elseif false { 3 } elseif true { 4 } else { 5 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that deeply nested structure is created
    try testing.expect(std.mem.indexOf(u8, ir, "merge0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge2") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge3") != null);
    // Check multiple phi nodes
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 4), phi_count);
}

test "integration: if expression with variables" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x: I32 = 10; let y: I32 = 20; return if x > y { x } else { y } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check variables allocated
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    // Check comparison loads variables
    try testing.expect(std.mem.indexOf(u8, ir, "load i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "icmp sgt") != null);
    // Check phi node
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
}

test "integration: if expression returning different types" {
    const testing = std.testing;

    const source = "fn main() -> I64 { return if true { 100 } else { 200 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check phi node uses i32 (literals are I32)
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
    // Check sign extension to i64
    try testing.expect(std.mem.indexOf(u8, ir, "sext i32") != null);
    // Check return type
    try testing.expect(std.mem.indexOf(u8, ir, "ret i64") != null);
}

test "integration: if expression with bool logical operators" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if (10 > 5) && (3 < 7) { 1 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check comparisons
    try testing.expect(std.mem.indexOf(u8, ir, "icmp sgt") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "icmp slt") != null);
    // Check logical and on bool values
    try testing.expect(std.mem.indexOf(u8, ir, "and i1") != null);
}

test "integration: if expression in let binding" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let result: I32 = if true { 42 } else { 0 }; return result }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check if expression generates phi
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
    // Check result stored and loaded
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "load i32") != null);
}

test "integration: if expression in binary operation" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return (if true { 10 } else { 20 }) + (if false { 30 } else { 40 }) }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check two if expressions (4 phi nodes for nested structure)
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
    // Check addition
    try testing.expect(std.mem.indexOf(u8, ir, "add i32") != null);
}

test "integration: if expression with function calls in branches" {
    const testing = std.testing;

    const source =
        \\fn get_a() -> I32 { return 10 }
        \\fn get_b() -> I32 { return 20 }
        \\fn main() -> I32 { return if true { get_a() } else { get_b() } }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function calls in branches
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @get_a()") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @get_b()") != null);
    // Check phi node merges results
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
}

test "integration: elseif without final else should fail" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if false { 1 } elseif true { 2 } }";
    const result = compile(source, testing.allocator);

    // This should fail type checking (no else branch)
    try testing.expectError(error.TypeMismatch, result);
}

test "integration: if without else should fail" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if true { 1 } }";
    const result = compile(source, testing.allocator);

    // This should fail type checking (no else branch)
    try testing.expectError(error.TypeMismatch, result);
}

test "integration: deeply nested elseif chain" {
    const testing = std.testing;

    // 10 levels of elseif
    const source =
        \\fn main() -> I32 {
        \\  return if false { 1 }
        \\    elseif false { 2 }
        \\    elseif false { 3 }
        \\    elseif false { 4 }
        \\    elseif false { 5 }
        \\    elseif false { 6 }
        \\    elseif false { 7 }
        \\    elseif false { 8 }
        \\    elseif false { 9 }
        \\    elseif true { 10 }
        \\    else { 11 }
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should compile successfully with many nested ifs
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @main()") != null);
    // Should have many phi nodes
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expect(phi_count >= 10);
}

test "integration: if expression as function argument" {
    const testing = std.testing;

    const source =
        \\fn add(x: I32, y: I32) -> I32 { return x + y }
        \\fn main() -> I32 { return add(if true { 10 } else { 20 }, if false { 30 } else { 40 }) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that if expressions are evaluated and passed to function
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @add(") != null);
    // Should have phi nodes from both if expressions
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "integration: multiple if expressions in same statement" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return (if true { 1 } else { 2 }) + (if false { 3 } else { 4 }) + (if true { 5 } else { 6 }) }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have three phi nodes
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 3), phi_count);

    // Should have addition operations
    const add_count = std.mem.count(u8, ir, "add i32");
    try testing.expectEqual(@as(usize, 2), add_count);
}

test "integration: if expression with logical operator on bools" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return if (true && false) || (false || true) { 1 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have bool logical operations
    try testing.expect(std.mem.indexOf(u8, ir, "and i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "or i1") != null);
}

test "integration: if in let binding with elseif" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x: I32 = if false { 1 } elseif false { 2 } elseif true { 3 } else { 4 }; return x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "load i32") != null);
}

test "integration: simple while loop" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return while true { 42 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have while blocks
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_body0:") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_exit0:") != null);

    // Should return 0 (unit value)
    try testing.expect(std.mem.indexOf(u8, ir, "ret i32 0") != null);
}

test "integration: while loop with comparison condition" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return while 5 > 3 { 1 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have comparison in condition
    try testing.expect(std.mem.indexOf(u8, ir, "icmp") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
}

test "integration: while loop with variable condition" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x: Bool = true; return while x { 1 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should load variable in condition
    try testing.expect(std.mem.indexOf(u8, ir, "load i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
}

test "integration: nested while loops" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return while true { while false { 1 } } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have two sets of while blocks
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_header1:") != null);
}

test "integration: while loop in let binding" {
    const testing = std.testing;

    const source = "fn main() -> I32 { let x: I32 = while false { 1 }; return x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should store result in variable
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32 0") != null); // Store unit value
}

test "integration: while with integer condition should fail" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return while 5 { 1 } }";
    const result = compile(source, testing.allocator);

    // Should fail type checking
    try testing.expectError(error.TypeMismatch, result);
}

test "integration: while loop with if in body" {
    const testing = std.testing;

    const source = "fn main() -> I32 { return while true { if false { 1 } else { 2 } } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have both while and if blocks
    try testing.expect(std.mem.indexOf(u8, ir, "while_header") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else") != null);
}

test "integration: while loop as function argument" {
    const testing = std.testing;

    const source = "fn identity(x: I32) -> I32 { return x } fn main() -> I32 { return identity(while false { 1 }) }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should compile successfully with while loop
    try testing.expect(std.mem.indexOf(u8, ir, "while_header") != null);
    // Function definitions should be present
    try testing.expect(std.mem.indexOf(u8, ir, "define") != null);
}
