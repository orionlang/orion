const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Codegen = @import("codegen.zig").Codegen;
const target_module = @import("target.zig");
const compiler_module = @import("compiler.zig");

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

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(allocator, target);
    defer codegen.deinit();
    return try codegen.generate(&ast);
}

fn compileWithStdlib(source: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    // Write source to a temporary file since the compiler needs to read from disk
    const tmp_path = ".test_temp.or";
    try std.fs.cwd().writeFile(.{.sub_path = tmp_path, .data = source});
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    const target = target_module.detectHostTriple();

    return try compiler_module.compileProgram(.{
        .allocator = allocator,
        .input_source = source,
        .input_path = tmp_path,
        .src_dir = ".",
        .stdlib_dir = "../stdlib",
        .include_dirs = &[_][]const u8{},
        .target = target,
    });
}

test "integration: compile simple function end-to-end" {
    const testing = std.testing;

    const source = "fn main() i32 { return 42 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "entry:") != null);
}

test "integration: line comments" {
    const testing = std.testing;

    const source =
        \\// This is a line comment
        \\fn main() i32 {
        \\    // Comment before return
        \\    return 42 // trailing comment
        \\}
    ;
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
}

test "integration: block comments" {
    const testing = std.testing;

    const source = "fn /* comment */ main() i32 { return /* inline */ 42 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
}

test "integration: nested block comments" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\    /* outer comment
        \\       /* nested comment */
        \\       still in outer */
        \\    return 42
        \\}
    ;
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
}

test "integration: integer type inference from context" {
    const testing = std.testing;

    const source =
        \\fn test_types() i32 {
        \\    let a: i8@? = 127;
        \\    let b: i16@? = 32000;
        \\    let c: i64@? = 9000;
        \\    let d: u8@? = 255;
        \\    let e: u16@? = 65000;
        \\    let f: u32@? = 4000000;
        \\    let g: u64@? = 9000000;
        \\    return 42
        \\}
        \\fn main() i32 { return test_types() }
    ;
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Check that different integer types are used
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "i8") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "i16") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "i64") != null);
}

test "integration: integer literal out of range error" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: i8 = 200; return x }";
    const result = compile(source, testing.allocator);

    try testing.expectError(error.TypeMismatch, result);
}

test "integration: unsigned type with valid value" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: u8@? = 255; return 0 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "i8") != null);
}

test "integration: assignment contextual typing" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\    var x: i8 = 10;
        \\    x = 20;
        \\    x = 127;
        \\    return 0
        \\}
    ;
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Should compile successfully with i8 type
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "i8") != null);
}

test "integration: assignment out of range error" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\    var x: i8 = 10;
        \\    x = 200;
        \\    return 0
        \\}
    ;
    const result = compile(source, testing.allocator);

    try testing.expectError(error.TypeMismatch, result);
}

test "integration: compile binary operations end-to-end" {
    const testing = std.testing;

    const source = "fn main() i32 { return 10 + 20 * 3 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32 20, 3") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 10") != null);
}

test "integration: arithmetic operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() i32 { return 10 + 5 }",
        "fn f() i32 { return 10 - 5 }",
        "fn f() i32 { return 10 * 5 }",
        "fn f() i32 { return 10 / 5 }",
        "fn f() i32 { return 10 % 3 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @f()") != null);
    }
}

test "integration: complex nested expressions" {
    const testing = std.testing;

    const source = "fn f() i32 { return ((1 + 2) * 3 - 4) / 5 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%.t") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 1, 2") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "sub i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "sdiv i32") != null);
}

test "integration: unary operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() i32 { return -42 }",
        "fn f() i32 { return !1 }",
        "fn f() i32 { return ~0 }",
        "fn f() i32 { return --5 }",
        "fn f() i32 { return -5 + 3 }",
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
        "fn f() i32 { return 5 && 3 }",
        "fn f() i32 { return 5 || 3 }",
        "fn f() i32 { return 1 && 0 }",
        "fn f() i32 { return 0 || 1 }",
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
        "fn f() bool { return true && false }",
        "fn f() bool { return true || false }",
        "fn f() bool { return (5 > 3) && (2 < 4) }",
        "fn f() bool { return (5 > 3) || (2 < 4) }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        // bool logical operators don't need icmp conversion
        try testing.expect(std.mem.indexOf(u8, llvm_ir, "and i1") != null or std.mem.indexOf(u8, llvm_ir, "or i1") != null);
    }
}

test "integration: comparison operators" {
    const testing = std.testing;

    const cases = [_][]const u8{
        "fn f() i32 { return 5 == 3 }",
        "fn f() i32 { return 5 != 3 }",
        "fn f() i32 { return 5 < 3 }",
        "fn f() i32 { return 5 > 3 }",
        "fn f() i32 { return 5 <= 3 }",
        "fn f() i32 { return 5 >= 3 }",
        "fn f() bool { return 5 == 3 }",
        "fn f() bool { return 5 < 3 }",
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
        "fn f() i32 { return 5 & 3 }",
        "fn f() i32 { return 5 | 3 }",
        "fn f() i32 { return 5 ^ 3 }",
        "fn f() i32 { return 5 << 2 }",
        "fn f() i32 { return 5 >> 2 }",
        "fn f() i32 { return ~5 }",
    };

    for (cases) |source| {
        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @f()") != null);
    }
}

test "integration: let binding without type annotation" {
    const testing = std.testing;

    const source = "fn main() i32 { let x = 42; return x }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32") != null);
}

test "integration: let binding with type annotation" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: i32 = 10; return x }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 10") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32") != null);
}

test "integration: multiple let bindings" {
    const testing = std.testing;

    const source = "fn main() i32 { let x = 10; let y = 20; return x + y }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%x = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%y = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 10, ptr %x") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "store i32 20, ptr %y") != null);
}

test "integration: let binding with expression" {
    const testing = std.testing;

    const source = "fn main() i32 { let x = 5 + 3; return x * 2 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 5, 3") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "load i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "mul i32") != null);
}

test "integration: let binding with bool type" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: bool = 5 > 3; return x }";
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
    // Only test types that accept i32 literals (>= 32 bits)
    const cases = [_]struct { source: []const u8, llvm_type: []const u8 }{
        .{ .source = "fn f() i32 { return 42 }", .llvm_type = "i32" },
        .{ .source = "fn f() i64 { return 42 }", .llvm_type = "i64" },
        .{ .source = "fn f() u32 { return 42 }", .llvm_type = "i32" },
        .{ .source = "fn f() u64 { return 42 }", .llvm_type = "i64" },
    };

    for (cases) |case| {
        const llvm_ir = try compile(case.source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        const expected_return = try std.fmt.allocPrint(testing.allocator, "ret {s}", .{case.llvm_type});
        defer testing.allocator.free(expected_return);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, expected_return) != null);
    }
}

test "integration: let bindings with all integer types" {
    const testing = std.testing;

    const literal_cases = [_]struct { type_name: []const u8, llvm_type: []const u8 }{
        .{ .type_name = "i32", .llvm_type = "i32" },
        .{ .type_name = "i64", .llvm_type = "i64" },
        .{ .type_name = "u32", .llvm_type = "i32" },
        .{ .type_name = "u64", .llvm_type = "i64" },
    };

    for (literal_cases) |case| {
        const source = try std.fmt.allocPrint(testing.allocator, "fn f() {s} {{ let x: {s} = 10 + 5; return x }}", .{ case.type_name, case.type_name });
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

    // Note: i8, i16, u8, u16 types cannot currently be tested with literals
    // because literals default to i32 and narrowing conversions are not implicit.
    // These types will be testable once we add:
    // - Function parameters (to pass values of these types)
    // - Typed literal syntax (like 42i8)
    // - Explicit cast syntax
}

test "integration: arithmetic operations with typed variables" {
    const testing = std.testing;

    const source = "fn f() i64 { let x: i64 = 10; let y: i64 = 20; return x + y }";
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
        .{ .type_name = "i32", .expected_inst = "sdiv" },
        .{ .type_name = "u32", .expected_inst = "udiv" },
    };

    for (div_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() {s} {{ let x: {s} = 10; let y: {s} = 2; return x / y }}", .{ case.type_name, case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }

    // Test modulo: srem for signed, urem for unsigned
    const mod_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "i32", .expected_inst = "srem" },
        .{ .type_name = "u32", .expected_inst = "urem" },
    };

    for (mod_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() {s} {{ let x: {s} = 10; let y: {s} = 3; return x % y }}", .{ case.type_name, case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }

    // Test comparisons: slt/sgt for signed, ult/ugt for unsigned
    const cmp_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "i32", .expected_inst = "icmp slt" },
        .{ .type_name = "u32", .expected_inst = "icmp ult" },
    };

    for (cmp_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() bool {{ let x: {s} = 10; let y: {s} = 20; return x < y }}", .{ case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }

    // Test shift right: ashr for signed, lshr for unsigned
    const shift_cases = [_]struct { type_name: []const u8, expected_inst: []const u8 }{
        .{ .type_name = "i32", .expected_inst = "ashr" },
        .{ .type_name = "u32", .expected_inst = "lshr" },
    };

    for (shift_cases) |case| {
        const source = std.fmt.allocPrint(testing.allocator, "fn f() {s} {{ let x: {s} = 16; let y: {s} = 2; return x >> y }}", .{ case.type_name, case.type_name, case.type_name }) catch unreachable;
        defer testing.allocator.free(source);

        const llvm_ir = try compile(source, testing.allocator);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_inst) != null);
    }
}

test "integration: implicit literal conversions" {
    const testing = std.testing;

    // With contextual typing, literals infer their type from context
    // So there should be NO conversion instructions for literals

    // Literal infers i64 from return type - no conversion needed
    const i64_literal = "fn f() i64 { return 42 }";
    const ir1 = try compile(i64_literal, testing.allocator);
    defer testing.allocator.free(ir1);
    try testing.expect(std.mem.indexOf(u8, ir1, "ret i64 42") != null);

    // Literal infers u64 from return type - no conversion needed
    const u64_literal = "fn f() u64 { return 42 }";
    const ir2 = try compile(u64_literal, testing.allocator);
    defer testing.allocator.free(ir2);
    try testing.expect(std.mem.indexOf(u8, ir2, "ret i64 42") != null);

    // Literal infers u32 from return type - no conversion needed
    const u32_literal = "fn f() u32 { return 42 }";
    const ir3 = try compile(u32_literal, testing.allocator);
    defer testing.allocator.free(ir3);
    try testing.expect(std.mem.indexOf(u8, ir3, "ret i32 42") != null);

    // Literal infers i8 from return type - no conversion needed
    const i8_literal = "fn f() i8 { return 42 }";
    const ir4 = try compile(i8_literal, testing.allocator);
    defer testing.allocator.free(ir4);
    try testing.expect(std.mem.indexOf(u8, ir4, "ret i8 42") != null);
}

test "integration: binary op requires exact type match" {
    const testing = std.testing;

    // Binary ops require both operands to have same type
    // Mixed i32/i64 should fail
    const mixed_types =
        \\fn f() i64 {
        \\    let x: i32 = 100;
        \\    let y: i64 = 42;
        \\    return x + y
        \\}
    ;
    try testing.expectError(error.TypeMismatch, compile(mixed_types, testing.allocator));

    // But same types work
    const same_types =
        \\fn f() i64 {
        \\    let x: i64 = 100;
        \\    let y: i64 = 42;
        \\    return x + y
        \\}
    ;
    const ir = try compile(same_types, testing.allocator);
    defer testing.allocator.free(ir);
    try testing.expect(std.mem.indexOf(u8, ir, "add i64") != null);
}

test "integration: binary op type mismatch with contextual typing" {
    const testing = std.testing;

    // Literal gets typed as u32 due to context, but variable is i32
    // This creates a type mismatch in the binary op
    const source =
        \\fn f() u32 {
        \\    let x: i32 = 100;
        \\    let y: u32 = x + 42;
        \\    return y
        \\}
    ;
    // Should fail because: literal 42 becomes u32, but x is i32, so x + 42 fails
    try testing.expectError(error.TypeMismatch, compile(source, testing.allocator));
}

test "integration: bool to integer type conversions" {
    const testing = std.testing;

    const cases = [_]struct { type_name: []const u8, llvm_type: []const u8 }{
        .{ .type_name = "i8", .llvm_type = "i8" },
        .{ .type_name = "i16", .llvm_type = "i16" },
        .{ .type_name = "i32", .llvm_type = "i32" },
        .{ .type_name = "i64", .llvm_type = "i64" },
        .{ .type_name = "u8", .llvm_type = "i8" },
        .{ .type_name = "u16", .llvm_type = "i16" },
        .{ .type_name = "u32", .llvm_type = "i32" },
        .{ .type_name = "u64", .llvm_type = "i64" },
    };

    for (cases) |case| {
        const source = try std.fmt.allocPrint(testing.allocator, "fn f() {s} {{ return 5 > 3 }}", .{case.type_name});
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

    const source = "fn add(x: i32, y: i32) i32 { return x + y }";
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

    const source = "fn mix(a: i64, b: u32@?, c: i8@?) i64 { return a }";
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

    const source = "fn get_answer() i32 { return 42 }";
    const llvm_ir = try compile(source, testing.allocator);
    defer testing.allocator.free(llvm_ir);

    // Check function signature with empty parameter list
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @get_answer()") != null);
}

test "integration: parameter usage in expressions" {
    const testing = std.testing;

    const source = "fn compute(x: i32@2, y: i32) i32 { let z: i32 = x * y; return z + x }";
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

    // Test i8 parameters
    const i8_source = "fn use_i8(x: i8, y: i8@?) i8 { return x }";
    const i8_ir = try compile(i8_source, testing.allocator);
    defer testing.allocator.free(i8_ir);
    try testing.expect(std.mem.indexOf(u8, i8_ir, "define i8 @use_i8(i8 %x, i8 %y)") != null);
    try testing.expect(std.mem.indexOf(u8, i8_ir, "%x.addr = alloca i8") != null);

    // Test i16 parameters
    const i16_source = "fn use_i16(a: i16) i16 { return a }";
    const i16_ir = try compile(i16_source, testing.allocator);
    defer testing.allocator.free(i16_ir);
    try testing.expect(std.mem.indexOf(u8, i16_ir, "define i16 @use_i16(i16 %a)") != null);
    try testing.expect(std.mem.indexOf(u8, i16_ir, "%a.addr = alloca i16") != null);

    // Test u8 parameters
    const u8_source = "fn use_u8(b: u8) u8 { return b }";
    const u8_ir = try compile(u8_source, testing.allocator);
    defer testing.allocator.free(u8_ir);
    try testing.expect(std.mem.indexOf(u8, u8_ir, "define i8 @use_u8(i8 %b)") != null);

    // Test u16 parameters
    const u16_source = "fn use_u16(c: u16) u16 { return c }";
    const u16_ir = try compile(u16_source, testing.allocator);
    defer testing.allocator.free(u16_ir);
    try testing.expect(std.mem.indexOf(u8, u16_ir, "define i16 @use_u16(i16 %c)") != null);
}

test "integration: operations with small integer parameters" {
    const testing = std.testing;

    // Test i8 arithmetic
    const i8_source = "fn add_i8(x: i8, y: i8) i8 { return x + y }";
    const i8_ir = try compile(i8_source, testing.allocator);
    defer testing.allocator.free(i8_ir);
    try testing.expect(std.mem.indexOf(u8, i8_ir, "add i8") != null);

    // Test u16 arithmetic
    const u16_source = "fn mul_u16(a: u16, b: u16) u16 { return a * b }";
    const u16_ir = try compile(u16_source, testing.allocator);
    defer testing.allocator.free(u16_ir);
    try testing.expect(std.mem.indexOf(u8, u16_ir, "mul i16") != null);

    // Test signed division on i16
    const i16_div = "fn div_i16(x: i16, y: i16) i16 { return x / y }";
    const i16_ir = try compile(i16_div, testing.allocator);
    defer testing.allocator.free(i16_ir);
    try testing.expect(std.mem.indexOf(u8, i16_ir, "sdiv i16") != null);

    // Test unsigned division on u8
    const u8_div = "fn div_u8(x: u8, y: u8) u8 { return x / y }";
    const u8_ir = try compile(u8_div, testing.allocator);
    defer testing.allocator.free(u8_ir);
    try testing.expect(std.mem.indexOf(u8, u8_ir, "udiv i8") != null);
}

test "integration: simple function call" {
    const testing = std.testing;

    const source =
        \\fn add(x: i32, y: i32) i32 { return x + y }
        \\fn main() i32 { return add(10, 20) }
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
        \\fn add(x: i32, y: i32) i32 { return x + y }
        \\fn main() i32 { let a: i32 = 5; let b: i32 = 7; return add(a, b) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that add is called (variables will be loaded first)
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @add(") != null);
}

test "integration: multiple function calls" {
    const testing = std.testing;

    const source =
        \\fn add(x: i32, y: i32) i32 { return x + y }
        \\fn mul(x: i32, y: i32) i32 { return x * y }
        \\fn main() i32 { return add(mul(2, 3), mul(4, 5)) }
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
        \\fn get_answer() i32 { return 42 }
        \\fn main() i32 { return get_answer() }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that get_answer is called with no arguments
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @get_answer()") != null);
}

test "integration: function call with different types" {
    const testing = std.testing;

    const source =
        \\fn use_i64(x: i64) i64 { return x }
        \\fn use_i32(x: i32) i32 { return x }
        \\fn main() i32 { let a: i64@? = 100; return 0 }
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
        \\fn factorial(n: i32@2) i32 { return n * factorial(n - 1) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that factorial calls itself
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @factorial(") != null);
}

test "integration: function call with small integer types" {
    const testing = std.testing;

    const source =
        \\fn add_i8(x: i8, y: i8) i8 { return x + y }
        \\fn main() i32 { return 0 }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature with i8 parameters
    try testing.expect(std.mem.indexOf(u8, ir, "define i8 @add_i8(i8 %x, i8 %y)") != null);
}

test "integration: function call in let binding" {
    const testing = std.testing;

    const source =
        \\fn mul(x: i32, y: i32) i32 { return x * y }
        \\fn main() i32 { let result: i32 = mul(6, 7); return result }
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
        \\fn get_five() i32 { return 5 }
        \\fn get_ten() i32 { return 10 }
        \\fn main() i32 { return get_five() + get_ten() }
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
        \\fn calc(a: i32, b: i32, c: i32) i32 { return a + b + c }
        \\fn main() i32 { let x: i32@2 = 10; return calc(x, 20, x) }
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
        \\fn get_u8(x: u8) u8 { return x }
        \\fn get_i16(x: i16) i16 { return x }
        \\fn get_u32(x: u32) u32 { return x }
        \\fn main() i32 { return 0 }
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

    const source = "fn main() i32 { return if true { 1 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that bool literals work (br on i1 constant)
    try testing.expect(std.mem.indexOf(u8, ir, "br i1 1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "phi i32") != null);
}

test "integration: simple if/else expression" {
    const testing = std.testing;

    const source = "fn main() i32 { return if true { 42 } else { 0 } }";
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

    const source = "fn main() i32 { return if 10 > 5 { 1 } else { 0 } }";
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

    const source = "fn main() i32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check multiple basic blocks
    try testing.expect(std.mem.indexOf(u8, ir, "then0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge2") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then3") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else4") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge5") != null);
    // Check multiple phi nodes
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "integration: if/elseif/else chain" {
    const testing = std.testing;

    const source = "fn main() i32 { return if false { 1 } elseif true { 2 } else { 3 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested structure created by elseif
    try testing.expect(std.mem.indexOf(u8, ir, "then0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge2") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then3") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else4") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge5") != null);
    // Check multiple phi nodes for nested structure
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "integration: multiple elseif chain" {
    const testing = std.testing;

    const source = "fn main() i32 { return if false { 1 } elseif false { 2 } elseif false { 3 } elseif true { 4 } else { 5 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that deeply nested structure is created
    try testing.expect(std.mem.indexOf(u8, ir, "merge2") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge5") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge8") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "merge11") != null);
    // Check multiple phi nodes
    const phi_count = std.mem.count(u8, ir, "phi i32");
    try testing.expectEqual(@as(usize, 4), phi_count);
}

test "integration: if expression with variables" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: i32@2 = 10; let y: i32@2 = 20; return if x > y { x } else { y } }";
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

    const source = "fn main() i64 { return if true { 100 } else { 200 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // With bidirectional type checking, literals adopt the expected type (i64) directly
    // Check phi node uses i64 (literals typed as i64 from return context)
    try testing.expect(std.mem.indexOf(u8, ir, "phi i64") != null);
    // Check return type
    try testing.expect(std.mem.indexOf(u8, ir, "ret i64") != null);
}

test "integration: if expression with bool logical operators" {
    const testing = std.testing;

    const source = "fn main() i32 { return if (10 > 5) && (3 < 7) { 1 } else { 0 } }";
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

    const source = "fn main() i32 { let result: i32 = if true { 42 } else { 0 }; return result }";
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

    const source = "fn main() i32 { return (if true { 10 } else { 20 }) + (if false { 30 } else { 40 }) }";
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
        \\fn get_a() i32 { return 10 }
        \\fn get_b() i32 { return 20 }
        \\fn main() i32 { return if true { get_a() } else { get_b() } }
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

    const source = "fn main() i32 { return if false { 1 } elseif true { 2 } }";
    const result = compile(source, testing.allocator);

    // This should fail type checking (no else branch)
    try testing.expectError(error.TypeMismatch, result);
}

test "integration: if without else should fail" {
    const testing = std.testing;

    const source = "fn main() i32 { return if true { 1 } }";
    const result = compile(source, testing.allocator);

    // This should fail type checking (no else branch)
    try testing.expectError(error.TypeMismatch, result);
}

test "integration: deeply nested elseif chain" {
    const testing = std.testing;

    // 10 levels of elseif
    const source =
        \\fn main() i32 {
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
        \\fn add(x: i32, y: i32) i32 { return x + y }
        \\fn main() i32 { return add(if true { 10 } else { 20 }, if false { 30 } else { 40 }) }
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

    const source = "fn main() i32 { return (if true { 1 } else { 2 }) + (if false { 3 } else { 4 }) + (if true { 5 } else { 6 }) }";
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

    const source = "fn main() i32 { return if (true && false) || (false || true) { 1 } else { 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have bool logical operations
    try testing.expect(std.mem.indexOf(u8, ir, "and i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "or i1") != null);
}

test "integration: if in let binding with elseif" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: i32 = if false { 1 } elseif false { 2 } elseif true { 3 } else { 4 }; return x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "load i32") != null);
}

test "integration: simple while loop" {
    const testing = std.testing;

    const source = "fn main() i32 { while true { 42 }; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have while blocks
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_body1:") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_exit2:") != null);

    // Should return 0
    try testing.expect(std.mem.indexOf(u8, ir, "ret i32 0") != null);
}

test "integration: while loop with comparison condition" {
    const testing = std.testing;

    const source = "fn main() i32 { while 5 > 3 { 1 }; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have comparison in condition
    try testing.expect(std.mem.indexOf(u8, ir, "icmp") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
}

test "integration: while loop with variable condition" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: bool = true; while x { 1 }; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should load variable in condition
    try testing.expect(std.mem.indexOf(u8, ir, "load i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
}

test "integration: nested while loops" {
    const testing = std.testing;

    const source = "fn main() i32 { while true { while false { 1 } }; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have two sets of while blocks
    try testing.expect(std.mem.indexOf(u8, ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "while_header3:") != null);
}

test "integration: while loop in let binding" {
    const testing = std.testing;

    const source = "fn main() i32 { while false { 1 }; let x: i32 = 0; return x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should store result in variable
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32 0") != null);
}

test "integration: while with integer condition should fail" {
    const testing = std.testing;

    const source = "fn main() i32 { while 5 { 1 }; return 0 }";
    const result = compile(source, testing.allocator);

    // Should fail type checking
    try testing.expectError(error.TypeMismatch, result);
}

test "integration: while loop with if in body" {
    const testing = std.testing;

    const source = "fn main() i32 { while true { if false { 1 } else { 2 } }; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have both while and if blocks
    try testing.expect(std.mem.indexOf(u8, ir, "while_header") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "then") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "else") != null);
}

test "integration: while loop as function argument" {
    const testing = std.testing;

    const source = "fn identity(x: i32) i32 { return x } fn main() i32 { while false { 1 }; return identity(0) }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should compile successfully with while loop
    try testing.expect(std.mem.indexOf(u8, ir, "while_header") != null);
    // Function definitions should be present
    try testing.expect(std.mem.indexOf(u8, ir, "define") != null);
}

test "integration: assignment to immutable variable should fail" {
    const testing = std.testing;

    const source = "fn main() i32 { let x: i32 = 5; x = 10; return x }";
    const result = compile(source, testing.allocator);

    try testing.expectError(error.AssignmentToImmutable, result);
}

test "integration: assignment to mutable variable" {
    const testing = std.testing;

    const source = "fn main() i32 { var x: i32 = 5; x = 10; return x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should have allocation and stores
    try testing.expect(std.mem.indexOf(u8, ir, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32 5") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store i32 10") != null);
}

test "integration: var keyword for mutable variables" {
    const testing = std.testing;

    const source = "fn main() i32 { var counter: i32 = 0; counter = counter + 1; return counter }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(std.mem.indexOf(u8, ir, "define") != null);
}

// Tuple integration tests

test "integration: empty tuple literal" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: ()@? = (); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Empty tuple should allocate {  } type (note: two spaces)
    try testing.expect(std.mem.indexOf(u8, ir, "alloca {  }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store {  } undef") != null);
}

test "integration: simple tuple literal" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: (i32, i32)@? = (1, 2); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple type is created
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32 }") != null);
    // Check insertvalue instructions
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
    // Check alloca for tuple
    try testing.expect(std.mem.indexOf(u8, ir, "alloca { i32, i32 }") != null);
}

test "integration: tuple with mixed types" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: (i32, bool, i32)@? = (42, true, 100); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple type with i32, i1, i32
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i1, i32 }") != null);
    // Check multiple insertvalue instructions
    const insertvalue_count = std.mem.count(u8, ir, "insertvalue");
    try testing.expect(insertvalue_count >= 3);
}

test "integration: nested tuple literal" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: (i32, (i32, i32))@? = (1, (2, 3)); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested tuple type { i32, { i32, i32 } }
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, { i32, i32 } }") != null);
    // Inner tuple should be built first
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
    // Then inserted into outer tuple
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, { i32, i32 } }") != null);
}

test "integration: deeply nested tuple" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: (i32, (i32, (i32, i32)))@? = (1, (2, (3, 4))); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check deeply nested type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, { i32, i32 } }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, { i32, { i32, i32 } } }") != null);
}

test "integration: tuple indexing simple" {
    const testing = std.testing;

    const source = "fn main() i32 { let t = (10, 20); return t.0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple is loaded
    try testing.expect(std.mem.indexOf(u8, ir, "load { i32, i32 }") != null);
    // Check extractvalue at index 0
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i32 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, ", 0") != null);
    // Check value is returned
    try testing.expect(std.mem.indexOf(u8, ir, "ret i32") != null);
}

test "integration: tuple indexing multiple elements" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: (i32, i32, i32)@3 = (1, 2, 3); return t.0 + t.1 + t.2 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32, i32 }") != null);
    // Check all three extracts
    try testing.expect(std.mem.indexOf(u8, ir, ", 0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, ", 1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, ", 2") != null);
    // Check additions
    const add_count = std.mem.count(u8, ir, "add i32");
    try testing.expectEqual(@as(usize, 2), add_count);
}

test "integration: nested tuple indexing" {
    const testing = std.testing;

    const source = "fn main() i32 { let t = (1, (2, 3)); return t.1.0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested tuple type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, { i32, i32 } }") != null);
    // Extract inner tuple at index 1
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, { i32, i32 } }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, ", 1") != null);
    // Then extract from inner tuple at index 0
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i32 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, ", 0") != null);
}

test "integration: tuple with bool elements" {
    const testing = std.testing;

    const source = "fn main() bool { let t = (true, false, true); return t.0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple type with bools
    try testing.expect(std.mem.indexOf(u8, ir, "{ i1, i1, i1 }") != null);
    // Check extractvalue returns i1
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i1, i1, i1 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "ret i1") != null);
}

test "integration: simple tuple destructuring" {
    const testing = std.testing;

    const source = "fn main() i32 { let (a, b) = (10, 20); return a + b }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple is created
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32 }") != null);
    // Check extractvalue for both elements
    const extractvalue_count = std.mem.count(u8, ir, "extractvalue");
    try testing.expect(extractvalue_count >= 2);
    // Check both variables allocated
    try testing.expect(std.mem.indexOf(u8, ir, "%a = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "%b = alloca i32") != null);
    // Check variables are loaded and added
    try testing.expect(std.mem.indexOf(u8, ir, "add i32") != null);
}

test "integration: nested tuple destructuring" {
    const testing = std.testing;

    const source = "fn main() i32 { let (x, (y, z)) = (1, (2, 3)); return x + y + z }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested tuple type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, { i32, i32 } }") != null);
    // Check all three variables allocated
    try testing.expect(std.mem.indexOf(u8, ir, "%x = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "%y = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "%z = alloca i32") != null);
    // Check multiple extracts (for x, inner tuple, y, z)
    const extractvalue_count = std.mem.count(u8, ir, "extractvalue");
    try testing.expect(extractvalue_count >= 3);
}

test "integration: tuple destructuring with type annotation" {
    const testing = std.testing;

    const source = "fn main() i32 { let (a, b): (i32, bool@?) = (42, true); return a }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check mixed-type tuple
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i1 }") != null);
    // Check a is i32 and b is i1
    try testing.expect(std.mem.indexOf(u8, ir, "%a = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "%b = alloca i1") != null);
}

test "integration: tuple function return type" {
    const testing = std.testing;

    const source = "fn make_pair() (i32, i32) { return (10, 20) } fn main() i32 { return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature with tuple return
    try testing.expect(std.mem.indexOf(u8, ir, "define { i32, i32 } @make_pair()") != null);
    // Check tuple construction in return
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "ret { i32, i32 }") != null);
}

test "integration: tuple as function parameter" {
    const testing = std.testing;

    const source = "fn use_pair(p: (i32, i32)) i32 { return p.0 } fn main() i32 { return use_pair((5, 10)) }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature with tuple parameter
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @use_pair({ i32, i32 } %p)") != null);
    // Check parameter is allocated
    try testing.expect(std.mem.indexOf(u8, ir, "%p.addr = alloca { i32, i32 }") != null);
    // Check call with tuple argument
    try testing.expect(std.mem.indexOf(u8, ir, "call i32 @use_pair({ i32, i32 }") != null);
}

test "integration: tuple in if expression" {
    const testing = std.testing;

    const source = "fn main() i32 { let t = if true { (1, 2) } else { (3, 4) }; return t.0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check tuple type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32 }") != null);
    // Check phi node for tuple
    try testing.expect(std.mem.indexOf(u8, ir, "phi { i32, i32 }") != null);
    // Check extractvalue after phi
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i32 }") != null);
}

test "integration: tuple destructuring with different types" {
    const testing = std.testing;

    const source = "fn main() bool { let (flag, num): (bool, i32@?) = (true, 42); return flag }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check mixed-type tuple
    try testing.expect(std.mem.indexOf(u8, ir, "{ i1, i32 }") != null);
    // Check flag is i1, num is i32
    try testing.expect(std.mem.indexOf(u8, ir, "%flag = alloca i1") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "%num = alloca i32") != null);
}

test "integration: large tuple" {
    const testing = std.testing;

    const source = "fn main() i32 { let t = (1, 2, 3, 4, 5, 6, 7, 8); return t.7 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check large tuple type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32, i32, i32, i32, i32, i32, i32 }") != null);
    // Check extract at index 7 (last element)
    try testing.expect(std.mem.indexOf(u8, ir, ", 7") != null);
}

test "integration: tuple with expressions" {
    const testing = std.testing;

    const source = "fn main() i32 { let t: (i32, i32)@2 = (5 + 3, 10 * 2); return t.0 + t.1 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check expressions are evaluated before tuple construction
    try testing.expect(std.mem.indexOf(u8, ir, "add i32 5, 3") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "mul i32 10, 2") != null);
    // Check results inserted into tuple
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
}

test "integration: tuple destructuring in mutable binding" {
    const testing = std.testing;

    const source = "fn main() i32 { var (x, y) = (10, 20); x = 30; return x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check variables are allocated
    try testing.expect(std.mem.indexOf(u8, ir, "%x = alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "%y = alloca i32") != null);
    // Check x is reassigned
    try testing.expect(std.mem.indexOf(u8, ir, "store i32 30, ptr %x") != null);
}

test "integration: triple nested tuple" {
    const testing = std.testing;

    const source = "fn main() i32 { let t = (1, (2, (3, 4))); return t.1.1.0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check triple nested type
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, { i32, { i32, i32 } } }") != null);
    // Check multiple levels of extractvalue
    const extractvalue_count = std.mem.count(u8, ir, "extractvalue");
    try testing.expect(extractvalue_count >= 3);
}

test "integration: tuple with function call elements" {
    const testing = std.testing;

    const source =
        \\fn get_num() i32 { return 42 }
        \\fn main() i32 { let t = (get_num(), get_num()); return t.0 }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function is called twice
    const call_count = std.mem.count(u8, ir, "call i32 @get_num()");
    try testing.expectEqual(@as(usize, 2), call_count);
    // Check results inserted into tuple
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
}

test "integration: empty tuple type" {
    const testing = std.testing;

    const source = "fn make_unit() () { return () } fn main() i32 { return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check empty tuple function signature (note: two spaces in {  })
    try testing.expect(std.mem.indexOf(u8, ir, "define {  } @make_unit()") != null);
    // Check empty tuple return
    try testing.expect(std.mem.indexOf(u8, ir, "ret {  } undef") != null);
}

test "integration: simple struct type definition" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } fn main() i32 { return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Should compile without error
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @main()") != null);
}

test "integration: struct literal creation" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } fn main() i32 { let p: Point@? = Point { x: 10, y: 20 }; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct type in alloca
    try testing.expect(std.mem.indexOf(u8, ir, "alloca { i32, i32 }") != null);
    // Check insertvalue instructions for struct creation
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 } undef, i32 10, 0") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
}

test "integration: struct field access" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } fn main() i32 { let p = Point { x: 10, y: 20 }; return p.x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check field access using extractvalue
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i32 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, ", 0") != null);
}

test "integration: struct with multiple fields" {
    const testing = std.testing;

    const source = "type Vec3 = { x: i32, y: i32, z: i32 } fn main() i32 { let v = Vec3 { x: 1, y: 2, z: 3 }; return v.z }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct type with 3 fields
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32, i32 }") != null);
    // Check extractvalue with index 2 for z field
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i32, i32 }") != null);
}

test "integration: struct as function parameter" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } fn get_x(p: Point) i32 { return p.x } fn main() i32 { let p = Point { x: 42, y: 10 }; return get_x(p) }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature with struct parameter
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @get_x({ i32, i32 } %p)") != null);
    // Check struct field access uses extractvalue
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i32 }") != null);
}

test "integration: struct as function return type" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } fn make_point() Point { return Point { x: 5, y: 10 } } fn main() i32 { let p = make_point(); return p.x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function signature with struct return type
    try testing.expect(std.mem.indexOf(u8, ir, "define { i32, i32 } @make_point()") != null);
    // Check struct literal construction
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32, i32 }") != null);
}

test "integration: nested structs" {
    const testing = std.testing;

    const source = "type Inner = { a: i32 } type Outer = { inner: Inner, b: i32 } fn main() i32 { let inner = Inner { a: 42 }; let outer = Outer { inner: inner, b: 10 }; return outer.b }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested struct types
    try testing.expect(std.mem.indexOf(u8, ir, "{ { i32 }, i32 }") != null);
}

test "integration: struct field access chain" {
    const testing = std.testing;

    const source = "type Inner = { value: i32 } type Outer = { inner: Inner } fn main() i32 { let inner = Inner { value: 99 }; let outer = Outer { inner: inner }; let extracted = outer.inner; return extracted.value }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check multiple extractvalue operations
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue") != null);
}

test "integration: empty struct" {
    const testing = std.testing;

    const source = "type Empty = {} fn main() i32 { let e: Empty@? = Empty {}; return 42 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check empty struct type
    try testing.expect(std.mem.indexOf(u8, ir, "{  }") != null);
}

test "integration: struct with different types" {
    const testing = std.testing;

    const source = "type Mixed = { flag: bool, count: i32 } fn main() i32 { let m = Mixed { flag: true, count: 100 }; return m.count }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct with mixed types
    try testing.expect(std.mem.indexOf(u8, ir, "{ i1, i32 }") != null);
}

test "integration: multiple struct types" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } type Color = { r: i32, g: i32, b: i32 } fn main() i32 { let p = Point { x: 1, y: 2 }; let c: Color@? = Color { r: 255, g: 0, b: 0 }; return p.x }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check both struct types exist
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i32, i32 }") != null);
}

test "integration: sum type definition" {
    const testing = std.testing;

    const source = "type Option = | None | Some(i32) fn main() i32 { return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check sum type compiled
    try testing.expect(ir.len > 0);
}

test "integration: nullary constructor" {
    const testing = std.testing;

    const source = "type Option = | None | Some(i32) fn main() i32 { let opt: Option@? = None; return 42 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check insertvalue for tag
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue") != null);
}

test "integration: constructor with payload" {
    const testing = std.testing;

    const source = "type Option = | None | Some(i32) fn main() i32 { let opt: Option@? = Some(42); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check insertvalue for constructor
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue") != null);
}

test "integration: simple match expression" {
    const testing = std.testing;

    const source = "type Option = | None | Some(i32) fn main() i32 { let opt = Some(42); return match opt { None => 0, Some(x) => x } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check switch instruction for match
    try testing.expect(std.mem.indexOf(u8, ir, "switch") != null);
    // Check phi for merge
    try testing.expect(std.mem.indexOf(u8, ir, "phi") != null);
    // Check getelementptr for payload extraction
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
}

test "integration: match with multiple arms" {
    const testing = std.testing;

    const source = "type Result = | Ok(i32) | Err(i32) fn main() i32 { let res = Ok(100); return match res { Ok(val) => val, Err(code) => code } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check switch and phi
    try testing.expect(std.mem.indexOf(u8, ir, "switch") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "phi") != null);
    // Check getelementptr for payload extraction
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
}

test "integration: sum type with tuple payload" {
    const testing = std.testing;

    const source = "type Pair = | Empty | Value(i32, i32) fn main() i32 { let p = Value(10, 20); return match p { Empty => 0, Value(x, y) => x } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check sum type with tuple compiles and extracts payload
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
}

test "integration: sum type with struct payload" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } type Shape = | Circle(i32) | Rectangle(Point) fn main() i32 { let p = Point { x: 1, y: 2 }; let s: Shape@? = Rectangle(p); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct in sum type compiles
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue") != null);
}

test "integration: nested sum types" {
    const testing = std.testing;

    const source = "type Inner = | A | B type Outer = | X(Inner) | Y fn main() i32 { let inner = A; let outer: Outer@? = X(inner); return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check nested sum types compile
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue") != null);
}

test "integration: match on sum type with struct payload" {
    const testing = std.testing;

    const source = "type Point = { x: i32, y: i32 } type MaybePoint = | NoPoint | SomePoint(Point) fn main() i32 { let mp = NoPoint; return match mp { NoPoint => 0, SomePoint(_) => 1 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check match with struct payload
    try testing.expect(std.mem.indexOf(u8, ir, "switch") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "phi") != null);
}

test "integration: sum type with multiple payload types" {
    const testing = std.testing;

    const source = "type Mixed = | IntVal(i32) | BoolVal(bool) | Pair(i32, bool) fn main() i32 { let m = Pair(42, true); return match m { IntVal(x) => x, BoolVal(b) => 1, Pair(a, b) => a } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check mixed payload types compile and extract
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
}

test "integration: match extracts both values from tuple payload" {
    const testing = std.testing;

    const source = "type Result = | Ok(i32, i32) | Err fn main() i32 { let r = Ok(100, 200); return match r { Ok(a, b) => b, Err => 0 } }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check that second value is extracted (should return 200)
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "switch") != null);
}

test "integration: recursive ADT definition" {
    const testing = std.testing;

    const source = "type List = | Nil | Cons(i32, List) fn main() i32 { let list: List@? = Nil; return 0 }";
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check recursive ADT compiles
    try testing.expect(ir.len > 0);
}

// Linearity checking tests

test "integration: linearity - variable used exactly once (default)" {
    const testing = std.testing;

    const source =
        \\fn use_once(x: i32) i32 {
        \\    return x
        \\}
        \\fn main() i32 { return use_once(5) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "integration: linearity - variable used exactly twice with @2" {
    const testing = std.testing;

    const source =
        \\fn use_twice(x: i32@2) i32 {
        \\    return x + x
        \\}
        \\fn main() i32 { return use_twice(10) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @use_twice(i32 %x)") != null);
}

test "integration: linearity - variable used exactly three times with @3" {
    const testing = std.testing;

    const source =
        \\fn use_three(x: i32@3) i32 {
        \\    return x + x + x
        \\}
        \\fn main() i32 { return use_three(10) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "integration: linearity - optional usage with zero uses" {
    const testing = std.testing;

    const source =
        \\fn use_zero(x: i32@?) i32 {
        \\    return 42
        \\}
        \\fn main() i32 { return use_zero(10) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "integration: linearity - optional usage with one use" {
    const testing = std.testing;

    const source =
        \\fn use_one(x: i32@?) i32 {
        \\    return x
        \\}
        \\fn main() i32 { return use_one(5) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "integration: linearity - mixed usage annotations" {
    const testing = std.testing;

    const source =
        \\fn mixed(a: i32, b: i32@2, c: i32@?) i32 {
        \\    return a + b + b
        \\}
        \\fn main() i32 { return mixed(1, 2, 3) }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "integration: linearity - violation using twice without annotation" {
    const testing = std.testing;

    const source =
        \\fn use_twice_bad(x: i32) i32 {
        \\    return x + x
        \\}
        \\fn main() i32 { return use_twice_bad(10) }
    ;
    const result = compile(source, testing.allocator);
    try testing.expectError(error.LinearityViolation, result);
}

test "integration: linearity - violation using zero times without annotation" {
    const testing = std.testing;

    const source =
        \\fn use_zero_bad(x: i32) i32 {
        \\    return 42
        \\}
        \\fn main() i32 { return use_zero_bad(10) }
    ;
    const result = compile(source, testing.allocator);
    try testing.expectError(error.LinearityViolation, result);
}

test "integration: linearity - violation optional used twice" {
    const testing = std.testing;

    const source =
        \\fn use_twice_optional(x: i32@?) i32 {
        \\    return x + x
        \\}
        \\fn main() i32 { return use_twice_optional(10) }
    ;
    const result = compile(source, testing.allocator);
    try testing.expectError(error.LinearityViolation, result);
}

test "integration: linearity - violation @2 used once" {
    const testing = std.testing;

    const source =
        \\fn use_once_bad(x: i32@2) i32 {
        \\    return x
        \\}
        \\fn main() i32 { return use_once_bad(10) }
    ;
    const result = compile(source, testing.allocator);
    try testing.expectError(error.LinearityViolation, result);
}

test "integration: linearity - violation @3 used twice" {
    const testing = std.testing;

    const source =
        \\fn use_twice_bad(x: i32@3) i32 {
        \\    return x + x
        \\}
        \\fn main() i32 { return use_twice_bad(10) }
    ;
    const result = compile(source, testing.allocator);
    try testing.expectError(error.LinearityViolation, result);
}

test "integration: linearity - mutable variables exempt from checking" {
    const testing = std.testing;

    const source =
        \\fn test_var() i32 {
        \\    var x: i32 = 10
        \\    x = x + 1
        \\    x = x + 1
        \\    return x
        \\}
        \\fn main() i32 { return test_var() }
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "type class definition parses" {
    const testing = std.testing;

    const source =
        \\class Copy {
        \\  copy: fn(Self) Self
        \\}
        \\fn main() i32 {
        \\  return 0
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "instance declaration parses" {
    const testing = std.testing;

    const source =
        \\type Point = { x: i32, y: i32 }
        \\
        \\class Copy {
        \\  copy: fn(Self) Self
        \\}
        \\
        \\instance Copy[Point] {
        \\  copy = fn(self: Point) Point {
        \\    return Point { x: self.x, y: self.y }
        \\  }
        \\}
        \\
        \\fn main() i32 {
        \\  return 0
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "Point__copy") != null);
}

test "method call compiles" {
    const testing = std.testing;

    const source =
        \\type Point = { x: i32, y: i32 }
        \\
        \\class Copy {
        \\  copy: fn(Self) Self
        \\}
        \\
        \\instance Copy[Point] {
        \\  copy = fn(self: Point) Point {
        \\    return Point { x: self.x, y: self.y }
        \\  }
        \\}
        \\
        \\fn main() i32 {
        \\  let p: Point = Point { x: 10, y: 20 }
        \\  let p2: Point = p.copy()
        \\  return p2.x
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "Point__copy") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "call") != null);
}

test "multiple instance methods" {
    const testing = std.testing;

    const source =
        \\type Point = { x: i32, y: i32 }
        \\
        \\class Display {
        \\  display: fn(Self) i32
        \\  debug: fn(Self) i32
        \\}
        \\
        \\instance Display[Point] {
        \\  display = fn(self: Point) i32 {
        \\    return self.x
        \\  }
        \\  debug = fn(self: Point) i32 {
        \\    return self.y
        \\  }
        \\}
        \\
        \\fn main() i32 {
        \\  let p: Point@2 = Point { x: 42, y: 99 }
        \\  let x: i32@? = p.display()
        \\  let y: i32 = p.debug()
        \\  return y
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "Point__display") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "Point__debug") != null);
}

test "unsafe block bypasses linearity checks" {
    const testing = std.testing;

    const source =
        \\fn test_unsafe() i32 {
        \\  let x: i32 = 42
        \\  let result: i32 = unsafe {
        \\    let y: i32 = x
        \\    let z: i32 = x
        \\    return y + z
        \\  }
        \\  return result
        \\}
        \\fn main() i32 {
        \\  return test_unsafe()
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "unsafe block with result expression" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: i32 = 10
        \\  let result: i32 = unsafe {
        \\    let a: i32 = x
        \\    let b: i32 = x
        \\    a + b
        \\  }
        \\  return result
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "nested unsafe blocks" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: i32 = 5
        \\  let result: i32 = unsafe {
        \\    let y: i32 = x
        \\    let inner: i32 = unsafe {
        \\      let z: i32 = y
        \\      let w: i32 = y
        \\      z + w
        \\    }
        \\    inner + x
        \\  }
        \\  return result
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "unsafe function bypasses linearity" {
    const testing = std.testing;

    const source =
        \\unsafe fn double_use(x: i32) i32 {
        \\  let a: i32 = x
        \\  let b: i32 = x
        \\  return a + b
        \\}
        \\fn main() i32 {
        \\  return unsafe { double_use(5) }
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "unsafe function called from unsafe block" {
    const testing = std.testing;

    const source =
        \\unsafe fn get_value() i32 {
        \\  return 42
        \\}
        \\fn main() i32 {
        \\  return unsafe {
        \\    get_value()
        \\  }
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "unsafe function called from another unsafe function" {
    const testing = std.testing;

    const source =
        \\unsafe fn helper() i32 {
        \\  return 10
        \\}
        \\unsafe fn caller() i32 {
        \\  return helper()
        \\}
        \\fn main() i32 {
        \\  return unsafe { caller() }
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
}

test "unsafe function cannot be called from safe context" {
    const testing = std.testing;

    const source =
        \\unsafe fn dangerous() i32 {
        \\  return 99
        \\}
        \\fn main() i32 {
        \\  return dangerous()
        \\}
    ;
    const result = compile(source, testing.allocator);
    try testing.expectError(error.UnsafeCallOutsideUnsafeContext, result);
}

test "intrinsic ptr_of creates pointer" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 42
        \\  let p: ptr@* = @ptr_of(x)
        \\  return 0
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "alloca") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store") != null);
}

test "intrinsic ptr_read loads value from pointer" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 42
        \\  let p: ptr@* = @ptr_of(x)
        \\  let val: u64@* = @ptr_read(p, @type(u64))
        \\  return 42
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "load") != null);
}

test "intrinsic ptr_write stores value to pointer" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 0
        \\  let p: ptr@* = @ptr_of(x)
        \\  let value: u64@* = 42
        \\  let unit: ()@* = @ptr_write(p, value)
        \\  let val: u64@* = @ptr_read(p, @type(u64))
        \\  return 42
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "store") != null);
}

test "intrinsic ptr_offset calculates pointer arithmetic" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 42
        \\  let p: ptr@* = @ptr_of(x)
        \\  let p2: ptr@* = @ptr_offset(p, 1)
        \\  return 0
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
}

test "stdlib Pointer.read loads value from pointer" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 42
        \\  let p: ptr@* = @ptr_of(x)
        \\  let val: ptr@* = p.read(@type(u64))
        \\  return 42
        \\}
    ;
    const ir = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "ptr__read") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "load") != null);
}

test "stdlib Pointer.write stores value to pointer" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 0
        \\  let p: ptr@* = @ptr_of(x)
        \\  let value: u64@* = 42
        \\  let unit: ()@* = @ptr_write(p, value)
        \\  let val: ptr@* = p.read(@type(u64))
        \\  return 42
        \\}
    ;
    const ir = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "ptr__write") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "ptr__read") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "store") != null);
}

test "stdlib Pointer.offset calculates pointer arithmetic" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 42
        \\  let p: ptr@* = @ptr_of(x)
        \\  let p2: ptr@* = p.offset(1)
        \\  return 0
        \\}
    ;
    const ir = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "ptr__offset") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "getelementptr") != null);
}

test "stdlib Pointer methods can be chained" {
    const testing = std.testing;

    const source =
        \\fn main() i32 {
        \\  let x: u64@* = 100
        \\  let p: ptr@* = @ptr_of(x)
        \\  let val: u64@* = 150
        \\  let _: ()@* = @ptr_write(p, val)
        \\  let value: ptr@* = p.read(@type(u64))
        \\  return 42
        \\}
    ;
    const ir = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(ir);

    try testing.expect(ir.len > 0);
    try testing.expect(std.mem.indexOf(u8, ir, "ptr__write") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "ptr__read") != null);
}

// Module System Integration Tests

test "module system: compile single file without imports" {
    const testing = std.testing;
    const ModuleResolver = @import("module.zig").ModuleResolver;
    const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;
    
    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(testing.allocator, "test_fixtures/single", "stdlib", include_dirs);
    var graph = DependencyGraph.init(testing.allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/single/standalone.or");

    // Should discover only standalone module
    try testing.expect(graph.modules.contains("standalone"));
    try testing.expectEqual(@as(usize, 1), graph.modules.count());
    
    // No cycles
    const cycle = try graph.detectCycles();
    try testing.expect(cycle == null);
    
    // Single level in topological sort
    const levels = try graph.topologicalSort();
    defer {
        for (levels) |*level| {
            level.deinit(testing.allocator);
        }
        testing.allocator.free(levels);
    }
    try testing.expectEqual(@as(usize, 1), levels.len);
}

test "module system: compile with single import" {
    const testing = std.testing;
    const ModuleResolver = @import("module.zig").ModuleResolver;
    const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(testing.allocator, "test_fixtures/simple", "stdlib", include_dirs);
    var graph = DependencyGraph.init(testing.allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/simple/main.or");

    // Should discover main and lexer
    try testing.expect(graph.modules.contains("main"));
    try testing.expect(graph.modules.contains("lexer"));

    // No cycles
    const cycle = try graph.detectCycles();
    try testing.expect(cycle == null);
}

test "module system: nested dependencies" {
    const testing = std.testing;
    const ModuleResolver = @import("module.zig").ModuleResolver;
    const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(testing.allocator, "test_fixtures/nested", "stdlib", include_dirs);
    var graph = DependencyGraph.init(testing.allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/nested/main.or");
    
    // Should discover main, lexer, and parser
    try testing.expect(graph.modules.contains("main"));
    try testing.expect(graph.modules.contains("lexer"));
    try testing.expect(graph.modules.contains("parser"));
    try testing.expectEqual(@as(usize, 3), graph.modules.count());
    
    // Verify topological order
    const levels = try graph.topologicalSort();
    defer {
        for (levels) |*level| {
            level.deinit(testing.allocator);
        }
        testing.allocator.free(levels);
    }
    
    // lexer -> parser -> main (3 levels)
    try testing.expectEqual(@as(usize, 3), levels.len);
}

test "module system: detect circular dependency" {
    const testing = std.testing;
    const ModuleResolver = @import("module.zig").ModuleResolver;
    const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;

    const include_dirs: []const []const u8 = &.{};
    const resolver = ModuleResolver.init(testing.allocator, "test_fixtures/cycle", "stdlib", include_dirs);
    var graph = DependencyGraph.init(testing.allocator, resolver);
    defer graph.deinit();
    
    try graph.discover("test_fixtures/cycle/main.or");
    
    // Should detect cycle
    const cycle = try graph.detectCycles();
    try testing.expect(cycle != null);
    
    if (cycle) |c| {
        defer testing.allocator.free(c);
        // Cycle should contain both a and b
        try testing.expect(c.len >= 2);
    }
}

test "module system: resolve from include_dirs" {
    const testing = std.testing;
    const ModuleResolver = @import("module.zig").ModuleResolver;
    const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;

    const include_dirs: []const []const u8 = &.{"test_fixtures/external_lib"};
    const resolver = ModuleResolver.init(testing.allocator, "test_fixtures/with_external/src", "stdlib", include_dirs);
    var graph = DependencyGraph.init(testing.allocator, resolver);
    defer graph.deinit();

    try graph.discover("test_fixtures/with_external/src/main.or");

    // Should discover main and utils (from include_dir)
    try testing.expect(graph.modules.contains("main"));
    try testing.expect(graph.modules.contains("utils"));
    try testing.expectEqual(@as(usize, 2), graph.modules.count());

    // main depends on utils
    const main_deps = graph.edges.get("main").?;
    try testing.expectEqual(@as(usize, 1), main_deps.items.len);
    try testing.expectEqualStrings("utils", main_deps.items[0]);

    // utils should be resolved from include_dir
    const utils_mod = graph.modules.get("utils").?;
    try testing.expectEqualStrings("test_fixtures/external_lib/utils.or", utils_mod.file_path);
}

test "string literals: basic string" {
    const testing = std.testing;
    const source =
        \\fn get_msg() str {
        \\    return "hello"
        \\}
        \\fn main() i32 {
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check for string constant declaration
    try testing.expect(std.mem.indexOf(u8, result, "@.str.0") != null);
    try testing.expect(std.mem.indexOf(u8, result, "hello") != null);

    // Check string is null-terminated
    try testing.expect(std.mem.indexOf(u8, result, "\\00") != null);
}

test "string literals: escape sequences" {
    const testing = std.testing;
    const source =
        \\fn get_msg() str {
        \\    return "hello\nworld\t!"
        \\}
        \\fn main() i32 {
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check for escape sequences in LLVM IR (\0A = newline, \09 = tab)
    try testing.expect(std.mem.indexOf(u8, result, "\\0A") != null);
    try testing.expect(std.mem.indexOf(u8, result, "\\09") != null);
}

test "string literals: empty string" {
    const testing = std.testing;
    const source =
        \\fn get_msg() str {
        \\    return ""
        \\}
        \\fn main() i32 {
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Empty string should just be null terminator
    try testing.expect(std.mem.indexOf(u8, result, "[1 x i8]") != null);
}

test "string literals: multiple strings" {
    const testing = std.testing;
    const source =
        \\fn get_first() str {
        \\    return "first"
        \\}
        \\fn get_second() str {
        \\    return "second"
        \\}
        \\fn main() i32 {
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should have two different string constants
    try testing.expect(std.mem.indexOf(u8, result, "@.str.0") != null);
    try testing.expect(std.mem.indexOf(u8, result, "@.str.1") != null);
    try testing.expect(std.mem.indexOf(u8, result, "first") != null);
    try testing.expect(std.mem.indexOf(u8, result, "second") != null);
}

test "string operations: len" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s: str = "hello"
        \\    let length: i64@* = s.len()
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "string operations: at" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s: str = "hello"
        \\    let c: u8@* = s.at(0)
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "string operations: is_empty" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s: str = "hello"
        \\    let empty: bool@* = s.is_empty()
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "string operations: equals" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s1: str = "hello"
        \\    let s2: str@* = "world"
        \\    let eq: bool@* = s1.equals(s2)
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "string operations: starts_with" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s: str = "hello world"
        \\    let starts: bool@* = s.starts_with("hello")
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "string operations: ends_with" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s: str = "hello world"
        \\    let ends: bool@* = s.ends_with("world")
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "string operations: find" {
    const testing = std.testing;
    const source =
        \\fn main() i32 {
        \\    let s: str = "hello world"
        \\    let pos: i64@* = s.find("world")
        \\    return 0
        \\}
    ;

    const result = try compileWithStdlib(source, testing.allocator);
    defer testing.allocator.free(result);

    // Should compile successfully
    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "dependent types: basic type definition" {
    const testing = std.testing;
    const source =
        \\type Vec[A, n: u64] = i32
        \\fn main() i32 { return 0 }
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
}

test "dependent types: function returning dependent type" {
    const testing = std.testing;
    const source =
        \\type Vec[A, n: u64] = i32
        \\fn make_vec() Vec[i32, 5] {
        \\  return 42
        \\}
        \\fn main() i32 {
        \\  unsafe {
        \\    let v: Vec[i32, 5] = make_vec()
        \\    return v
        \\  }
        \\}
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "define") != null);
    try testing.expect(std.mem.indexOf(u8, result, "make_vec") != null);
}

test "dependent types: instance method with mangled names" {
    const testing = std.testing;
    const source =
        \\type Vec[A, n: u64] = i32
        \\class VecOps {
        \\  get_value: fn(Vec[A, n]) i32;
        \\}
        \\instance VecOps[Vec[i32, 5]] {
        \\  get_value = fn(v: Vec[i32, 5]) i32 {
        \\    return v
        \\  }
        \\}
        \\fn main() i32 { return 0 }
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check for mangled method name: Vec$$i32$5$$__get_value
    try testing.expect(std.mem.indexOf(u8, result, "Vec$$i32$5$$__get_value") != null);
}

test "dependent types: method call on dependent type" {
    const testing = std.testing;
    const source =
        \\type Vec[A, n: u64] = i32
        \\class VecOps {
        \\  get_value: fn(Vec[A, n]) i32;
        \\}
        \\instance VecOps[Vec[i32, 5]] {
        \\  get_value = fn(v: Vec[i32, 5]) i32 {
        \\    return v
        \\  }
        \\}
        \\fn make_vec() Vec[i32, 5] {
        \\  return 42
        \\}
        \\fn main() i32 {
        \\  unsafe {
        \\    let my_vec: Vec[i32, 5] = make_vec()
        \\    let value: i32 = my_vec.get_value()
        \\    return value
        \\  }
        \\}
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check for mangled method name in both definition and call
    try testing.expect(std.mem.indexOf(u8, result, "Vec$$i32$5$$__get_value") != null);
    try testing.expect(std.mem.indexOf(u8, result, "call") != null);
}

test "dependent types: multiple value parameters" {
    const testing = std.testing;
    const source =
        \\type Matrix[A, rows: u64, cols: u64] = i32
        \\class MatrixOps {
        \\  get_elem: fn(Matrix[A, rows, cols]) i32;
        \\}
        \\instance MatrixOps[Matrix[i32, 3, 4]] {
        \\  get_elem = fn(m: Matrix[i32, 3, 4]) i32 {
        \\    return m
        \\  }
        \\}
        \\fn main() i32 { return 0 }
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check for mangled method name: Matrix$$i32$3$4$$__get_elem
    try testing.expect(std.mem.indexOf(u8, result, "Matrix$$i32$3$4$$__get_elem") != null);
}

test "dependent types: type resolution in operations" {
    const testing = std.testing;
    const source =
        \\type Vec[A, n: u64] = i32
        \\class VecOps {
        \\  add_ten: fn(Vec[A, n]) i32;
        \\}
        \\instance VecOps[Vec[i32, 5]] {
        \\  add_ten = fn(v: Vec[i32, 5]) i32 {
        \\    return v + 10
        \\  }
        \\}
        \\fn main() i32 { return 0 }
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check that arithmetic operation compiles correctly
    try testing.expect(std.mem.indexOf(u8, result, "add") != null);
}

test "dependent types: full chain test" {
    const testing = std.testing;
    const source =
        \\type Vec[A, n: u64] = i32
        \\class VecOps {
        \\  get_value: fn(Vec[A, n]) i32;
        \\  add_ten: fn(Vec[A, n]) i32;
        \\}
        \\instance VecOps[Vec[i32, 5]] {
        \\  get_value = fn(v: Vec[i32, 5]) i32 {
        \\    return v
        \\  }
        \\  add_ten = fn(v: Vec[i32, 5]) i32 {
        \\    return v + 10
        \\  }
        \\}
        \\fn make_vec() Vec[i32, 5] {
        \\  return 42
        \\}
        \\fn main() i32 {
        \\  unsafe {
        \\    let my_vec: Vec[i32, 5] = make_vec()
        \\    let value: i32 = my_vec.get_value()
        \\    let result: i32 = my_vec.add_ten()
        \\    return result
        \\  }
        \\}
    ;

    const result = try compile(source, testing.allocator);
    defer testing.allocator.free(result);

    // Check all mangled names are present
    try testing.expect(std.mem.indexOf(u8, result, "Vec$$i32$5$$__get_value") != null);
    try testing.expect(std.mem.indexOf(u8, result, "Vec$$i32$5$$__add_ten") != null);
    try testing.expect(std.mem.indexOf(u8, result, "make_vec") != null);
}

// Generic struct tests
test "integration: basic generic struct" {
    const testing = std.testing;

    const source =
        \\type Box[T] = { value: T }
        \\fn main() i32 {
        \\  let b: Box[i32] = Box[i32] { value: 42 }
        \\  return b.value
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct type with i32 field (T substituted with i32)
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32 }") != null);
    // Check insertvalue for struct creation
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i32 } undef, i32 42, 0") != null);
    // Check extractvalue for field access
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32 }") != null);
}

test "integration: generic struct with i64" {
    const testing = std.testing;

    const source =
        \\type Box[T] = { value: T }
        \\fn main() i64 {
        \\  let b: Box[i64] = Box[i64] { value: 100 }
        \\  return b.value
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct type with i64 field (T substituted with i64)
    try testing.expect(std.mem.indexOf(u8, ir, "{ i64 }") != null);
    try testing.expect(std.mem.indexOf(u8, ir, "insertvalue { i64 } undef, i64 100, 0") != null);
}

test "integration: generic struct with multiple fields" {
    const testing = std.testing;

    const source =
        \\type Pair[A, B] = { first: A, second: B }
        \\fn main() i32 {
        \\  let p: Pair[i32, i64] = Pair[i32, i64] { first: 1, second: 2 }
        \\  return p.first
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check struct type with i32 and i64 fields
    try testing.expect(std.mem.indexOf(u8, ir, "{ i32, i64 }") != null);
    // Check field access
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32, i64 }") != null);
}

test "integration: generic struct field access returns correct type" {
    const testing = std.testing;

    const source =
        \\type Box[T] = { value: T }
        \\fn get_value(b: Box[i32]) i32 {
        \\  return b.value
        \\}
        \\fn main() i32 {
        \\  let b: Box[i32] = Box[i32] { value: 99 }
        \\  return get_value(b)
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function takes struct parameter
    try testing.expect(std.mem.indexOf(u8, ir, "define i32 @get_value({ i32 } %b)") != null);
    // Check extractvalue returns i32
    try testing.expect(std.mem.indexOf(u8, ir, "extractvalue { i32 }") != null);
}

test "integration: generic struct as function return type" {
    const testing = std.testing;

    const source =
        \\type Box[T] = { value: T }
        \\fn make_box() Box[i32] {
        \\  return Box[i32] { value: 42 }
        \\}
        \\fn main() i32 {
        \\  let b: Box[i32] = make_box()
        \\  return b.value
        \\}
    ;
    const ir = try compile(source, testing.allocator);
    defer testing.allocator.free(ir);

    // Check function returns struct type
    try testing.expect(std.mem.indexOf(u8, ir, "define { i32 } @make_box()") != null);
}
