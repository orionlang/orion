const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *TypeChecker) void {
        _ = self;
    }

    pub fn check(self: *TypeChecker, ast: *const AST) !void {
        for (ast.functions.items) |func| {
            try self.checkFunction(&func);
        }
    }

    fn checkFunction(self: *TypeChecker, func: *const parser.FunctionDecl) !void {
        // Check that function has a body
        if (func.body.items.len == 0) {
            return error.EmptyFunctionBody;
        }

        // For now, just check the return statement
        for (func.body.items) |stmt| {
            switch (stmt) {
                .return_stmt => |expr| {
                    const expr_type = try self.inferExprType(expr);
                    // Allow Bool -> I32 implicit conversion
                    const types_compatible = self.typesMatch(expr_type, func.return_type) or
                        (expr_type == .bool and func.return_type == .i32);

                    if (!types_compatible) {
                        std.debug.print("Type mismatch in function '{s}': expected {s}, got {s}\n", .{
                            func.name,
                            @tagName(func.return_type),
                            @tagName(expr_type),
                        });
                        return error.TypeMismatch;
                    }
                },
            }
        }
    }

    fn inferExprType(self: *TypeChecker, expr: *const Expr) !Type {
        switch (expr.*) {
            .integer_literal => return .i32,
            .binary_op => |binop| {
                const left_type = try self.inferExprType(binop.left);
                const right_type = try self.inferExprType(binop.right);

                if (!self.typesMatch(left_type, .i32) or !self.typesMatch(right_type, .i32)) {
                    return error.TypeMismatch;
                }

                return if (binop.op.returns_bool()) .bool else .i32;
            },
            .unary_op => |unop| {
                const operand_type = try self.inferExprType(unop.operand);

                if (!self.typesMatch(operand_type, .i32)) {
                    return error.TypeMismatch;
                }

                return if (unop.op.returns_bool()) .bool else .i32;
            },
        }
    }

    fn typesMatch(self: *TypeChecker, a: Type, b: Type) bool {
        _ = self;
        return std.meta.eql(a, b);
    }
};

test "typechecker: valid function" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn main() -> I32 { return 42 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    var typechecker = TypeChecker.init(testing.allocator);
    defer typechecker.deinit();

    // Should not error
    try typechecker.check(&ast);
}

test "typechecker: type mismatch" {
    const testing = std.testing;
    // For now this test would require manually constructing an AST with wrong types
    // We'll skip it until we have more type variations
    _ = testing;
}

test "typechecker: bool return type" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn main() -> Bool { return 5 > 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    var typechecker = TypeChecker.init(testing.allocator);
    defer typechecker.deinit();

    try typechecker.check(&ast);
}

test "typechecker: comparison operators return bool" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_][]const u8{
        "fn f() -> Bool { return 5 == 3 }",
        "fn f() -> Bool { return 5 != 3 }",
        "fn f() -> Bool { return 5 < 3 }",
        "fn f() -> Bool { return 5 > 3 }",
        "fn f() -> Bool { return 5 <= 3 }",
        "fn f() -> Bool { return 5 >= 3 }",
    };

    for (cases) |case| {
        var lex = Lexer.init(case);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        var typechecker = TypeChecker.init(testing.allocator);
        defer typechecker.deinit();

        try typechecker.check(&ast);
    }
}

test "typechecker: logical operators return bool" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_][]const u8{
        "fn f() -> Bool { return 5 && 3 }",
        "fn f() -> Bool { return 5 || 3 }",
        "fn f() -> Bool { return !5 }",
    };

    for (cases) |case| {
        var lex = Lexer.init(case);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        var typechecker = TypeChecker.init(testing.allocator);
        defer typechecker.deinit();

        try typechecker.check(&ast);
    }
}

test "typechecker: bool to i32 implicit conversion" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 5 > 3 }",
        "fn f() -> I32 { return 5 && 3 }",
        "fn f() -> I32 { return !5 }",
    };

    for (cases) |case| {
        var lex = Lexer.init(case);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        var typechecker = TypeChecker.init(testing.allocator);
        defer typechecker.deinit();

        try typechecker.check(&ast);
    }
}

