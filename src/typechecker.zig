const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;

const FunctionSignature = struct {
    params: []const Type,
    return_type: Type,
};

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(Type),
    functions: std.StringHashMap(FunctionSignature),

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(Type).init(allocator),
            .functions = std.StringHashMap(FunctionSignature).init(allocator),
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        var iter = self.functions.valueIterator();
        while (iter.next()) |sig| {
            self.allocator.free(sig.params);
        }
        self.variables.deinit();
        self.functions.deinit();
    }

    pub fn check(self: *TypeChecker, ast: *const AST) !void {
        // First pass: collect function signatures
        for (ast.functions.items) |func| {
            const param_types = try self.allocator.alloc(Type, func.params.len);
            for (func.params, 0..) |param, i| {
                param_types[i] = param.param_type;
            }
            try self.functions.put(func.name, .{
                .params = param_types,
                .return_type = func.return_type,
            });
        }

        // Second pass: check function bodies
        for (ast.functions.items) |func| {
            try self.checkFunction(&func);
        }
    }

    fn checkFunction(self: *TypeChecker, func: *const parser.FunctionDecl) !void {
        // Clear variables from previous function
        self.variables.clearRetainingCapacity();

        // Add parameters to variable environment
        for (func.params) |param| {
            try self.variables.put(param.name, param.param_type);
        }

        // Check that function has a body
        if (func.body.items.len == 0) {
            return error.EmptyFunctionBody;
        }

        // Check all statements
        for (func.body.items) |stmt| {
            switch (stmt) {
                .let_binding => |binding| {
                    const value_type = try self.inferExprType(binding.value);

                    // Determine actual variable type
                    const var_type = if (binding.type_annotation) |expected_type| blk: {
                        if (!self.canImplicitlyConvert(value_type, expected_type)) {
                            std.debug.print("Type mismatch in let binding '{s}': expected {s}, got {s}\n", .{
                                binding.name,
                                @tagName(expected_type),
                                @tagName(value_type),
                            });
                            return error.TypeMismatch;
                        }
                        break :blk expected_type;
                    } else value_type;

                    // Add variable to environment
                    try self.variables.put(binding.name, var_type);
                },
                .return_stmt => |expr| {
                    const expr_type = try self.inferExprType(expr);

                    if (!self.canImplicitlyConvert(expr_type, func.return_type)) {
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
            .bool_literal => return .bool,
            .variable => |name| {
                if (self.variables.get(name)) |var_type| {
                    return var_type;
                }
                std.debug.print("Undefined variable: {s}\n", .{name});
                return error.UndefinedVariable;
            },
            .binary_op => |binop| {
                const left_type = try self.inferExprType(binop.left);
                const right_type = try self.inferExprType(binop.right);

                // Logical operators work on both bool and integers
                if (binop.op == .logical_and or binop.op == .logical_or) {
                    // If both are bool, result is bool
                    if (left_type == .bool and right_type == .bool) {
                        return .bool;
                    }
                    // If both are integers, result is integer (old behavior for compatibility)
                    if (self.isIntegerType(left_type) and self.isIntegerType(right_type)) {
                        if (!self.typesMatch(left_type, right_type)) {
                            return error.TypeMismatch;
                        }
                        return left_type;
                    }
                    // Mixed types not allowed
                    return error.TypeMismatch;
                }

                // All other operators require integer operands
                if (!self.isIntegerType(left_type) or !self.isIntegerType(right_type)) {
                    return error.TypeMismatch;
                }

                // Comparison operators return bool
                if (binop.op.returns_bool()) return .bool;

                // For arithmetic operators, both operands must be the same type
                if (!self.typesMatch(left_type, right_type)) {
                    return error.TypeMismatch;
                }

                // Result type is the same as operand type
                return left_type;
            },
            .unary_op => |unop| {
                const operand_type = try self.inferExprType(unop.operand);

                if (!self.isIntegerType(operand_type)) {
                    return error.TypeMismatch;
                }

                // Comparison operators return bool, arithmetic operators preserve type
                return if (unop.op.returns_bool()) .bool else operand_type;
            },
            .function_call => |call| {
                // Look up function signature
                const sig = self.functions.get(call.name) orelse {
                    std.debug.print("Undefined function: {s}\n", .{call.name});
                    return error.UndefinedFunction;
                };

                // Check argument count
                if (call.args.len != sig.params.len) {
                    std.debug.print("Function {s} expects {d} arguments, got {d}\n", .{
                        call.name,
                        sig.params.len,
                        call.args.len,
                    });
                    return error.ArgumentCountMismatch;
                }

                // Check argument types
                for (call.args, 0..) |arg, i| {
                    const arg_type = try self.inferExprType(arg);
                    const expected_type = sig.params[i];

                    if (!self.canImplicitlyConvert(arg_type, expected_type)) {
                        std.debug.print("Function {s} argument {d}: expected {s}, got {s}\n", .{
                            call.name,
                            i,
                            @tagName(expected_type),
                            @tagName(arg_type),
                        });
                        return error.TypeMismatch;
                    }
                }

                return sig.return_type;
            },
            .if_expr => |if_expr| {
                // Condition must be bool
                const condition_type = try self.inferExprType(if_expr.condition);
                if (condition_type != .bool) {
                    std.debug.print("If condition must be bool, got {s}\n", .{@tagName(condition_type)});
                    return error.TypeMismatch;
                }

                // Then branch type
                const then_type = try self.inferExprType(if_expr.then_branch);

                // If there's an else branch, both branches must have same type
                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.inferExprType(else_branch);
                    if (!self.typesMatch(then_type, else_type)) {
                        std.debug.print("If branches have different types: then={s}, else={s}\n", .{
                            @tagName(then_type),
                            @tagName(else_type),
                        });
                        return error.TypeMismatch;
                    }
                    return then_type;
                } else {
                    // If expressions must have else branch to produce a value
                    std.debug.print("If expression must have else branch\n", .{});
                    return error.TypeMismatch;
                }
            },
            .while_expr => |while_expr| {
                // Condition must be bool
                const condition_type = try self.inferExprType(while_expr.condition);
                if (condition_type != .bool) {
                    std.debug.print("While condition must be bool, got {s}\n", .{@tagName(condition_type)});
                    return error.TypeMismatch;
                }

                // Type check the body (but ignore its type)
                _ = try self.inferExprType(while_expr.body);

                // While loops always return i32 as unit value (body type is ignored)
                return .i32;
            },
        }
    }

    fn canImplicitlyConvert(_: *TypeChecker, from: Type, to: Type) bool {

        // Exact match is always allowed
        if (std.meta.eql(from, to)) return true;

        // Bool can convert to any integer type (safe: 0 or 1)
        if (from == .bool and to != .bool) return true;

        // TODO: Add typed integer literals to avoid need for I32 promotion
        // I32 literals can widen to same or larger width types (no data loss)
        if (from == .i32) {
            return to.bitWidth() >= from.bitWidth();
        }

        // No other implicit conversions allowed
        return false;
    }

    fn typesMatch(_: *TypeChecker, a: Type, b: Type) bool {
        return std.meta.eql(a, b);
    }

    fn isIntegerType(_: *TypeChecker, typ: Type) bool {
        return typ.isInteger();
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

test "typechecker: logical operators on integers return integers" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_][]const u8{
        "fn f() -> I32 { return 5 && 3 }",
        "fn f() -> I32 { return 5 || 3 }",
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

test "typechecker: logical operators on bools return bool" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_][]const u8{
        "fn f() -> Bool { return (5 > 3) && (2 < 4) }",
        "fn f() -> Bool { return (5 > 3) || (2 < 4) }",
        "fn f() -> Bool { return true && false }",
        "fn f() -> Bool { return true || false }",
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

