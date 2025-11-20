const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;

const FunctionSignature = struct {
    params: []const Type,
    return_type: Type,
};

const TypeCheckError = error{
    TypeMismatch,
    UndefinedVariable,
    UndefinedFunction,
    ArgumentCountMismatch,
    EmptyFunctionBody,
    OutOfMemory,
    AssignmentToImmutable,
};

const VariableInfo = struct {
    var_type: Type,
    mutable: bool,
};

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(VariableInfo),
    functions: std.StringHashMap(FunctionSignature),

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(VariableInfo).init(allocator),
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

    fn printTypeMismatch(context: []const u8, name: []const u8, expected: Type, actual: Type) void {
        std.debug.print("Type mismatch in {s} '{s}': expected {s}, got {s}\n", .{
            context,
            name,
            @tagName(expected),
            @tagName(actual),
        });
    }

    fn checkFunction(self: *TypeChecker, func: *const parser.FunctionDecl) !void {
        // Clear variables from previous function
        self.variables.clearRetainingCapacity();

        // Add parameters to variable environment (immutable)
        for (func.params) |param| {
            try self.variables.put(param.name, .{ .var_type = param.param_type, .mutable = false });
        }

        // Check that function has a body
        if (func.body.items.len == 0) {
            return error.EmptyFunctionBody;
        }

        // Check all statements
        for (func.body.items) |stmt| {
            // Propagate expected types to return statement expressions before checking
            if (stmt == .return_stmt) {
                try self.checkExprWithExpectedType(stmt.return_stmt, func.return_type);
            }

            try self.checkStatement(stmt, func.return_type);
        }

        // Validate that at least one return statement exists and has correct type
        // Type validation happens in checkStatement for .return_stmt case
    }

    fn checkStatement(self: *TypeChecker, stmt: Stmt, func_return_type: ?Type) TypeCheckError!void {
        switch (stmt) {
            .let_binding => |binding| {
                // For literals, update their inferred type to match context
                if (binding.type_annotation) |expected_type| {
                    try self.checkExprWithExpectedType(binding.value, expected_type);
                }
                // Verify the final type matches (catches type errors for non-literal expressions)
                const value_type = try self.inferExprType(binding.value);
                const var_type = binding.type_annotation orelse value_type;
                if (binding.type_annotation) |expected_type| {
                    if (!self.canImplicitlyConvert(value_type, expected_type)) {
                        printTypeMismatch("let binding", binding.name, expected_type, value_type);
                        return error.TypeMismatch;
                    }
                }
                try self.variables.put(binding.name, .{ .var_type = var_type, .mutable = binding.mutable });
            },
            .assignment => |assign| {
                const var_info = self.variables.get(assign.name) orelse {
                    std.debug.print("Assignment to undefined variable: {s}\n", .{assign.name});
                    return error.UndefinedVariable;
                };
                if (!var_info.mutable) {
                    std.debug.print("Cannot assign to immutable variable: {s}\n", .{assign.name});
                    return error.AssignmentToImmutable;
                }
                // Propagate expected type to RHS expression (for literal inference)
                try self.checkExprWithExpectedType(assign.value, var_info.var_type);

                const value_type = try self.inferExprType(assign.value);
                if (!self.canImplicitlyConvert(value_type, var_info.var_type)) {
                    printTypeMismatch("assignment to", assign.name, var_info.var_type, value_type);
                    return error.TypeMismatch;
                }
            },
            .while_stmt => |while_stmt| {
                const condition_type = try self.inferExprType(while_stmt.condition);
                if (condition_type != .bool) {
                    std.debug.print("While condition must be bool, got {s}\n", .{@tagName(condition_type)});
                    return error.TypeMismatch;
                }
                _ = try self.inferExprType(while_stmt.body);
            },
            .return_stmt => |ret_expr| {
                const expr_type = try self.inferExprType(ret_expr);
                if (func_return_type) |expected_type| {
                    if (!self.canImplicitlyConvert(expr_type, expected_type)) {
                        std.debug.print("Type mismatch in return statement: expected {s}, got {s}\n", .{
                            @tagName(expected_type),
                            @tagName(expr_type),
                        });
                        return error.TypeMismatch;
                    }
                }
            },
        }
    }

    fn checkExprWithExpectedType(self: *TypeChecker, expr: *Expr, expected_type: Type) !void {
        // For integer literals, update the inferred type and validate range
        switch (expr.*) {
            .integer_literal => |*lit| {
                // Check if the value fits in the expected type
                const value = lit.value;
                const min = expected_type.minValue();
                const max = expected_type.maxValue();

                if (value < min or value > max) {
                    // Provide clear error messages for common cases
                    if (!expected_type.isSigned() and value < 0) {
                        std.debug.print("Integer literal {d} is negative but type {s} is unsigned\n", .{
                            value,
                            @tagName(expected_type),
                        });
                    } else {
                        std.debug.print("Integer literal {d} does not fit in type {s} (range: {d} to {d})\n", .{
                            value,
                            @tagName(expected_type),
                            min,
                            max,
                        });
                    }
                    return error.TypeMismatch;
                }

                // Update the inferred type
                lit.inferred_type = expected_type;
            },
            .binary_op => |*binop| {
                // For arithmetic binary operations, propagate expected type to both operands
                // This allows `let x: U32 = 10 + 5` to type both literals as U32
                if (!binop.op.returns_bool()) {
                    try self.checkExprWithExpectedType(binop.left, expected_type);
                    try self.checkExprWithExpectedType(binop.right, expected_type);
                }
                // For comparison ops, we don't propagate type (result is bool anyway)
            },
            .unary_op => |*unop| {
                // TODO: Propagate expected type to operand for unary minus
                // Example: `let x: I64 = -42` should type the literal 42 as I64 before negating
                _ = unop;
            },
            .if_expr => |*if_expr| {
                // TODO: Propagate expected type to both branches
                // Example: `let x: I64 = if cond { 42 } else { 100 }` should type both literals as I64
                _ = if_expr;
            },
            .block_expr => |*block| {
                // TODO: Propagate expected type to result expression
                // Example: `let x: I64 = { stmt; stmt; 42 }` should type 42 as I64
                _ = block;
            },
            .bool_literal, .variable, .function_call => {
                // These expressions have fixed types that can't be influenced by context:
                // - bool_literal: always Bool
                // - variable: type determined at declaration
                // - function_call: return type is fixed by function signature
            },
        }
    }

    fn inferExprType(self: *TypeChecker, expr: *const Expr) !Type {
        switch (expr.*) {
            .integer_literal => |lit| return lit.inferred_type,
            .bool_literal => return .bool,
            .variable => |name| {
                if (self.variables.get(name)) |var_info| {
                    return var_info.var_type;
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
            .block_expr => |block| {
                for (block.statements) |stmt| {
                    try self.checkStatement(stmt, null);
                }

                if (block.result) |result| {
                    return try self.inferExprType(result);
                } else {
                    return .i32;
                }
            },
        }
    }

    fn canImplicitlyConvert(_: *TypeChecker, from: Type, to: Type) bool {

        // Exact match is always allowed
        if (std.meta.eql(from, to)) return true;

        // Bool can convert to any integer type (safe: 0 or 1)
        if (from == .bool and to.isInteger()) return true;

        // Allow widening conversions between integer types (no data loss)
        // Signed can widen to larger signed, unsigned can widen to larger unsigned
        // We don't allow mixing signed/unsigned to prevent unexpected behavior
        if (from.isInteger() and to.isInteger()) {
            // Same signedness and target is wider (not equal - handled by exact match above)
            if (from.isSigned() == to.isSigned() and to.bitWidth() > from.bitWidth()) {
                return true;
            }
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

    const source = "fn main() I32 { return 42 }";
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

    const source = "fn main() Bool { return 5 > 3 }";
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
        "fn f() Bool { return 5 == 3 }",
        "fn f() Bool { return 5 != 3 }",
        "fn f() Bool { return 5 < 3 }",
        "fn f() Bool { return 5 > 3 }",
        "fn f() Bool { return 5 <= 3 }",
        "fn f() Bool { return 5 >= 3 }",
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
        "fn f() I32 { return 5 && 3 }",
        "fn f() I32 { return 5 || 3 }",
        "fn f() I32 { return !5 }",
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
        "fn f() Bool { return (5 > 3) && (2 < 4) }",
        "fn f() Bool { return (5 > 3) || (2 < 4) }",
        "fn f() Bool { return true && false }",
        "fn f() Bool { return true || false }",
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
        "fn f() I32 { return 5 > 3 }",
        "fn f() I32 { return 5 && 3 }",
        "fn f() I32 { return !5 }",
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

