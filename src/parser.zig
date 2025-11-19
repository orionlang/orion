const std = @import("std");
const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenKind = lexer.TokenKind;

pub const Type = enum {
    bool,
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    u64,

    pub fn isSigned(self: Type) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64 => true,
            .bool, .u8, .u16, .u32, .u64 => false,
        };
    }

    pub fn bitWidth(self: Type) u32 {
        return switch (self) {
            .bool => 1,
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32 => 32,
            .i64, .u64 => 64,
        };
    }

    pub fn isInteger(self: Type) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => true,
            .bool => false,
        };
    }

    pub fn llvmTypeName(self: Type) []const u8 {
        return switch (self) {
            .bool => "i1",
            .i8, .u8 => "i8",
            .i16, .u16 => "i16",
            .i32, .u32 => "i32",
            .i64, .u64 => "i64",
        };
    }
};

pub const ParseError = error{
    UnexpectedEndOfFile,
    UnexpectedToken,
    ExpectedExpression,
    UnknownType,
    ExpectedStatement,
    OutOfMemory,
    InvalidCharacter,
    Overflow,
};

pub const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,
    equal,
    not_equal,
    less_than,
    greater_than,
    less_equal,
    greater_equal,
    logical_and,
    logical_or,
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    shift_left,
    shift_right,

    pub fn returns_bool(self: BinaryOp) bool {
        return switch (self) {
            .equal, .not_equal, .less_than, .greater_than, .less_equal, .greater_equal, .logical_and, .logical_or => true,
            else => false,
        };
    }
};

pub const UnaryOp = enum {
    negate,
    logical_not,
    bitwise_not,

    pub fn returns_bool(self: UnaryOp) bool {
        return self == .logical_not;
    }
};

pub const Expr = union(enum) {
    integer_literal: i32,
    variable: []const u8,
    binary_op: struct {
        op: BinaryOp,
        left: *Expr,
        right: *Expr,
    },
    unary_op: struct {
        op: UnaryOp,
        operand: *Expr,
    },
    function_call: struct {
        name: []const u8,
        args: []*Expr,
    },
};

pub const Param = struct {
    name: []const u8,
    param_type: Type,
};

pub const Stmt = union(enum) {
    let_binding: struct {
        name: []const u8,
        type_annotation: ?Type,
        value: *Expr,
    },
    return_stmt: *Expr,
};

pub const FunctionDecl = struct {
    name: []const u8,
    params: []Param,
    return_type: Type,
    body: std.ArrayList(Stmt),

    pub fn deinit(self: *FunctionDecl, allocator: std.mem.Allocator) void {
        allocator.free(self.params);
        for (self.body.items) |*stmt| {
            switch (stmt.*) {
                .let_binding => |binding| {
                    deinitExpr(allocator, binding.value);
                    allocator.destroy(binding.value);
                },
                .return_stmt => |expr| {
                    deinitExpr(allocator, expr);
                    allocator.destroy(expr);
                },
            }
        }
        self.body.deinit(allocator);
    }

    fn deinitExpr(allocator: std.mem.Allocator, expr: *Expr) void {
        switch (expr.*) {
            .integer_literal => {},
            .variable => {},
            .binary_op => |binop| {
                deinitExpr(allocator, binop.left);
                allocator.destroy(binop.left);
                deinitExpr(allocator, binop.right);
                allocator.destroy(binop.right);
            },
            .unary_op => |unop| {
                deinitExpr(allocator, unop.operand);
                allocator.destroy(unop.operand);
            },
            .function_call => |call| {
                for (call.args) |arg| {
                    deinitExpr(allocator, arg);
                    allocator.destroy(arg);
                }
                allocator.free(call.args);
            },
        }
    }
};

pub const AST = struct {
    functions: std.ArrayList(FunctionDecl),

    pub fn deinit(self: *AST, allocator: std.mem.Allocator) void {
        for (self.functions.items) |*func| {
            func.deinit(allocator);
        }
        self.functions.deinit(allocator);
    }
};

pub const Parser = struct {
    tokens: []const Token,
    pos: usize,
    allocator: std.mem.Allocator,

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Parser) !AST {
        var functions: std.ArrayList(FunctionDecl) = .empty;
        errdefer functions.deinit(self.allocator);

        while (!self.isAtEnd()) {
            const func = try self.parseFunction();
            try functions.append(self.allocator, func);
        }

        return AST{ .functions = functions };
    }

    fn parseFunction(self: *Parser) !FunctionDecl {
        _ = try self.expect(.fn_keyword);

        const name_token = try self.expect(.identifier);
        const name = name_token.lexeme;

        _ = try self.expect(.left_paren);

        // Parse parameters: param_name: Type, param_name: Type, ...
        var params: std.ArrayList(Param) = .empty;
        errdefer params.deinit(self.allocator);

        if (!self.check(.right_paren)) {
            while (true) {
                const param_name = try self.expect(.identifier);
                _ = try self.expect(.colon);
                const param_type = try self.parseType();

                try params.append(self.allocator, Param{
                    .name = param_name.lexeme,
                    .param_type = param_type,
                });

                if (!self.check(.comma)) break;
                _ = try self.expect(.comma);
            }
        }

        _ = try self.expect(.right_paren);

        _ = try self.expect(.arrow);
        const return_type = try self.parseType();

        _ = try self.expect(.left_brace);
        var body: std.ArrayList(Stmt) = .empty;
        errdefer body.deinit(self.allocator);

        while (!self.check(.right_brace)) {
            const stmt = try self.parseStatement();
            try body.append(self.allocator, stmt);
        }

        _ = try self.expect(.right_brace);

        return FunctionDecl{
            .name = name,
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .body = body,
        };
    }

    const type_name_map = std.StaticStringMap(Type).initComptime(.{
        .{ "Bool", .bool },
        .{ "I8", .i8 },
        .{ "I16", .i16 },
        .{ "I32", .i32 },
        .{ "I64", .i64 },
        .{ "U8", .u8 },
        .{ "U16", .u16 },
        .{ "U32", .u32 },
        .{ "U64", .u64 },
    });

    fn parseType(self: *Parser) !Type {
        const type_token = try self.expect(.identifier);
        return type_name_map.get(type_token.lexeme) orelse error.UnknownType;
    }

    fn parseStatement(self: *Parser) !Stmt {
        if (self.check(.let_keyword)) {
            return try self.parseLetBinding();
        }
        if (self.check(.return_keyword)) {
            return try self.parseReturnStatement();
        }
        return error.ExpectedStatement;
    }

    fn parseLetBinding(self: *Parser) !Stmt {
        _ = try self.expect(.let_keyword);
        const name_token = try self.expect(.identifier);

        var type_annotation: ?Type = null;
        if (self.check(.colon)) {
            _ = try self.expect(.colon);
            type_annotation = try self.parseType();
        }

        _ = try self.expect(.equal);

        const value_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(value_ptr);
        value_ptr.* = try self.parseExpression();

        _ = try self.expect(.semicolon);

        return .{ .let_binding = .{
            .name = name_token.lexeme,
            .type_annotation = type_annotation,
            .value = value_ptr,
        } };
    }

    fn parseReturnStatement(self: *Parser) !Stmt {
        _ = try self.expect(.return_keyword);
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpression();
        return .{ .return_stmt = expr };
    }

    fn parseExpression(self: *Parser) ParseError!Expr {
        return try self.parseLogicalOr();
    }

    fn makeBinaryOp(self: *Parser, op: BinaryOp, left: Expr, right: Expr) ParseError!Expr {
        const left_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(left_ptr);
        left_ptr.* = left;

        const right_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(right_ptr);
        right_ptr.* = right;

        return .{ .binary_op = .{ .op = op, .left = left_ptr, .right = right_ptr } };
    }

    // Precedence climbing parser
    fn parseLogicalOr(self: *Parser) ParseError!Expr {
        var left = try self.parseLogicalAnd();

        while (self.check(.pipe_pipe)) {
            _ = try self.expect(.pipe_pipe);
            const right = try self.parseLogicalAnd();
            left = try self.makeBinaryOp(.logical_or, left, right);
        }

        return left;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!Expr {
        var left = try self.parseBitwiseOr();

        while (self.check(.ampersand_ampersand)) {
            _ = try self.expect(.ampersand_ampersand);
            const right = try self.parseBitwiseOr();
            left = try self.makeBinaryOp(.logical_and, left, right);
        }

        return left;
    }

    fn parseBitwiseOr(self: *Parser) ParseError!Expr {
        var left = try self.parseBitwiseXor();

        while (self.check(.pipe)) {
            _ = try self.expect(.pipe);
            const right = try self.parseBitwiseXor();
            left = try self.makeBinaryOp(.bitwise_or, left, right);
        }

        return left;
    }

    fn parseBitwiseXor(self: *Parser) ParseError!Expr {
        var left = try self.parseBitwiseAnd();

        while (self.check(.caret)) {
            _ = try self.expect(.caret);
            const right = try self.parseBitwiseAnd();
            left = try self.makeBinaryOp(.bitwise_xor, left, right);
        }

        return left;
    }

    fn parseBitwiseAnd(self: *Parser) ParseError!Expr {
        var left = try self.parseComparison();

        while (self.check(.ampersand)) {
            _ = try self.expect(.ampersand);
            const right = try self.parseComparison();
            left = try self.makeBinaryOp(.bitwise_and, left, right);
        }

        return left;
    }

    fn parseComparison(self: *Parser) ParseError!Expr {
        var left = try self.parseShift();

        while (true) {
            const op: BinaryOp = if (self.check(.equal_equal)) blk: {
                _ = try self.expect(.equal_equal);
                break :blk .equal;
            } else if (self.check(.bang_equal)) blk: {
                _ = try self.expect(.bang_equal);
                break :blk .not_equal;
            } else if (self.check(.less)) blk: {
                _ = try self.expect(.less);
                break :blk .less_than;
            } else if (self.check(.greater)) blk: {
                _ = try self.expect(.greater);
                break :blk .greater_than;
            } else if (self.check(.less_equal)) blk: {
                _ = try self.expect(.less_equal);
                break :blk .less_equal;
            } else if (self.check(.greater_equal)) blk: {
                _ = try self.expect(.greater_equal);
                break :blk .greater_equal;
            } else break;

            const right = try self.parseShift();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseShift(self: *Parser) ParseError!Expr {
        var left = try self.parseTerm();

        while (true) {
            const op: BinaryOp = if (self.check(.less_less)) blk: {
                _ = try self.expect(.less_less);
                break :blk .shift_left;
            } else if (self.check(.greater_greater)) blk: {
                _ = try self.expect(.greater_greater);
                break :blk .shift_right;
            } else break;

            const right = try self.parseTerm();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseTerm(self: *Parser) ParseError!Expr {
        var left = try self.parseFactor();

        while (true) {
            const op: BinaryOp = if (self.check(.plus)) blk: {
                _ = try self.expect(.plus);
                break :blk .add;
            } else if (self.check(.minus)) blk: {
                _ = try self.expect(.minus);
                break :blk .subtract;
            } else break;

            const right = try self.parseFactor();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseFactor(self: *Parser) ParseError!Expr {
        var left = try self.parseUnary();

        while (true) {
            const op: BinaryOp = if (self.check(.star)) blk: {
                _ = try self.expect(.star);
                break :blk .multiply;
            } else if (self.check(.slash)) blk: {
                _ = try self.expect(.slash);
                break :blk .divide;
            } else if (self.check(.percent)) blk: {
                _ = try self.expect(.percent);
                break :blk .modulo;
            } else break;

            const right = try self.parseUnary();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!Expr {
        const op: ?UnaryOp = if (self.check(.minus)) blk: {
            _ = try self.expect(.minus);
            break :blk .negate;
        } else if (self.check(.bang)) blk: {
            _ = try self.expect(.bang);
            break :blk .logical_not;
        } else if (self.check(.tilde)) blk: {
            _ = try self.expect(.tilde);
            break :blk .bitwise_not;
        } else null;

        if (op) |unary_op| {
            const operand_ptr = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(operand_ptr);
            operand_ptr.* = try self.parseUnary();
            return .{ .unary_op = .{ .op = unary_op, .operand = operand_ptr } };
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!Expr {
        if (self.check(.integer)) {
            const token = try self.expect(.integer);
            const value = try std.fmt.parseInt(i32, token.lexeme, 10);
            return .{ .integer_literal = value };
        }

        if (self.check(.identifier)) {
            const token = try self.expect(.identifier);

            // Check if this is a function call
            if (self.check(.left_paren)) {
                _ = try self.expect(.left_paren);

                // Parse arguments
                var args: std.ArrayList(*Expr) = .empty;
                errdefer {
                    for (args.items) |arg| {
                        FunctionDecl.deinitExpr(self.allocator, arg);
                        self.allocator.destroy(arg);
                    }
                    args.deinit(self.allocator);
                }

                if (!self.check(.right_paren)) {
                    while (true) {
                        const arg_expr = try self.parseExpression();
                        const arg_ptr = try self.allocator.create(Expr);
                        arg_ptr.* = arg_expr;
                        try args.append(self.allocator, arg_ptr);

                        if (!self.check(.comma)) break;
                        _ = try self.expect(.comma);
                    }
                }

                _ = try self.expect(.right_paren);

                return .{
                    .function_call = .{
                        .name = token.lexeme,
                        .args = try args.toOwnedSlice(self.allocator),
                    },
                };
            }

            // Just a variable
            return .{ .variable = token.lexeme };
        }

        if (self.check(.left_paren)) {
            _ = try self.expect(.left_paren);
            const expr = try self.parseExpression();
            _ = try self.expect(.right_paren);
            return expr;
        }

        return error.ExpectedExpression;
    }

    fn expect(self: *Parser, kind: TokenKind) !Token {
        if (self.isAtEnd()) return error.UnexpectedEndOfFile;
        const token = self.current();
        if (token.kind != kind) {
            std.debug.print("Expected {s}, got {s} at line {d}\n", .{
                @tagName(kind),
                @tagName(token.kind),
                token.line,
            });
            return error.UnexpectedToken;
        }
        self.pos += 1;
        return token;
    }

    fn check(self: *Parser, kind: TokenKind) bool {
        if (self.isAtEnd()) return false;
        return self.current().kind == kind;
    }

    fn current(self: *Parser) Token {
        return self.tokens[self.pos];
    }

    fn isAtEnd(self: *Parser) bool {
        return self.pos >= self.tokens.len or self.current().kind == .eof;
    }
};

test "parser: simple function" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() -> I32 { return 42 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 1), ast.functions.items.len);

    const func = ast.functions.items[0];
    try testing.expectEqualStrings("main", func.name);
    try testing.expectEqual(Type.i32, func.return_type);
    try testing.expectEqual(@as(usize, 1), func.body.items.len);

    const stmt = func.body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .integer_literal);
    try testing.expectEqual(@as(i32, 42), stmt.return_stmt.integer_literal);
}

test "parser: binary addition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() -> I32 { return 1 + 2 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    const stmt = func.body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, stmt.return_stmt.binary_op.op);
    try testing.expectEqual(@as(i32, 1), stmt.return_stmt.binary_op.left.integer_literal);
    try testing.expectEqual(@as(i32, 2), stmt.return_stmt.binary_op.right.integer_literal);
}

test "parser: operator precedence" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() -> I32 { return 1 + 2 * 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    const stmt = func.body.items[0];
    // Should parse as 1 + (2 * 3)
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, stmt.return_stmt.binary_op.op);
    try testing.expectEqual(@as(i32, 1), stmt.return_stmt.binary_op.left.integer_literal);

    const right = stmt.return_stmt.binary_op.right;
    try testing.expect(right.* == .binary_op);
    try testing.expectEqual(BinaryOp.multiply, right.binary_op.op);
    try testing.expectEqual(@as(i32, 2), right.binary_op.left.integer_literal);
    try testing.expectEqual(@as(i32, 3), right.binary_op.right.integer_literal);
}

test "parser: arithmetic operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() -> I32 { return 5 - 3 }", .op = .subtract },
        .{ .src = "fn f() -> I32 { return 5 * 3 }", .op = .multiply },
        .{ .src = "fn f() -> I32 { return 5 / 3 }", .op = .divide },
        .{ .src = "fn f() -> I32 { return 5 % 3 }", .op = .modulo },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: comparison operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() -> I32 { return 5 == 3 }", .op = .equal },
        .{ .src = "fn f() -> I32 { return 5 != 3 }", .op = .not_equal },
        .{ .src = "fn f() -> I32 { return 5 < 3 }", .op = .less_than },
        .{ .src = "fn f() -> I32 { return 5 > 3 }", .op = .greater_than },
        .{ .src = "fn f() -> I32 { return 5 <= 3 }", .op = .less_equal },
        .{ .src = "fn f() -> I32 { return 5 >= 3 }", .op = .greater_equal },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: logical operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() -> I32 { return 5 && 3 }", .op = .logical_and },
        .{ .src = "fn f() -> I32 { return 5 || 3 }", .op = .logical_or },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: bitwise operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() -> I32 { return 5 & 3 }", .op = .bitwise_and },
        .{ .src = "fn f() -> I32 { return 5 | 3 }", .op = .bitwise_or },
        .{ .src = "fn f() -> I32 { return 5 ^ 3 }", .op = .bitwise_xor },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: shift operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() -> I32 { return 5 << 3 }", .op = .shift_left },
        .{ .src = "fn f() -> I32 { return 5 >> 3 }", .op = .shift_right },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: parentheses" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() -> I32 { return (1 + 2) * 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    // Should parse as (1 + 2) * 3
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.multiply, stmt.return_stmt.binary_op.op);
    try testing.expectEqual(@as(i32, 3), stmt.return_stmt.binary_op.right.integer_literal);

    const left = stmt.return_stmt.binary_op.left;
    try testing.expect(left.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, left.binary_op.op);
    try testing.expectEqual(@as(i32, 1), left.binary_op.left.integer_literal);
    try testing.expectEqual(@as(i32, 2), left.binary_op.right.integer_literal);
}

test "parser: unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: UnaryOp }{
        .{ .src = "fn f() -> I32 { return -5 }", .op = .negate },
        .{ .src = "fn f() -> I32 { return !5 }", .op = .logical_not },
        .{ .src = "fn f() -> I32 { return ~5 }", .op = .bitwise_not },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expect(stmt.return_stmt.* == .unary_op);
        try testing.expectEqual(case.op, stmt.return_stmt.unary_op.op);
        try testing.expectEqual(@as(i32, 5), stmt.return_stmt.unary_op.operand.integer_literal);
    }
}

test "parser: nested unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() -> I32 { return --5 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    // Should parse as -(-5)
    try testing.expect(stmt.return_stmt.* == .unary_op);
    try testing.expectEqual(UnaryOp.negate, stmt.return_stmt.unary_op.op);

    const inner = stmt.return_stmt.unary_op.operand;
    try testing.expect(inner.* == .unary_op);
    try testing.expectEqual(UnaryOp.negate, inner.unary_op.op);
    try testing.expectEqual(@as(i32, 5), inner.unary_op.operand.integer_literal);
}

test "parser: unary with binary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() -> I32 { return -5 + 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    // Should parse as (-5) + 3
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, stmt.return_stmt.binary_op.op);

    const left = stmt.return_stmt.binary_op.left;
    try testing.expect(left.* == .unary_op);
    try testing.expectEqual(UnaryOp.negate, left.unary_op.op);
    try testing.expectEqual(@as(i32, 5), left.unary_op.operand.integer_literal);

    try testing.expectEqual(@as(i32, 3), stmt.return_stmt.binary_op.right.integer_literal);
}

