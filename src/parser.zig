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

    pub fn minValue(self: Type) i64 {
        if (!self.isInteger()) {
            if (self == .bool) return 0;
            return 0;
        }
        if (!self.isSigned()) return 0; // Unsigned types have min of 0

        // Signed types: -2^(bits-1)
        return switch (self) {
            .i8 => -128,
            .i16 => -32768,
            .i32 => -2147483648,
            .i64 => std.math.minInt(i64),
            else => 0,
        };
    }

    pub fn maxValue(self: Type) i64 {
        return switch (self) {
            .bool => 1,
            .i8 => 127,
            .i16 => 32767,
            .i32 => 2147483647,
            .i64 => std.math.maxInt(i64),
            .u8 => 255,
            .u16 => 65535,
            .u32 => 4294967295,
            .u64 => std.math.maxInt(i64), // Limited by i64 storage
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
    integer_literal: struct {
        value: i64,
        inferred_type: Type,
    },
    bool_literal: bool,
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
    if_expr: struct {
        condition: *Expr,
        then_branch: *Expr,
        else_branch: ?*Expr,
    },
    block_expr: struct {
        statements: []Stmt,
        result: ?*Expr,
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
        mutable: bool,
    },
    assignment: struct {
        name: []const u8,
        value: *Expr,
    },
    while_stmt: struct {
        condition: *Expr,
        body: *Expr,
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
            deinitStmt(allocator, stmt);
        }
        self.body.deinit(allocator);
    }

    fn deinitStmt(allocator: std.mem.Allocator, stmt: *Stmt) void {
        switch (stmt.*) {
            .let_binding => |binding| {
                deinitExpr(allocator, binding.value);
                allocator.destroy(binding.value);
            },
            .assignment => |assign| {
                deinitExpr(allocator, assign.value);
                allocator.destroy(assign.value);
            },
            .while_stmt => |while_stmt| {
                deinitExpr(allocator, while_stmt.condition);
                allocator.destroy(while_stmt.condition);
                deinitExpr(allocator, while_stmt.body);
                allocator.destroy(while_stmt.body);
            },
            .return_stmt => |expr| {
                deinitExpr(allocator, expr);
                allocator.destroy(expr);
            },
        }
    }

    fn deinitExpr(allocator: std.mem.Allocator, expr: *Expr) void {
        switch (expr.*) {
            .integer_literal => {},
            .bool_literal => {},
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
            .if_expr => |if_expr| {
                deinitExpr(allocator, if_expr.condition);
                allocator.destroy(if_expr.condition);
                deinitExpr(allocator, if_expr.then_branch);
                allocator.destroy(if_expr.then_branch);
                if (if_expr.else_branch) |else_branch| {
                    deinitExpr(allocator, else_branch);
                    allocator.destroy(else_branch);
                }
            },
            .block_expr => |block| {
                for (block.statements) |*stmt| {
                    deinitStmt(allocator, stmt);
                }
                allocator.free(block.statements);
                if (block.result) |result| {
                    deinitExpr(allocator, result);
                    allocator.destroy(result);
                }
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

    fn skipOptionalSemicolon(self: *Parser) void {
        if (self.check(.semicolon)) {
            _ = self.expect(.semicolon) catch {};
        }
    }

    fn isStatementStart(self: *Parser) bool {
        return self.check(.let_keyword) or
               self.check(.var_keyword) or
               self.check(.return_keyword) or
               self.check(.while_keyword) or
               (self.check(.identifier) and self.peekAhead(1).kind == .equal);
    }

    fn parseStatement(self: *Parser) !Stmt {
        if (self.check(.let_keyword)) {
            return try self.parseLetBinding(false);
        }
        if (self.check(.var_keyword)) {
            return try self.parseLetBinding(true);
        }
        if (self.check(.return_keyword)) {
            return try self.parseReturnStatement();
        }
        if (self.check(.while_keyword)) {
            return try self.parseWhileStatement();
        }
        // Check for assignment: identifier = expr;
        if (self.check(.identifier) and self.peekAhead(1).kind == .equal) {
            return try self.parseAssignment();
        }
        return error.ExpectedStatement;
    }

    fn parseAssignment(self: *Parser) !Stmt {
        const name_token = try self.expect(.identifier);
        _ = try self.expect(.equal);

        const value_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(value_ptr);
        value_ptr.* = try self.parseExpression();

        self.skipOptionalSemicolon();

        return .{ .assignment = .{
            .name = name_token.lexeme,
            .value = value_ptr,
        } };
    }

    fn parseLetBinding(self: *Parser, mutable: bool) !Stmt {
        if (mutable) {
            _ = try self.expect(.var_keyword);
        } else {
            _ = try self.expect(.let_keyword);
        }
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

        self.skipOptionalSemicolon();

        return .{ .let_binding = .{
            .name = name_token.lexeme,
            .type_annotation = type_annotation,
            .value = value_ptr,
            .mutable = mutable,
        } };
    }

    fn parseReturnStatement(self: *Parser) !Stmt {
        _ = try self.expect(.return_keyword);
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpression();
        return .{ .return_stmt = expr };
    }

    fn parseWhileStatement(self: *Parser) !Stmt {
        _ = try self.expect(.while_keyword);

        // Parse condition
        const condition = try self.parseExpressionPtr(&.{});

        // Parse body (must be block expression)
        const body = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(body);
        body.* = try self.parseBlockExpression();

        self.skipOptionalSemicolon();

        return .{
            .while_stmt = .{
                .condition = condition,
                .body = body,
            },
        };
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
            const value = try std.fmt.parseInt(i64, token.lexeme, 10);
            return .{ .integer_literal = .{ .value = value, .inferred_type = .i32 } };
        }

        if (self.check(.true_keyword)) {
            _ = try self.expect(.true_keyword);
            return .{ .bool_literal = true };
        }

        if (self.check(.false_keyword)) {
            _ = try self.expect(.false_keyword);
            return .{ .bool_literal = false };
        }

        if (self.check(.if_keyword)) {
            return try self.parseIfExpression();
        }

        if (self.check(.left_brace)) {
            return try self.parseBlockExpression();
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

    fn parseExpressionPtr(self: *Parser, already_allocated: []const *Expr) !*Expr {
        const expr = try self.parseExpression();
        const ptr = try self.allocator.create(Expr);
        errdefer {
            for (already_allocated) |allocated| {
                FunctionDecl.deinitExpr(self.allocator, allocated);
                self.allocator.destroy(allocated);
            }
            self.allocator.destroy(ptr);
        }
        ptr.* = expr;
        return ptr;
    }

    fn parseIfExpression(self: *Parser) ParseError!Expr {
        _ = try self.expect(.if_keyword);
        return try self.parseIfExpressionContinuation();
    }

    fn parseIfExpressionContinuation(self: *Parser) ParseError!Expr {
        // Parse condition
        const condition = try self.parseExpressionPtr(&.{});

        // Parse then branch
        _ = try self.expect(.left_brace);
        const then_branch = try self.parseExpressionPtr(&.{condition});
        _ = try self.expect(.right_brace);

        // Parse optional elseif/else branch
        const else_branch = blk: {
            if (self.check(.elseif_keyword)) {
                // elseif becomes a nested if expression
                _ = try self.expect(.elseif_keyword);
                const nested_if = try self.parseIfExpressionContinuation();
                const else_ptr = try self.allocator.create(Expr);
                else_ptr.* = nested_if;
                break :blk else_ptr;
            } else if (self.check(.else_keyword)) {
                _ = try self.expect(.else_keyword);
                _ = try self.expect(.left_brace);
                const else_ptr = try self.parseExpressionPtr(&.{ condition, then_branch });
                _ = try self.expect(.right_brace);
                break :blk else_ptr;
            } else {
                break :blk null;
            }
        };
        errdefer {
            if (else_branch) |eb| {
                FunctionDecl.deinitExpr(self.allocator, eb);
                self.allocator.destroy(eb);
            }
        }

        return .{
            .if_expr = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        };
    }

    fn parseBlockExpression(self: *Parser) ParseError!Expr {
        _ = try self.expect(.left_brace);

        var stmts: std.ArrayList(Stmt) = .empty;
        errdefer {
            for (stmts.items) |*stmt| {
                FunctionDecl.deinitStmt(self.allocator, stmt);
            }
            stmts.deinit(self.allocator);
        }

        // Parse statements until we hit right_brace or final expression
        while (!self.check(.right_brace)) {
            if (self.isStatementStart()) {
                const stmt = try self.parseStatement();
                try stmts.append(self.allocator, stmt);
            } else {
                // It's a final expression (no semicolon)
                const result_ptr = try self.allocator.create(Expr);
                errdefer self.allocator.destroy(result_ptr);
                result_ptr.* = try self.parseExpression();
                _ = try self.expect(.right_brace);
                return .{
                    .block_expr = .{
                        .statements = try stmts.toOwnedSlice(self.allocator),
                        .result = result_ptr,
                    },
                };
            }
        }

        _ = try self.expect(.right_brace);
        return .{
            .block_expr = .{
                .statements = try stmts.toOwnedSlice(self.allocator),
                .result = null,
            },
        };
    }

    fn peekAhead(self: *Parser, n: usize) Token {
        const target_pos = self.pos + n;
        if (target_pos >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1]; // Return EOF token
        }
        return self.tokens[target_pos];
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

    const source = "fn main() I32 { return 42 }";
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
    try testing.expectEqual(@as(i64, 42), stmt.return_stmt.integer_literal.value);
}

test "parser: binary addition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return 1 + 2 }";
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
    try testing.expectEqual(@as(i64, 1), stmt.return_stmt.binary_op.left.integer_literal.value);
    try testing.expectEqual(@as(i64, 2), stmt.return_stmt.binary_op.right.integer_literal.value);
}

test "parser: operator precedence" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return 1 + 2 * 3 }";
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
    try testing.expectEqual(@as(i64, 1), stmt.return_stmt.binary_op.left.integer_literal.value);

    const right = stmt.return_stmt.binary_op.right;
    try testing.expect(right.* == .binary_op);
    try testing.expectEqual(BinaryOp.multiply, right.binary_op.op);
    try testing.expectEqual(@as(i64, 2), right.binary_op.left.integer_literal.value);
    try testing.expectEqual(@as(i64, 3), right.binary_op.right.integer_literal.value);
}

test "parser: arithmetic operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() I32 { return 5 - 3 }", .op = .subtract },
        .{ .src = "fn f() I32 { return 5 * 3 }", .op = .multiply },
        .{ .src = "fn f() I32 { return 5 / 3 }", .op = .divide },
        .{ .src = "fn f() I32 { return 5 % 3 }", .op = .modulo },
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
        .{ .src = "fn f() I32 { return 5 == 3 }", .op = .equal },
        .{ .src = "fn f() I32 { return 5 != 3 }", .op = .not_equal },
        .{ .src = "fn f() I32 { return 5 < 3 }", .op = .less_than },
        .{ .src = "fn f() I32 { return 5 > 3 }", .op = .greater_than },
        .{ .src = "fn f() I32 { return 5 <= 3 }", .op = .less_equal },
        .{ .src = "fn f() I32 { return 5 >= 3 }", .op = .greater_equal },
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
        .{ .src = "fn f() I32 { return 5 && 3 }", .op = .logical_and },
        .{ .src = "fn f() I32 { return 5 || 3 }", .op = .logical_or },
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
        .{ .src = "fn f() I32 { return 5 & 3 }", .op = .bitwise_and },
        .{ .src = "fn f() I32 { return 5 | 3 }", .op = .bitwise_or },
        .{ .src = "fn f() I32 { return 5 ^ 3 }", .op = .bitwise_xor },
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
        .{ .src = "fn f() I32 { return 5 << 3 }", .op = .shift_left },
        .{ .src = "fn f() I32 { return 5 >> 3 }", .op = .shift_right },
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

    const source = "fn main() I32 { return (1 + 2) * 3 }";
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
    try testing.expectEqual(@as(i64, 3), stmt.return_stmt.binary_op.right.integer_literal.value);

    const left = stmt.return_stmt.binary_op.left;
    try testing.expect(left.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, left.binary_op.op);
    try testing.expectEqual(@as(i64, 1), left.binary_op.left.integer_literal.value);
    try testing.expectEqual(@as(i64, 2), left.binary_op.right.integer_literal.value);
}

test "parser: unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: UnaryOp }{
        .{ .src = "fn f() I32 { return -5 }", .op = .negate },
        .{ .src = "fn f() I32 { return !5 }", .op = .logical_not },
        .{ .src = "fn f() I32 { return ~5 }", .op = .bitwise_not },
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
        try testing.expectEqual(@as(i64, 5), stmt.return_stmt.unary_op.operand.integer_literal.value);
    }
}

test "parser: nested unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return --5 }";
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
    try testing.expectEqual(@as(i64, 5), inner.unary_op.operand.integer_literal.value);
}

test "parser: unary with binary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return -5 + 3 }";
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
    try testing.expectEqual(@as(i64, 5), left.unary_op.operand.integer_literal.value);

    try testing.expectEqual(@as(i64, 3), stmt.return_stmt.binary_op.right.integer_literal.value);
}

test "parser: bool literals" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() Bool { return true }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .bool_literal);
    try testing.expectEqual(true, stmt.return_stmt.bool_literal);
}

test "parser: simple if expression" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if true { 1 } else { 2 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const if_expr = stmt.return_stmt.if_expr;
    // Check condition
    try testing.expect(if_expr.condition.* == .bool_literal);
    try testing.expectEqual(true, if_expr.condition.bool_literal);

    // Check then branch
    try testing.expect(if_expr.then_branch.* == .integer_literal);
    try testing.expectEqual(@as(i64, 1), if_expr.then_branch.integer_literal.value);

    // Check else branch
    try testing.expect(if_expr.else_branch != null);
    try testing.expect(if_expr.else_branch.?.* == .integer_literal);
    try testing.expectEqual(@as(i64, 2), if_expr.else_branch.?.integer_literal.value);
}

test "parser: if expression with comparison condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if 10 > 5 { 1 } else { 0 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const if_expr = stmt.return_stmt.if_expr;
    // Condition should be binary_op
    try testing.expect(if_expr.condition.* == .binary_op);
    try testing.expectEqual(BinaryOp.greater_than, if_expr.condition.binary_op.op);
}

test "parser: elseif chain" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const if_expr = stmt.return_stmt.if_expr;
    // First condition
    try testing.expect(if_expr.condition.* == .bool_literal);
    try testing.expectEqual(false, if_expr.condition.bool_literal);

    // Then branch
    try testing.expectEqual(@as(i64, 1), if_expr.then_branch.integer_literal.value);

    // Else branch should be another if_expr (the elseif)
    try testing.expect(if_expr.else_branch != null);
    try testing.expect(if_expr.else_branch.?.* == .if_expr);

    const nested_if = if_expr.else_branch.?.if_expr;
    try testing.expect(nested_if.condition.* == .bool_literal);
    try testing.expectEqual(true, nested_if.condition.bool_literal);
    try testing.expectEqual(@as(i64, 2), nested_if.then_branch.integer_literal.value);
    try testing.expectEqual(@as(i64, 3), nested_if.else_branch.?.integer_literal.value);
}

test "parser: multiple elseif chain" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if false { 1 } elseif false { 2 } elseif true { 3 } else { 4 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    // Should create deeply nested if expressions
    const if1 = stmt.return_stmt.if_expr;
    try testing.expectEqual(@as(i64, 1), if1.then_branch.integer_literal.value);

    // First elseif
    try testing.expect(if1.else_branch.?.* == .if_expr);
    const if2 = if1.else_branch.?.if_expr;
    try testing.expectEqual(@as(i64, 2), if2.then_branch.integer_literal.value);

    // Second elseif
    try testing.expect(if2.else_branch.?.* == .if_expr);
    const if3 = if2.else_branch.?.if_expr;
    try testing.expectEqual(@as(i64, 3), if3.then_branch.integer_literal.value);

    // Final else
    try testing.expectEqual(@as(i64, 4), if3.else_branch.?.integer_literal.value);
}

test "parser: nested if expressions" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const outer_if = stmt.return_stmt.if_expr;
    // Then branch should be another if_expr
    try testing.expect(outer_if.then_branch.* == .if_expr);

    const inner_if = outer_if.then_branch.if_expr;
    try testing.expectEqual(@as(i64, 1), inner_if.then_branch.integer_literal.value);
    try testing.expectEqual(@as(i64, 2), inner_if.else_branch.?.integer_literal.value);

    // Outer else
    try testing.expectEqual(@as(i64, 3), outer_if.else_branch.?.integer_literal.value);
}

test "parser: simple while loop" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { while true { 42 }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .while_stmt);

    const while_loop = stmt.while_stmt;
    try testing.expect(while_loop.condition.* == .bool_literal);
    try testing.expectEqual(true, while_loop.condition.bool_literal);
    try testing.expect(while_loop.body.* == .block_expr);
}

test "parser: while loop with comparison condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { while 5 > 3 { 1 }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .while_stmt);

    const while_loop = stmt.while_stmt;
    try testing.expect(while_loop.condition.* == .binary_op);
    try testing.expectEqual(BinaryOp.greater_than, while_loop.condition.binary_op.op);
}

test "parser: nested while loops" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { while true { while false { 1 } }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .while_stmt);

    const outer_while = stmt.while_stmt;
    try testing.expect(outer_while.body.* == .block_expr);
    const outer_body = outer_while.body.block_expr;
    try testing.expectEqual(@as(usize, 1), outer_body.statements.len);
    try testing.expect(outer_body.statements[0] == .while_stmt);

    const inner_while = outer_body.statements[0].while_stmt;
    try testing.expect(inner_while.condition.* == .bool_literal);
    try testing.expectEqual(false, inner_while.condition.bool_literal);
}

