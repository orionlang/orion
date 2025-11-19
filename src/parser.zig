const std = @import("std");
const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenKind = lexer.TokenKind;

pub const Type = union(enum) {
    i32,
    // More types later
};

pub const Expr = union(enum) {
    integer_literal: i32,
    // More expressions later
};

pub const Stmt = union(enum) {
    return_stmt: *Expr,
    // More statements later
};

pub const FunctionDecl = struct {
    name: []const u8,
    params: []const u8, // Empty for now
    return_type: Type,
    body: std.ArrayList(Stmt),

    pub fn deinit(self: *FunctionDecl, allocator: std.mem.Allocator) void {
        for (self.body.items) |*stmt| {
            switch (stmt.*) {
                .return_stmt => |expr| allocator.destroy(expr),
            }
        }
        self.body.deinit(allocator);
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
        // fn
        _ = try self.expect(.fn_keyword);

        // name
        const name_token = try self.expect(.identifier);
        const name = name_token.lexeme;

        // ()
        _ = try self.expect(.left_paren);
        _ = try self.expect(.right_paren);

        // -> I32
        _ = try self.expect(.arrow);
        const return_type = try self.parseType();

        // { body }
        _ = try self.expect(.left_brace);
        var body: std.ArrayList(Stmt) = .empty;
        errdefer body.deinit(self.allocator);

        // For now, just parse a single return statement
        const stmt = try self.parseStatement();
        try body.append(self.allocator, stmt);

        _ = try self.expect(.right_brace);

        return FunctionDecl{
            .name = name,
            .params = "",
            .return_type = return_type,
            .body = body,
        };
    }

    fn parseType(self: *Parser) !Type {
        const type_token = try self.expect(.identifier);
        if (std.mem.eql(u8, type_token.lexeme, "I32")) {
            return .i32;
        }
        return error.UnknownType;
    }

    fn parseStatement(self: *Parser) !Stmt {
        if (self.check(.return_keyword)) {
            return try self.parseReturnStatement();
        }
        return error.ExpectedStatement;
    }

    fn parseReturnStatement(self: *Parser) !Stmt {
        _ = try self.expect(.return_keyword);
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpression();
        return .{ .return_stmt = expr };
    }

    fn parseExpression(self: *Parser) !Expr {
        const token = try self.expect(.integer);
        const value = try std.fmt.parseInt(i32, token.lexeme, 10);
        return .{ .integer_literal = value };
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

