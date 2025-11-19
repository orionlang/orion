const std = @import("std");

pub const TokenKind = enum {
    // Keywords
    fn_keyword,
    return_keyword,

    // Literals
    integer,

    // Identifiers
    identifier,

    // Punctuation
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    arrow, // ->

    // Special
    eof,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    line: usize,
    column: usize,
};

pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: usize,
    column: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
        };
    }

    pub fn tokenize(self: *Lexer, allocator: std.mem.Allocator) !std.ArrayList(Token) {
        var tokens: std.ArrayList(Token) = .empty;
        errdefer tokens.deinit(allocator);

        while (!self.isAtEnd()) {
            self.skipWhitespace();
            if (self.isAtEnd()) break;

            const token = try self.nextToken();
            try tokens.append(allocator, token);
        }

        try tokens.append(allocator, .{
            .kind = .eof,
            .lexeme = "",
            .line = self.line,
            .column = self.column,
        });

        return tokens;
    }

    fn nextToken(self: *Lexer) !Token {
        const start_line = self.line;
        const start_column = self.column;
        const start_pos = self.pos;

        const c = self.advance();

        switch (c) {
            '(' => return self.makeToken(.left_paren, start_pos, start_line, start_column),
            ')' => return self.makeToken(.right_paren, start_pos, start_line, start_column),
            '{' => return self.makeToken(.left_brace, start_pos, start_line, start_column),
            '}' => return self.makeToken(.right_brace, start_pos, start_line, start_column),
            '-' => {
                if (self.match('>')) {
                    return self.makeToken(.arrow, start_pos, start_line, start_column);
                }
                return error.UnexpectedCharacter;
            },
            '0'...'9' => {
                while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.integer, start_pos, start_line, start_column);
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (!self.isAtEnd() and self.isIdentifierChar(self.peek())) {
                    _ = self.advance();
                }
                const lexeme = self.source[start_pos..self.pos];
                const kind = self.identifierKind(lexeme);
                return self.makeToken(kind, start_pos, start_line, start_column);
            },
            else => return error.UnexpectedCharacter,
        }
    }

    fn makeToken(self: *Lexer, kind: TokenKind, start_pos: usize, line: usize, column: usize) Token {
        return .{
            .kind = kind,
            .lexeme = self.source[start_pos..self.pos],
            .line = line,
            .column = column,
        };
    }

    fn identifierKind(self: *Lexer, lexeme: []const u8) TokenKind {
        _ = self;
        if (std.mem.eql(u8, lexeme, "fn")) return .fn_keyword;
        if (std.mem.eql(u8, lexeme, "return")) return .return_keyword;
        return .identifier;
    }

    fn isIdentifierChar(self: *Lexer, c: u8) bool {
        _ = self;
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => _ = self.advance(),
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 1;
                },
                '/' => {
                    if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '/') {
                        // Line comment
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    } else {
                        break;
                    }
                },
                else => break,
            }
        }
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        self.column += 1;
        return c;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != expected) return false;
        _ = self.advance();
        return true;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.pos >= self.source.len;
    }
};

test "lexer: simple function" {
    const testing = std.testing;
    const source = "fn main() -> I32 { return 42 }";
    var lexer = Lexer.init(source);

    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    // fn main ( ) -> I32 { return 42 } EOF
    try testing.expectEqual(@as(usize, 11), tokens.items.len);
    try testing.expectEqual(TokenKind.fn_keyword, tokens.items[0].kind);
    try testing.expectEqual(TokenKind.identifier, tokens.items[1].kind);
    try testing.expectEqualStrings("main", tokens.items[1].lexeme);
    try testing.expectEqual(TokenKind.left_paren, tokens.items[2].kind);
    try testing.expectEqual(TokenKind.right_paren, tokens.items[3].kind);
    try testing.expectEqual(TokenKind.arrow, tokens.items[4].kind);
    try testing.expectEqual(TokenKind.identifier, tokens.items[5].kind);
    try testing.expectEqualStrings("I32", tokens.items[5].lexeme);
    try testing.expectEqual(TokenKind.left_brace, tokens.items[6].kind);
    try testing.expectEqual(TokenKind.return_keyword, tokens.items[7].kind);
    try testing.expectEqual(TokenKind.integer, tokens.items[8].kind);
    try testing.expectEqualStrings("42", tokens.items[8].lexeme);
    try testing.expectEqual(TokenKind.right_brace, tokens.items[9].kind);
}

test "lexer: integers" {
    const testing = std.testing;
    const source = "42 123 0";
    var lexer = Lexer.init(source);

    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 4), tokens.items.len); // 3 + EOF
    try testing.expectEqualStrings("42", tokens.items[0].lexeme);
    try testing.expectEqualStrings("123", tokens.items[1].lexeme);
    try testing.expectEqualStrings("0", tokens.items[2].lexeme);
}

test "lexer: keywords" {
    const testing = std.testing;
    const source = "fn return";
    var lexer = Lexer.init(source);

    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    try testing.expectEqual(TokenKind.fn_keyword, tokens.items[0].kind);
    try testing.expectEqual(TokenKind.return_keyword, tokens.items[1].kind);
}

test "lexer: line comments" {
    const testing = std.testing;
    const source =
        \\fn main() // this is a comment
        \\{ return 42 }
    ;
    var lexer = Lexer.init(source);

    var tokens = try lexer.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    // Should skip the comment
    // fn main ( ) I32 { return 42 } EOF
    try testing.expectEqual(@as(usize, 9), tokens.items.len);
    try testing.expectEqual(TokenKind.fn_keyword, tokens.items[0].kind);
    try testing.expectEqual(TokenKind.left_brace, tokens.items[4].kind);
}

