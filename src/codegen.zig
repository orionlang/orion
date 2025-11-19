const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;

pub const Codegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    next_temp: usize,

    pub fn init(allocator: std.mem.Allocator) Codegen {
        return .{
            .allocator = allocator,
            .output = .empty,
            .next_temp = 0,
        };
    }

    pub fn deinit(self: *Codegen) void {
        self.output.deinit(self.allocator);
    }

    pub fn generate(self: *Codegen, ast: *const AST) ![]const u8 {
        // LLVM IR header
        try self.output.appendSlice(self.allocator, "; Bootstrap Orion Compiler Output\n");
        try self.output.appendSlice(self.allocator, "target triple = \"x86_64-pc-linux-gnu\"\n\n");

        // Generate each function
        for (ast.functions.items) |func| {
            try self.generateFunction(&func);
        }

        return try self.allocator.dupe(u8, self.output.items);
    }

    fn generateFunction(self: *Codegen, func: *const parser.FunctionDecl) !void {
        // Function signature
        const return_type_str = self.llvmType(func.return_type);
        try self.output.writer(self.allocator).print("define {s} @{s}() {{\n", .{ return_type_str, func.name });
        try self.output.appendSlice(self.allocator, "entry:\n");

        // Generate body
        for (func.body.items) |stmt| {
            try self.generateStatement(&stmt);
        }

        try self.output.appendSlice(self.allocator, "}\n\n");
    }

    fn generateStatement(self: *Codegen, stmt: *const Stmt) !void {
        switch (stmt.*) {
            .return_stmt => |expr| {
                const value = try self.generateExpression(expr);
                defer self.allocator.free(value);
                const type_str = self.llvmTypeForExpr(expr);
                try self.output.writer(self.allocator).print("  ret {s} {s}\n", .{ type_str, value });
            },
        }
    }

    fn generateExpression(self: *Codegen, expr: *const Expr) ![]const u8 {
        switch (expr.*) {
            .integer_literal => |value| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{value});
            },
            .binary_op => |binop| {
                const left_val = try self.generateExpression(binop.left);
                defer self.allocator.free(left_val);
                const right_val = try self.generateExpression(binop.right);
                defer self.allocator.free(right_val);

                const temp_name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                defer self.allocator.free(temp_name);
                self.next_temp += 1;

                const op_str = self.llvmBinaryOp(binop.op);
                try self.output.writer(self.allocator).print("  {s} = {s} i32 {s}, {s}\n", .{ temp_name, op_str, left_val, right_val });

                return try self.allocator.dupe(u8, temp_name);
            },
        }
    }

    fn llvmType(self: *Codegen, typ: Type) []const u8 {
        _ = self;
        switch (typ) {
            .i32 => return "i32",
        }
    }

    fn llvmTypeForExpr(self: *Codegen, expr: *const Expr) []const u8 {
        _ = self;
        switch (expr.*) {
            .integer_literal => return "i32",
            .binary_op => return "i32",
        }
    }

    fn llvmBinaryOp(self: *Codegen, op: parser.BinaryOp) []const u8 {
        _ = self;
        return switch (op) {
            .add => "add",
            .subtract => "sub",
            .multiply => "mul",
            .divide => "sdiv",
            .modulo => "srem",
            .equal => "icmp eq",
            .not_equal => "icmp ne",
            .less_than => "icmp slt",
            .greater_than => "icmp sgt",
            .less_equal => "icmp sle",
            .greater_equal => "icmp sge",
            .logical_and => "and",
            .logical_or => "or",
            .bitwise_and => "and",
            .bitwise_or => "or",
            .bitwise_xor => "xor",
            .shift_left => "shl",
            .shift_right => "ashr",
        };
    }
};

test "codegen: simple function" {
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

    var codegen = Codegen.init(testing.allocator);
    defer codegen.deinit();

    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    // Check that output contains expected LLVM IR
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "define i32 @main()") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 42") != null);
}

test "codegen: binary addition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn main() -> I32 { return 1 + 2 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    var codegen = Codegen.init(testing.allocator);
    defer codegen.deinit();

    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    try testing.expect(std.mem.indexOf(u8, llvm_ir, "add i32 1, 2") != null);
}

test "codegen: all arithmetic operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_]struct { src: []const u8, expected: []const u8 }{
        .{ .src = "fn f() -> I32 { return 10 - 5 }", .expected = "sub i32 10, 5" },
        .{ .src = "fn f() -> I32 { return 10 * 5 }", .expected = "mul i32 10, 5" },
        .{ .src = "fn f() -> I32 { return 10 / 5 }", .expected = "sdiv i32 10, 5" },
        .{ .src = "fn f() -> I32 { return 10 % 5 }", .expected = "srem i32 10, 5" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        var codegen = Codegen.init(testing.allocator);
        defer codegen.deinit();

        const llvm_ir = try codegen.generate(&ast);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected) != null);
    }
}

test "codegen: comparison operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_]struct { src: []const u8, expected: []const u8 }{
        .{ .src = "fn f() -> I32 { return 5 == 3 }", .expected = "icmp eq i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 != 3 }", .expected = "icmp ne i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 < 3 }", .expected = "icmp slt i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 > 3 }", .expected = "icmp sgt i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 <= 3 }", .expected = "icmp sle i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 >= 3 }", .expected = "icmp sge i32 5, 3" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        var codegen = Codegen.init(testing.allocator);
        defer codegen.deinit();

        const llvm_ir = try codegen.generate(&ast);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected) != null);
    }
}

test "codegen: bitwise and shift operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_]struct { src: []const u8, expected: []const u8 }{
        .{ .src = "fn f() -> I32 { return 5 & 3 }", .expected = "and i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 | 3 }", .expected = "or i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 ^ 3 }", .expected = "xor i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 << 3 }", .expected = "shl i32 5, 3" },
        .{ .src = "fn f() -> I32 { return 5 >> 3 }", .expected = "ashr i32 5, 3" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        var codegen = Codegen.init(testing.allocator);
        defer codegen.deinit();

        const llvm_ir = try codegen.generate(&ast);
        defer testing.allocator.free(llvm_ir);

        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected) != null);
    }
}

