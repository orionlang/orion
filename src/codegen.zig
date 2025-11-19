const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;

pub const Codegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) Codegen {
        return .{
            .allocator = allocator,
            .output = .empty,
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
        }
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

