const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;

const VarInfo = struct {
    llvm_ptr: []const u8,
    var_type: Type,
};

pub const Codegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    next_temp: usize,
    variables: std.StringHashMap(VarInfo),

    pub fn init(allocator: std.mem.Allocator) Codegen {
        return .{
            .allocator = allocator,
            .output = .empty,
            .next_temp = 0,
            .variables = std.StringHashMap(VarInfo).init(allocator),
        };
    }

    pub fn deinit(self: *Codegen) void {
        // Free all allocated variable names
        var iter = self.variables.valueIterator();
        while (iter.next()) |var_info| {
            self.allocator.free(var_info.llvm_ptr);
        }

        self.output.deinit(self.allocator);
        self.variables.deinit();
    }

    pub fn generate(self: *Codegen, ast: *const AST) ![]const u8 {
        // LLVM IR header
        try self.output.appendSlice(self.allocator, "; Bootstrap Orion Compiler Output\n");
        try self.output.appendSlice(self.allocator, "target triple = \"x86_64-pc-linux-gnu\"\n\n");

        // Declare syscall exit
        try self.output.appendSlice(self.allocator, "declare void @llvm.trap() noreturn nounwind\n\n");

        // Generate each function
        for (ast.functions.items) |func| {
            try self.generateFunction(&func);
        }

        // Generate _start entry point that calls main and exits
        try self.generateStartFunction();

        return try self.allocator.dupe(u8, self.output.items);
    }

    fn generateStartFunction(self: *Codegen) !void {
        try self.output.writer(self.allocator).print(
            \\define void @_start() {{
            \\  %result = call i32 @main()
            \\  %result_i64 = sext i32 %result to i64
            \\  %rax = call i64 asm sideeffect "syscall", "={{rax}},{{rax}},{{rdi}}"(i64 60, i64 %result_i64)
            \\  call void @llvm.trap()
            \\  unreachable
            \\}}
            \\
        , .{});
    }

    fn generateFunction(self: *Codegen, func: *const parser.FunctionDecl) !void {
        // Clear variables from previous function
        self.variables.clearRetainingCapacity();

        // Function signature
        const return_type_str = self.llvmType(func.return_type);
        try self.output.writer(self.allocator).print("define {s} @{s}() {{\n", .{ return_type_str, func.name });
        try self.output.appendSlice(self.allocator, "entry:\n");

        // Generate body
        for (func.body.items) |stmt| {
            try self.generateStatement(&stmt, func.return_type);
        }

        try self.output.appendSlice(self.allocator, "}\n\n");
    }

    fn generateStatement(self: *Codegen, stmt: *const Stmt, return_type: Type) !void {
        switch (stmt.*) {
            .let_binding => |binding| {
                // Infer the type of the value
                const value_expr_type = self.inferExprType(binding.value);

                // If type annotation provided, use it; otherwise use inferred type
                const var_type = binding.type_annotation orelse value_expr_type;
                const llvm_type_str = self.llvmType(var_type);

                // Allocate stack slot for variable
                const var_name = try std.fmt.allocPrint(self.allocator, "%{s}", .{binding.name});
                try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{ var_name, llvm_type_str });

                // Generate value expression
                const value = try self.generateExpression(binding.value);
                defer self.allocator.free(value);

                // Store value in variable
                try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ llvm_type_str, value, var_name });

                // Remember variable for later references (reuse var_name, no duplication needed)
                try self.variables.put(binding.name, .{
                    .llvm_ptr = var_name,
                    .var_type = var_type,
                });
            },
            .return_stmt => |expr| {
                const value = try self.generateExpression(expr);
                defer self.allocator.free(value);
                const expr_type_str = self.llvmTypeForExpr(expr);
                const return_type_str = self.llvmType(return_type);

                // If expression is i1 but function returns i32, extend it
                if (std.mem.eql(u8, expr_type_str, "i1") and std.mem.eql(u8, return_type_str, "i32")) {
                    const extended = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                    defer self.allocator.free(extended);
                    self.next_temp += 1;
                    try self.output.writer(self.allocator).print("  {s} = zext i1 {s} to i32\n", .{ extended, value });
                    try self.output.writer(self.allocator).print("  ret i32 {s}\n", .{extended});
                } else {
                    try self.output.writer(self.allocator).print("  ret {s} {s}\n", .{ expr_type_str, value });
                }
            },
        }
    }

    fn generateExpression(self: *Codegen, expr: *const Expr) ![]const u8 {
        switch (expr.*) {
            .integer_literal => |value| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{value});
            },
            .variable => |name| {
                // Load variable from stack
                const var_info = self.variables.get(name).?;
                const temp_name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                self.next_temp += 1;

                const llvm_type_str = self.llvmType(var_info.var_type);
                try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ temp_name, llvm_type_str, var_info.llvm_ptr });

                return temp_name;
            },
            .binary_op => |binop| {
                const left_val = try self.generateExpression(binop.left);
                defer self.allocator.free(left_val);
                const right_val = try self.generateExpression(binop.right);
                defer self.allocator.free(right_val);

                const temp_name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                defer self.allocator.free(temp_name);
                self.next_temp += 1;

                // Logical operators need special handling: convert i32 to i1 first
                switch (binop.op) {
                    .logical_and, .logical_or => {
                        // Convert left to bool
                        const left_bool = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                        defer self.allocator.free(left_bool);
                        self.next_temp += 1;
                        try self.output.writer(self.allocator).print("  {s} = icmp ne i32 {s}, 0\n", .{ left_bool, left_val });

                        // Convert right to bool
                        const right_bool = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                        defer self.allocator.free(right_bool);
                        self.next_temp += 1;
                        try self.output.writer(self.allocator).print("  {s} = icmp ne i32 {s}, 0\n", .{ right_bool, right_val });

                        // Boolean operation (result is i1)
                        const bool_op = if (binop.op == .logical_and) "and" else "or";
                        try self.output.writer(self.allocator).print("  {s} = {s} i1 {s}, {s}\n", .{ temp_name, bool_op, left_bool, right_bool });

                        return try self.allocator.dupe(u8, temp_name);
                    },
                    else => {
                        const op_str = self.llvmBinaryOp(binop.op);
                        try self.output.writer(self.allocator).print("  {s} = {s} i32 {s}, {s}\n", .{ temp_name, op_str, left_val, right_val });
                        return try self.allocator.dupe(u8, temp_name);
                    },
                }
            },
            .unary_op => |unop| {
                const operand_val = try self.generateExpression(unop.operand);
                defer self.allocator.free(operand_val);

                const temp_name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
                defer self.allocator.free(temp_name);
                self.next_temp += 1;

                const op_str = self.llvmUnaryOp(unop.op);
                try self.output.writer(self.allocator).print("  {s} = {s}, {s}\n", .{ temp_name, op_str, operand_val });

                return try self.allocator.dupe(u8, temp_name);
            },
        }
    }

    fn llvmType(self: *Codegen, typ: Type) []const u8 {
        _ = self;
        switch (typ) {
            .bool => return "i1",
            .i32 => return "i32",
        }
    }

    fn inferExprType(self: *Codegen, expr: *const Expr) Type {
        switch (expr.*) {
            .integer_literal => return .i32,
            .variable => |name| {
                const var_info = self.variables.get(name).?;
                return var_info.var_type;
            },
            .binary_op => |binop| {
                return if (binop.op.returns_bool()) .bool else .i32;
            },
            .unary_op => |unop| {
                return if (unop.op.returns_bool()) .bool else .i32;
            },
        }
    }

    fn llvmTypeForExpr(self: *Codegen, expr: *const Expr) []const u8 {
        const expr_type = self.inferExprType(expr);
        return self.llvmType(expr_type);
    }

    fn llvmUnaryOp(self: *Codegen, op: parser.UnaryOp) []const u8 {
        _ = self;
        return switch (op) {
            .negate => "sub i32 0",
            .logical_not => "icmp eq i32 0",
            .bitwise_not => "xor i32 -1",
        };
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
            .bitwise_and => "and",
            .bitwise_or => "or",
            .bitwise_xor => "xor",
            .shift_left => "shl",
            .shift_right => "ashr",
            // logical_and and logical_or are handled specially in generateExpression
            .logical_and, .logical_or => unreachable,
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

test "codegen: unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_]struct { src: []const u8, expected: []const u8 }{
        .{ .src = "fn f() -> I32 { return -5 }", .expected = "sub i32 0, 5" },
        .{ .src = "fn f() -> I32 { return !5 }", .expected = "zext i1" },
        .{ .src = "fn f() -> I32 { return ~5 }", .expected = "xor i32 -1, 5" },
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

test "codegen: logical operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const cases = [_]struct { src: []const u8, expected_conversions: []const u8, expected_op: []const u8 }{
        .{ .src = "fn f() -> I32 { return 5 && 3 }", .expected_conversions = "icmp ne i32", .expected_op = "and i1" },
        .{ .src = "fn f() -> I32 { return 5 || 3 }", .expected_conversions = "icmp ne i32", .expected_op = "or i1" },
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

        // Verify i32 to i1 conversion happens
        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_conversions) != null);
        // Verify boolean operation
        try testing.expect(std.mem.indexOf(u8, llvm_ir, case.expected_op) != null);
        // Verify extension back to i32 for return
        try testing.expect(std.mem.indexOf(u8, llvm_ir, "zext i1") != null);
    }
}

