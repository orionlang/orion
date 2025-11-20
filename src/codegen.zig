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

const FunctionInfo = struct {
    return_type: Type,
};

pub const Codegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    next_temp: usize,
    variables: std.StringHashMap(VarInfo),
    functions: std.StringHashMap(FunctionInfo),
    current_block: ?[]const u8,

    pub fn init(allocator: std.mem.Allocator) Codegen {
        return .{
            .allocator = allocator,
            .output = .empty,
            .next_temp = 0,
            .variables = std.StringHashMap(VarInfo).init(allocator),
            .functions = std.StringHashMap(FunctionInfo).init(allocator),
            .current_block = null,
        };
    }

    pub fn deinit(self: *Codegen) void {
        // Free all allocated variable names
        var iter = self.variables.valueIterator();
        while (iter.next()) |var_info| {
            self.allocator.free(var_info.llvm_ptr);
        }

        if (self.current_block) |block| {
            self.allocator.free(block);
        }

        self.output.deinit(self.allocator);
        self.variables.deinit();
        self.functions.deinit();
    }

    fn allocTempName(self: *Codegen) ![]const u8 {
        const temp_name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
        self.next_temp += 1;
        return temp_name;
    }

    fn setCurrentBlock(self: *Codegen, label: []const u8) !void {
        if (self.current_block) |old_block| self.allocator.free(old_block);
        self.current_block = try self.allocator.dupe(u8, label);
    }

    fn convertConditionToBool(self: *Codegen, condition_val: []const u8, condition_type: Type) ![]const u8 {
        if (condition_type == .bool) {
            return try self.allocator.dupe(u8, condition_val);
        }
        const temp = try self.allocTempName();
        const cond_type_str = self.llvmType(condition_type);
        try self.output.writer(self.allocator).print("  {s} = icmp ne {s} {s}, 0\n", .{ temp, cond_type_str, condition_val });
        return temp;
    }

    pub fn generate(self: *Codegen, ast: *const AST) ![]const u8 {
        // First pass: collect function signatures
        for (ast.functions.items) |func| {
            try self.functions.put(func.name, .{
                .return_type = func.return_type,
            });
        }

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
        // Clear variables from previous function and free allocated names
        var iter = self.variables.valueIterator();
        while (iter.next()) |var_info| {
            self.allocator.free(var_info.llvm_ptr);
        }
        self.variables.clearRetainingCapacity();

        // Function signature with parameters
        const return_type_str = self.llvmType(func.return_type);
        try self.output.writer(self.allocator).print("define {s} @{s}(", .{ return_type_str, func.name });

        // Generate parameter list
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.output.appendSlice(self.allocator, ", ");
            const param_type_str = self.llvmType(param.param_type);
            try self.output.writer(self.allocator).print("{s} %{s}", .{ param_type_str, param.name });
        }

        try self.output.appendSlice(self.allocator, ") {\nentry:\n");

        // Allocate and store parameters
        for (func.params) |param| {
            const param_type_str = self.llvmType(param.param_type);
            const var_name = try std.fmt.allocPrint(self.allocator, "%{s}.addr", .{param.name});

            try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{ var_name, param_type_str });
            try self.output.writer(self.allocator).print("  store {s} %{s}, ptr {s}\n", .{ param_type_str, param.name, var_name });

            try self.variables.put(param.name, .{
                .llvm_ptr = var_name,
                .var_type = param.param_type,
            });
        }

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

                // Convert value type if necessary
                const final_value = try self.convertIfNeeded(value, value_expr_type, var_type);
                defer self.allocator.free(final_value);

                // Store value in variable
                try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ llvm_type_str, final_value, var_name });

                // Remember variable for later references (reuse var_name, no duplication needed)
                try self.variables.put(binding.name, .{
                    .llvm_ptr = var_name,
                    .var_type = var_type,
                });
            },
            .return_stmt => |expr| {
                const value = try self.generateExpression(expr);
                defer self.allocator.free(value);
                const expr_type = self.inferExprType(expr);
                const return_type_str = self.llvmType(return_type);

                // Convert type if needed
                const converted = try self.convertIfNeeded(value, expr_type, return_type);
                defer self.allocator.free(converted);
                try self.output.writer(self.allocator).print("  ret {s} {s}\n", .{ return_type_str, converted });
            },
        }
    }

    fn convertIfNeeded(self: *Codegen, value: []const u8, from_type: Type, to_type: Type) ![]const u8 {
        if (from_type == to_type) {
            return try self.allocator.dupe(u8, value);
        }
        return try self.convertType(value, from_type, to_type);
    }

    fn convertType(self: *Codegen, value: []const u8, from_type: Type, to_type: Type) ![]const u8 {
        const from_llvm = self.llvmType(from_type);
        const to_llvm = self.llvmType(to_type);

        // Bool uses zero extension (unsigned)
        if (from_type == .bool) {
            const temp = try self.allocTempName();
            try self.output.writer(self.allocator).print("  {s} = zext i1 {s} to {s}\n", .{ temp, value, to_llvm });
            return temp;
        }

        const from_bits = from_type.bitWidth();
        const to_bits = to_type.bitWidth();

        if (from_bits == to_bits) {
            return try self.allocator.dupe(u8, value);
        }

        const temp = try self.allocTempName();
        if (from_bits < to_bits) {
            const inst: []const u8 = if (from_type.isSigned()) "sext" else "zext";
            try self.output.writer(self.allocator).print("  {s} = {s} {s} {s} to {s}\n", .{ temp, inst, from_llvm, value, to_llvm });
        } else {
            try self.output.writer(self.allocator).print("  {s} = trunc {s} {s} to {s}\n", .{ temp, from_llvm, value, to_llvm });
        }

        return temp;
    }

    fn generateExpression(self: *Codegen, expr: *const Expr) ![]const u8 {
        switch (expr.*) {
            .integer_literal => |value| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{value});
            },
            .bool_literal => |value| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{if (value) @as(i32, 1) else @as(i32, 0)});
            },
            .variable => |name| {
                // Load variable from stack
                const var_info = self.variables.get(name).?;
                const temp_name = try self.allocTempName();

                const llvm_type_str = self.llvmType(var_info.var_type);
                try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ temp_name, llvm_type_str, var_info.llvm_ptr });

                return temp_name;
            },
            .binary_op => |binop| {
                const left_val = try self.generateExpression(binop.left);
                defer self.allocator.free(left_val);
                const right_val = try self.generateExpression(binop.right);
                defer self.allocator.free(right_val);

                const temp_name = try self.allocTempName();
                defer self.allocator.free(temp_name);

                // Logical operators need special handling: convert i32 to i1 first
                switch (binop.op) {
                    .logical_and, .logical_or => {
                        return try self.generateLogicalBinOp(binop.op, binop.left, left_val, right_val, temp_name);
                    },
                    else => {
                        // Get type of left operand (both should be same type after type checking)
                        const operand_type = self.inferExprType(binop.left);
                        const op_str = self.llvmBinaryOp(binop.op, operand_type);
                        const type_str = self.llvmType(operand_type);
                        try self.output.writer(self.allocator).print("  {s} = {s} {s} {s}, {s}\n", .{ temp_name, op_str, type_str, left_val, right_val });
                        return try self.allocator.dupe(u8, temp_name);
                    },
                }
            },
            .unary_op => |unop| {
                const operand_val = try self.generateExpression(unop.operand);
                defer self.allocator.free(operand_val);

                const temp_name = try self.allocTempName();
                defer self.allocator.free(temp_name);

                const operand_type = self.inferExprType(unop.operand);
                const op_str = self.llvmUnaryOp(unop.op, operand_type);
                defer self.allocator.free(op_str);
                try self.output.writer(self.allocator).print("  {s} = {s}, {s}\n", .{ temp_name, op_str, operand_val });

                return try self.allocator.dupe(u8, temp_name);
            },
            .function_call => |call| {
                // Evaluate arguments
                var arg_values = try self.allocator.alloc([]const u8, call.args.len);
                defer {
                    for (arg_values) |val| {
                        self.allocator.free(val);
                    }
                    self.allocator.free(arg_values);
                }

                for (call.args, 0..) |arg, i| {
                    arg_values[i] = try self.generateExpression(arg);
                }

                // Get function return type from the function we're calling
                const return_type = self.inferExprType(expr);
                const return_type_str = self.llvmType(return_type);

                // Generate call instruction
                const temp_name = try self.allocTempName();
                defer self.allocator.free(temp_name);

                try self.output.writer(self.allocator).print("  {s} = call {s} @{s}(", .{
                    temp_name,
                    return_type_str,
                    call.name,
                });

                for (arg_values, 0..) |arg_val, i| {
                    if (i > 0) try self.output.appendSlice(self.allocator, ", ");
                    const arg_type = self.inferExprType(call.args[i]);
                    const arg_type_str = self.llvmType(arg_type);
                    try self.output.writer(self.allocator).print("{s} {s}", .{ arg_type_str, arg_val });
                }

                try self.output.appendSlice(self.allocator, ")\n");

                return try self.allocator.dupe(u8, temp_name);
            },
            .if_expr => |if_expr| {
                // Generate condition
                const condition_val = try self.generateExpression(if_expr.condition);
                defer self.allocator.free(condition_val);

                // Convert condition to i1 if needed
                const condition_type = self.inferExprType(if_expr.condition);
                const cond_bool = try self.convertConditionToBool(condition_val, condition_type);
                defer self.allocator.free(cond_bool);

                // Create labels for then, else, and merge blocks
                const then_label = try std.fmt.allocPrint(self.allocator, "then{d}", .{self.next_temp});
                const else_label = try std.fmt.allocPrint(self.allocator, "else{d}", .{self.next_temp});
                const merge_label = try std.fmt.allocPrint(self.allocator, "merge{d}", .{self.next_temp});
                self.next_temp += 1;

                // Branch on condition
                try self.output.writer(self.allocator).print("  br i1 {s}, label %{s}, label %{s}\n", .{ cond_bool, then_label, else_label });

                // Then block
                try self.output.writer(self.allocator).print("{s}:\n", .{then_label});
                try self.setCurrentBlock(then_label);

                const then_val = try self.generateExpression(if_expr.then_branch);

                // After generating then_branch, current_block tells us where we are
                const then_end_block = try self.allocator.dupe(u8, self.current_block.?);
                try self.output.writer(self.allocator).print("  br label %{s}\n", .{merge_label});

                // Else block
                try self.output.writer(self.allocator).print("{s}:\n", .{else_label});
                try self.setCurrentBlock(else_label);

                // Else branch is guaranteed to exist by typechecker
                const else_val = try self.generateExpression(if_expr.else_branch.?);

                // After generating else_branch, current_block tells us where we are
                const else_end_block = try self.allocator.dupe(u8, self.current_block.?);
                try self.output.writer(self.allocator).print("  br label %{s}\n", .{merge_label});

                // Merge block with phi node
                try self.output.writer(self.allocator).print("{s}:\n", .{merge_label});
                try self.setCurrentBlock(merge_label);

                const result_type = self.inferExprType(expr);
                const result_type_str = self.llvmType(result_type);
                const result_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = phi {s} [ {s}, %{s} ], [ {s}, %{s} ]\n", .{
                    result_temp,
                    result_type_str,
                    then_val,
                    then_end_block,
                    else_val,
                    else_end_block,
                });

                // Clean up labels and blocks
                self.allocator.free(then_label);
                self.allocator.free(else_label);
                self.allocator.free(merge_label);
                self.allocator.free(then_end_block);
                self.allocator.free(else_end_block);
                self.allocator.free(then_val);
                self.allocator.free(else_val);

                return result_temp;
            },
        }
    }

    fn generateLogicalBinOp(self: *Codegen, op: parser.BinaryOp, left_expr: *const parser.Expr, left_val: []const u8, right_val: []const u8, result_name: []const u8) ![]const u8 {
        // Logical operators can work on both bools and integers
        // Determine the type from the left operand
        const operand_type = self.inferExprType(left_expr);

        if (operand_type == .bool) {
            // Boolean operation on i1 values
            const bool_op = if (op == .logical_and) "and" else "or";
            try self.output.writer(self.allocator).print("  {s} = {s} i1 {s}, {s}\n", .{ result_name, bool_op, left_val, right_val });
        } else {
            // Integer operation - convert to bool first
            const left_bool = try self.allocTempName();
            defer self.allocator.free(left_bool);
            const type_str = self.llvmType(operand_type);
            try self.output.writer(self.allocator).print("  {s} = icmp ne {s} {s}, 0\n", .{ left_bool, type_str, left_val });

            const right_bool = try self.allocTempName();
            defer self.allocator.free(right_bool);
            try self.output.writer(self.allocator).print("  {s} = icmp ne {s} {s}, 0\n", .{ right_bool, type_str, right_val });

            const bool_op = if (op == .logical_and) "and" else "or";
            const temp_result = try self.allocTempName();
            defer self.allocator.free(temp_result);
            try self.output.writer(self.allocator).print("  {s} = {s} i1 {s}, {s}\n", .{ temp_result, bool_op, left_bool, right_bool });

            // Convert back to integer
            try self.output.writer(self.allocator).print("  {s} = zext i1 {s} to {s}\n", .{ result_name, temp_result, type_str });
        }

        return try self.allocator.dupe(u8, result_name);
    }

    fn llvmType(_: *Codegen, typ: Type) []const u8 {
        return typ.llvmTypeName();
    }

    fn inferExprType(self: *Codegen, expr: *const Expr) Type {
        switch (expr.*) {
            .integer_literal => return .i32,
            .bool_literal => return .bool,
            .variable => |name| {
                if (self.variables.get(name)) |var_info| {
                    return var_info.var_type;
                }
                return .i32;
            },
            .binary_op => |binop| {
                return if (binop.op.returns_bool()) .bool else self.inferExprType(binop.left);
            },
            .unary_op => |unop| {
                return if (unop.op.returns_bool()) .bool else self.inferExprType(unop.operand);
            },
            .function_call => |call| {
                if (self.functions.get(call.name)) |func_info| {
                    return func_info.return_type;
                }
                return .i32;
            },
            .if_expr => |if_expr| {
                return self.inferExprType(if_expr.then_branch);
            },
        }
    }

    fn llvmUnaryOp(self: *Codegen, op: parser.UnaryOp, operand_type: Type) []const u8 {
        const type_str = self.llvmType(operand_type);
        return switch (op) {
            .negate => std.fmt.allocPrint(self.allocator, "sub {s} 0", .{type_str}) catch unreachable,
            .logical_not => std.fmt.allocPrint(self.allocator, "icmp eq {s} 0", .{type_str}) catch unreachable,
            .bitwise_not => std.fmt.allocPrint(self.allocator, "xor {s} -1", .{type_str}) catch unreachable,
        };
    }

    fn llvmBinaryOp(self: *Codegen, op: parser.BinaryOp, operand_type: Type) []const u8 {
        _ = self;
        const is_signed = operand_type.isSigned();
        return switch (op) {
            .add => "add",
            .subtract => "sub",
            .multiply => "mul",
            .divide => if (is_signed) "sdiv" else "udiv",
            .modulo => if (is_signed) "srem" else "urem",
            .equal => "icmp eq",
            .not_equal => "icmp ne",
            .less_than => if (is_signed) "icmp slt" else "icmp ult",
            .greater_than => if (is_signed) "icmp sgt" else "icmp ugt",
            .less_equal => if (is_signed) "icmp sle" else "icmp ule",
            .greater_equal => if (is_signed) "icmp sge" else "icmp uge",
            .bitwise_and => "and",
            .bitwise_or => "or",
            .bitwise_xor => "xor",
            .shift_left => "shl",
            .shift_right => if (is_signed) "ashr" else "lshr",
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
        .{ .src = "fn f() -> I32 { return -5 }", .expected = "= sub i32 0, 5" },
        .{ .src = "fn f() -> I32 { return !5 }", .expected = "= icmp eq i32 0, 5" },
        .{ .src = "fn f() -> I32 { return ~5 }", .expected = "= xor i32 -1, 5" },
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

test "codegen: bool literals" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if true { 1 } else { 0 } }";
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

    // Bool literal should be used directly in br
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "br i1 1") != null);
}

test "codegen: simple if expression" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if true { 42 } else { 0 } }";
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

    // Check basic blocks created
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "else0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge0:") != null);

    // Check phi node
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "phi i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "[ 42,") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "[ 0,") != null);
}

test "codegen: if with integer condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if 5 { 1 } else { 0 } }";
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

    // Integer condition needs conversion to i1
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp ne i32 5, 0") != null);
}

test "codegen: if with comparison condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if 10 > 5 { 1 } else { 0 } }";
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

    // Comparison already returns i1, no conversion needed
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp sgt i32 10, 5") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "br i1 %") != null);
}

test "codegen: nested if expressions" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
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

    // Check multiple levels of basic blocks
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge1:") != null);

    // Check multiple phi nodes
    const phi_count = std.mem.count(u8, llvm_ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "codegen: elseif creates nested if" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
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

    // Elseif should create nested if structure
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "else0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "else1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge1:") != null);

    // Check phi nodes with correct predecessors
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "phi i32 [ 2, %then1 ], [ 3, %else1 ]") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "phi i32 [ 1, %then0 ]") != null);
}

test "codegen: basic block tracking in elseif" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() -> I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
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

    // The outer phi node should reference merge1 (not else0) as predecessor
    // because after evaluating the nested if in else0, we're in merge1
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%merge1 ]") != null);
}

