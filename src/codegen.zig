const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;
const Pattern = parser.Pattern;

pub const CodegenError = error{
    OutOfMemory,
    UndefinedVariable,
    TupleNotImplemented,
    TupleDestructuringNotImplemented,
};

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
    type_defs: std.StringHashMap(Type),
    current_block: ?[]const u8,
    inferred_types: std.ArrayList(Type),

    pub fn init(allocator: std.mem.Allocator) Codegen {
        return .{
            .allocator = allocator,
            .output = .empty,
            .next_temp = 0,
            .variables = std.StringHashMap(VarInfo).init(allocator),
            .functions = std.StringHashMap(FunctionInfo).init(allocator),
            .type_defs = std.StringHashMap(Type).init(allocator),
            .current_block = null,
            .inferred_types = .empty,
        };
    }

    pub fn deinit(self: *Codegen) void {
        // Free all allocated variable names
        var iter = self.variables.valueIterator();
        while (iter.next()) |var_info| {
            self.allocator.free(var_info.llvm_ptr);
        }

        // Free all inferred tuple types (shallow - nested tuples tracked separately)
        for (self.inferred_types.items) |*typ| {
            typ.deinitShallow(self.allocator);
        }
        self.inferred_types.deinit(self.allocator);

        if (self.current_block) |block| {
            self.allocator.free(block);
        }

        self.output.deinit(self.allocator);
        self.variables.deinit();
        self.functions.deinit();
        self.type_defs.deinit();
    }

    fn allocTempName(self: *Codegen) ![]const u8 {
        const temp_name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{self.next_temp});
        self.next_temp += 1;
        return temp_name;
    }

    fn allocLabel(self: *Codegen, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ prefix, self.next_temp });
        self.next_temp += 1;
        return label;
    }

    fn setCurrentBlock(self: *Codegen, label: []const u8) !void {
        if (self.current_block) |old_block| self.allocator.free(old_block);
        self.current_block = try self.allocator.dupe(u8, label);
    }

    fn convertConditionToBool(self: *Codegen, condition_val: []const u8, condition_type: Type) ![]const u8 {
        if (condition_type.kind == .primitive and condition_type.kind.primitive == .bool) {
            return try self.allocator.dupe(u8, condition_val);
        }
        const temp = try self.allocTempName();
        const cond_type_str = self.llvmType(condition_type);
        try self.output.writer(self.allocator).print("  {s} = icmp ne {s} {s}, 0\n", .{ temp, cond_type_str, condition_val });
        return temp;
    }

    fn unitValue(self: *Codegen) ![]const u8 {
        return try self.allocator.dupe(u8, "0");
    }

    fn storePayloadsSequentially(
        self: *Codegen,
        sum_type_str: []const u8,
        current_val: []const u8,
        args: []const *Expr,
    ) CodegenError![]const u8 {
        // Allocate stack space to build the sum type value
        const data_ptr_temp = try self.allocTempName();
        defer self.allocator.free(data_ptr_temp);
        try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{ data_ptr_temp, sum_type_str });
        try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ sum_type_str, current_val, data_ptr_temp });

        // Get pointer to data field
        const data_field_ptr = try self.allocTempName();
        defer self.allocator.free(data_field_ptr);
        try self.output.writer(self.allocator).print("  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 1\n", .{
            data_field_ptr,
            sum_type_str,
            data_ptr_temp,
        });

        // Store each argument sequentially in the data field
        var byte_offset: usize = 0;
        for (args) |arg| {
            const arg_val = try self.generateExpression(arg);
            defer self.allocator.free(arg_val);

            const arg_type = self.inferExprType(arg);
            const arg_type_str = try self.llvmTypeString(arg_type);
            defer self.allocator.free(arg_type_str);

            // Get pointer at byte offset
            const offset_ptr = try self.allocTempName();
            defer self.allocator.free(offset_ptr);
            try self.output.writer(self.allocator).print("  {s} = getelementptr inbounds i8, ptr {s}, i32 {d}\n", .{
                offset_ptr,
                data_field_ptr,
                byte_offset,
            });

            // Store the argument value at this offset
            try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{
                arg_type_str,
                arg_val,
                offset_ptr,
            });

            // Update byte offset for next argument
            byte_offset += arg_type.bitWidth() / 8;
        }

        // Load back the complete sum type value
        const final_temp = try self.allocTempName();
        try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{
            final_temp,
            sum_type_str,
            data_ptr_temp,
        });

        return final_temp;
    }

    fn extractPayloadsSequentially(
        self: *Codegen,
        scrutinee_type_str: []const u8,
        scrutinee_val: []const u8,
        bindings: []const []const u8,
        payload_types: []const *Type,
    ) CodegenError!void {
        // Allocate stack space to store the scrutinee
        const scrutinee_ptr = try self.allocTempName();
        defer self.allocator.free(scrutinee_ptr);
        try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{
            scrutinee_ptr,
            scrutinee_type_str,
        });
        try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{
            scrutinee_type_str,
            scrutinee_val,
            scrutinee_ptr,
        });

        // Get pointer to data field (index 1)
        const data_field_ptr = try self.allocTempName();
        defer self.allocator.free(data_field_ptr);
        try self.output.writer(self.allocator).print("  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 1\n", .{
            data_field_ptr,
            scrutinee_type_str,
            scrutinee_ptr,
        });

        // Extract each binding from the data field at its byte offset
        var byte_offset: usize = 0;
        for (bindings, payload_types) |binding, payload_type| {
            // Get pointer at byte offset
            const offset_ptr = try self.allocTempName();
            defer self.allocator.free(offset_ptr);
            try self.output.writer(self.allocator).print("  {s} = getelementptr inbounds i8, ptr {s}, i32 {d}\n", .{
                offset_ptr,
                data_field_ptr,
                byte_offset,
            });

            // Allocate stack space for this binding
            const binding_ptr = try self.allocTempName();
            // Note: binding_ptr is NOT freed here because it's stored in variables map
            // and will be freed when the variable is removed from scope
            const payload_type_str = try self.llvmTypeString(payload_type.*);
            defer self.allocator.free(payload_type_str);

            try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{
                binding_ptr,
                payload_type_str,
            });

            // Load the value from the data field
            const loaded_val = try self.allocTempName();
            defer self.allocator.free(loaded_val);
            try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{
                loaded_val,
                payload_type_str,
                offset_ptr,
            });

            // Store it in the binding's stack location
            try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{
                payload_type_str,
                loaded_val,
                binding_ptr,
            });

            // Add to variables map
            try self.variables.put(binding, .{
                .var_type = payload_type.*,
                .llvm_ptr = binding_ptr,
            });

            // Update byte offset for next payload
            byte_offset += payload_type.*.bitWidth() / 8;
        }
    }

    pub fn generate(self: *Codegen, ast: *const AST) ![]const u8 {
        // First pass: collect type definitions
        for (ast.type_defs.items) |typedef| {
            try self.type_defs.put(typedef.name, typedef.type_value);
        }

        // Second pass: collect function signatures
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
        const return_type_str = try self.llvmTypeString(func.return_type);
        defer self.allocator.free(return_type_str);
        try self.output.writer(self.allocator).print("define {s} @{s}(", .{ return_type_str, func.name });

        // Generate parameter list
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.output.appendSlice(self.allocator, ", ");
            const param_type_str = try self.llvmTypeString(param.param_type);
            defer self.allocator.free(param_type_str);
            try self.output.writer(self.allocator).print("{s} %{s}", .{ param_type_str, param.name });
        }

        try self.output.appendSlice(self.allocator, ") {\nentry:\n");

        // Allocate and store parameters
        for (func.params) |param| {
            const param_type_str = try self.llvmTypeString(param.param_type);
            defer self.allocator.free(param_type_str);
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

    fn generateStatement(self: *Codegen, stmt: *const Stmt, return_type: Type) CodegenError!void {
        switch (stmt.*) {
            .let_binding => |binding| {
                // Infer the type of the value
                const value_expr_type = self.inferExprType(binding.value);

                // If type annotation provided, use it; otherwise use inferred type
                const var_type = binding.type_annotation orelse value_expr_type;

                // Generate value expression
                const value = try self.generateExpression(binding.value);
                defer self.allocator.free(value);

                // Convert value type if necessary
                const final_value = try self.convertIfNeeded(value, value_expr_type, var_type);
                defer self.allocator.free(final_value);

                // Bind pattern to variables
                try self.bindPattern(binding.pattern, var_type, final_value, binding.mutable);
            },
            .assignment => |assign| {
                // Look up the variable
                const var_info = self.variables.get(assign.name) orelse {
                    return error.UndefinedVariable;
                };

                const var_type = var_info.var_type;
                const llvm_type_str = try self.llvmTypeString(var_type);
                defer self.allocator.free(llvm_type_str);

                // Generate value expression
                const value = try self.generateExpression(assign.value);
                defer self.allocator.free(value);

                // Get value type and convert if needed
                const value_type = self.inferExprType(assign.value);
                const final_value = try self.convertIfNeeded(value, value_type, var_type);
                defer self.allocator.free(final_value);

                // Store new value in variable
                try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ llvm_type_str, final_value, var_info.llvm_ptr });
            },
            .while_stmt => |while_stmt| {
                const header_label = try self.allocLabel("while_header");
                const body_label = try self.allocLabel("while_body");
                const exit_label = try self.allocLabel("while_exit");

                // Branch to header
                try self.output.writer(self.allocator).print("  br label %{s}\n", .{header_label});

                // Header block: evaluate condition
                try self.output.writer(self.allocator).print("{s}:\n", .{header_label});
                try self.setCurrentBlock(header_label);

                const cond_bool = try self.generateConditionAsBool(while_stmt.condition);
                defer self.allocator.free(cond_bool);

                // Branch on condition: true -> body, false -> exit
                try self.output.writer(self.allocator).print("  br i1 {s}, label %{s}, label %{s}\n", .{ cond_bool, body_label, exit_label });

                // Body block
                try self.output.writer(self.allocator).print("{s}:\n", .{body_label});
                try self.setCurrentBlock(body_label);

                const body_val = try self.generateExpression(while_stmt.body);
                defer self.allocator.free(body_val);

                // Branch back to header
                try self.output.writer(self.allocator).print("  br label %{s}\n", .{header_label});

                // Exit block
                try self.output.writer(self.allocator).print("{s}:\n", .{exit_label});
                try self.setCurrentBlock(exit_label);

                // Clean up labels
                self.allocator.free(header_label);
                self.allocator.free(body_label);
                self.allocator.free(exit_label);
            },
            .return_stmt => |expr| {
                const value = try self.generateExpression(expr);
                defer self.allocator.free(value);
                const expr_type = self.inferExprType(expr);
                const return_type_str = try self.llvmTypeString(return_type);
                defer self.allocator.free(return_type_str);

                // Convert type if needed
                const converted = try self.convertIfNeeded(value, expr_type, return_type);
                defer self.allocator.free(converted);
                try self.output.writer(self.allocator).print("  ret {s} {s}\n", .{ return_type_str, converted });
            },
        }
    }

    fn bindPattern(self: *Codegen, pattern: Pattern, value_type: Type, value_llvm: []const u8, mutable: bool) !void {
        switch (pattern) {
            .identifier => |name| {
                const llvm_type_str = try self.llvmTypeString(value_type);
                defer self.allocator.free(llvm_type_str);

                // Allocate stack slot for variable
                const var_name = try std.fmt.allocPrint(self.allocator, "%{s}", .{name});
                try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{ var_name, llvm_type_str });

                // Store value in variable
                try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ llvm_type_str, value_llvm, var_name });

                // Remember variable for later references
                try self.variables.put(name, .{
                    .llvm_ptr = var_name,
                    .var_type = value_type,
                });
            },
            .tuple => |sub_patterns| {
                // Extract each element from the tuple and bind recursively
                const tuple_type_str = try self.llvmTypeString(value_type);
                defer self.allocator.free(tuple_type_str);

                for (sub_patterns, 0..) |sub_pattern, i| {
                    const elem_type = value_type.kind.tuple[i].*;
                    const elem_temp = try self.allocTempName();

                    // Extract element from tuple
                    try self.output.writer(self.allocator).print("  {s} = extractvalue {s} {s}, {d}\n", .{
                        elem_temp,
                        tuple_type_str,
                        value_llvm,
                        i,
                    });

                    // Recursively bind the pattern
                    try self.bindPattern(sub_pattern.*, elem_type, elem_temp, mutable);
                    self.allocator.free(elem_temp);
                }
            },
        }
    }

    fn convertIfNeeded(self: *Codegen, value: []const u8, from_type: Type, to_type: Type) ![]const u8 {
        if (from_type.eql(to_type)) {
            return try self.allocator.dupe(u8, value);
        }
        return try self.convertType(value, from_type, to_type);
    }

    fn convertType(self: *Codegen, value: []const u8, from_type: Type, to_type: Type) ![]const u8 {
        const from_llvm = self.llvmType(from_type);
        const to_llvm = self.llvmType(to_type);

        // Bool uses zero extension (unsigned)
        if (from_type.kind == .primitive and from_type.kind.primitive == .bool) {
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
            .integer_literal => |lit| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{lit.value});
            },
            .bool_literal => |value| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{if (value) @as(i32, 1) else @as(i32, 0)});
            },
            .variable => |name| {
                // Load variable from stack
                const var_info = self.variables.get(name).?;
                const temp_name = try self.allocTempName();

                const llvm_type_str = try self.llvmTypeString(var_info.var_type);
                defer self.allocator.free(llvm_type_str);
                try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ temp_name, llvm_type_str, var_info.llvm_ptr });

                return temp_name;
            },
            .tuple_literal => |elements| {
                // Generate tuple value using insertvalue instructions
                const tuple_type = self.inferExprType(expr);
                const tuple_type_str = try self.llvmTypeString(tuple_type);
                defer self.allocator.free(tuple_type_str);

                // Empty tuple case
                if (elements.len == 0) {
                    return try std.fmt.allocPrint(self.allocator, "undef", .{});
                }

                // Start with undef
                var current_val: []const u8 = try std.fmt.allocPrint(self.allocator, "undef", .{});

                for (elements, 0..) |elem, i| {
                    const elem_val = try self.generateExpression(elem);
                    defer self.allocator.free(elem_val);

                    const elem_type = self.inferExprType(elem);
                    const elem_type_str = try self.llvmTypeString(elem_type);
                    defer self.allocator.free(elem_type_str);

                    const temp = try self.allocTempName();
                    try self.output.writer(self.allocator).print("  {s} = insertvalue {s} {s}, {s} {s}, {d}\n", .{
                        temp,
                        tuple_type_str,
                        current_val,
                        elem_type_str,
                        elem_val,
                        i,
                    });

                    if (i < elements.len - 1) {
                        self.allocator.free(current_val);
                        current_val = try self.allocator.dupe(u8, temp);
                        self.allocator.free(temp);
                    } else {
                        self.allocator.free(current_val);
                        return temp;
                    }
                }

                unreachable;
            },
            .tuple_index => |index| {
                const tuple_val = try self.generateExpression(index.tuple);
                defer self.allocator.free(tuple_val);

                const tuple_type = self.inferExprType(index.tuple);
                const tuple_type_str = try self.llvmTypeString(tuple_type);
                defer self.allocator.free(tuple_type_str);

                const temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = extractvalue {s} {s}, {d}\n", .{
                    temp,
                    tuple_type_str,
                    tuple_val,
                    index.index,
                });

                return temp;
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
                const return_type_str = try self.llvmTypeString(return_type);
                defer self.allocator.free(return_type_str);

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
                    const arg_type_str = try self.llvmTypeString(arg_type);
                    defer self.allocator.free(arg_type_str);
                    try self.output.writer(self.allocator).print("{s} {s}", .{ arg_type_str, arg_val });
                }

                try self.output.appendSlice(self.allocator, ")\n");

                return try self.allocator.dupe(u8, temp_name);
            },
            .if_expr => |if_expr| {
                // Generate condition as bool
                const cond_bool = try self.generateConditionAsBool(if_expr.condition);
                defer self.allocator.free(cond_bool);

                const then_label = try self.allocLabel("then");
                const else_label = try self.allocLabel("else");
                const merge_label = try self.allocLabel("merge");

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
                const result_type_str = try self.llvmTypeString(result_type);
                defer self.allocator.free(result_type_str);
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
            .block_expr => |block| {
                // Generate all statements
                for (block.statements) |*stmt| {
                    try self.generateStatement(stmt, .{ .kind = .{ .primitive = .i32  }, .usage = .once }); // Dummy return type for blocks
                }

                // Generate result expression or unit value
                if (block.result) |result| {
                    return try self.generateExpression(result);
                } else {
                    return try self.unitValue();
                }
            },
            .struct_literal => |lit| {
                const struct_type = self.inferExprType(expr);
                const struct_type_str = try self.llvmTypeString(struct_type);
                defer self.allocator.free(struct_type_str);

                const typedef = self.type_defs.get(lit.type_name) orelse unreachable;

                var current_val: []const u8 = try std.fmt.allocPrint(self.allocator, "undef", .{});

                for (typedef.kind.struct_type, 0..) |type_field, field_idx| {
                    var field_value: ?[]const u8 = null;
                    for (lit.fields) |lit_field| {
                        if (std.mem.eql(u8, lit_field.name, type_field.name)) {
                            field_value = try self.generateExpression(lit_field.value);
                            break;
                        }
                    }
                    const field_val = field_value orelse unreachable;
                    defer self.allocator.free(field_val);

                    const field_type = type_field.field_type.*;
                    const field_type_str = try self.llvmTypeString(field_type);
                    defer self.allocator.free(field_type_str);

                    const temp = try self.allocTempName();
                    try self.output.writer(self.allocator).print("  {s} = insertvalue {s} {s}, {s} {s}, {d}\n", .{
                        temp,
                        struct_type_str,
                        current_val,
                        field_type_str,
                        field_val,
                        field_idx,
                    });

                    if (field_idx < typedef.kind.struct_type.len - 1) {
                        self.allocator.free(current_val);
                        current_val = try self.allocator.dupe(u8, temp);
                        self.allocator.free(temp);
                    } else {
                        self.allocator.free(current_val);
                        return temp;
                    }
                }

                self.allocator.free(current_val);
                return try std.fmt.allocPrint(self.allocator, "undef", .{});
            },
            .field_access => |access| {
                const object_val = try self.generateExpression(access.object);
                defer self.allocator.free(object_val);

                const object_type = self.inferExprType(access.object);
                const object_type_str = try self.llvmTypeString(object_type);
                defer self.allocator.free(object_type_str);

                const typedef = self.type_defs.get(object_type.kind.named) orelse unreachable;

                var field_idx: usize = 0;
                for (typedef.kind.struct_type, 0..) |field, idx| {
                    if (std.mem.eql(u8, field.name, access.field_name)) {
                        field_idx = idx;
                        break;
                    }
                }

                const temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = extractvalue {s} {s}, {d}\n", .{
                    temp,
                    object_type_str,
                    object_val,
                    field_idx,
                });

                return temp;
            },
            .constructor_call => |call| {
                // Find which sum type and variant this constructor belongs to
                var found_type_name: ?[]const u8 = null;
                var found_typedef: ?Type = null;
                var variant_index: usize = 0;

                var iter = self.type_defs.iterator();
                while (iter.next()) |entry| {
                    if (entry.value_ptr.*.kind == .sum_type) {
                        for (entry.value_ptr.*.kind.sum_type, 0..) |variant, idx| {
                            if (std.mem.eql(u8, variant.name, call.name)) {
                                found_type_name = entry.key_ptr.*;
                                found_typedef = entry.value_ptr.*;
                                variant_index = idx;
                                break;
                            }
                        }
                        if (found_type_name != null) break;
                    }
                }

                const sum_type = found_typedef.?;
                const sum_type_str = try self.llvmTypeString(sum_type);
                defer self.allocator.free(sum_type_str);

                // Start with undef
                var current_val: []const u8 = try std.fmt.allocPrint(self.allocator, "undef", .{});

                // Insert tag (i64)
                const temp1 = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = insertvalue {s} {s}, i64 {d}, 0\n", .{
                    temp1,
                    sum_type_str,
                    current_val,
                    variant_index,
                });
                self.allocator.free(current_val);
                current_val = temp1;

                // Insert payload if constructor has arguments
                // We store payloads sequentially in the data array
                // Since we use [N x i8] representation, we need to bitcast

                if (call.args.len > 0) {
                    const final_temp = try self.storePayloadsSequentially(sum_type_str, current_val, call.args);
                    self.allocator.free(current_val);
                    return final_temp;
                }

                return current_val;
            },
            .match_expr => |match_expr| {
                const scrutinee_val = try self.generateExpression(match_expr.scrutinee);
                defer self.allocator.free(scrutinee_val);

                const scrutinee_type = self.inferExprType(match_expr.scrutinee);
                const scrutinee_type_str = try self.llvmTypeString(scrutinee_type);
                defer self.allocator.free(scrutinee_type_str);

                const typedef = self.type_defs.get(scrutinee_type.kind.named) orelse unreachable;

                // Extract tag
                const tag_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = extractvalue {s} {s}, 0\n", .{
                    tag_temp,
                    scrutinee_type_str,
                    scrutinee_val,
                });
                defer self.allocator.free(tag_temp);

                // Create switch on tag
                const merge_label = try self.allocLabel("match_merge");
                defer self.allocator.free(merge_label);

                // Create arm labels
                var arm_labels = try self.allocator.alloc([]const u8, match_expr.arms.len);
                defer {
                    for (arm_labels) |label| {
                        self.allocator.free(label);
                    }
                    self.allocator.free(arm_labels);
                }

                for (0..match_expr.arms.len) |i| {
                    arm_labels[i] = try self.allocLabel("match_arm");
                }

                // Generate switch instruction
                try self.output.writer(self.allocator).print("  switch i64 {s}, label %{s} [", .{
                    tag_temp,
                    arm_labels[0],
                });

                // Add cases for each constructor pattern
                for (match_expr.arms, 0..) |arm, i| {
                    switch (arm.pattern) {
                        .identifier => {},
                        .constructor => |constructor| {
                            // Find the variant index
                            for (typedef.kind.sum_type, 0..) |variant, variant_idx| {
                                if (std.mem.eql(u8, variant.name, constructor.name)) {
                                    if (i > 0) try self.output.appendSlice(self.allocator, ", ");
                                    try self.output.writer(self.allocator).print("\n    i64 {d}, label %{s}", .{
                                        variant_idx,
                                        arm_labels[i],
                                    });
                                    break;
                                }
                            }
                        },
                    }
                }
                try self.output.appendSlice(self.allocator, "\n  ]\n");

                // Generate code for each arm
                var arm_vals = try self.allocator.alloc([]const u8, match_expr.arms.len);
                defer {
                    for (arm_vals) |val| {
                        self.allocator.free(val);
                    }
                    self.allocator.free(arm_vals);
                }

                var arm_end_blocks = try self.allocator.alloc([]const u8, match_expr.arms.len);
                defer {
                    for (arm_end_blocks) |block| {
                        self.allocator.free(block);
                    }
                    self.allocator.free(arm_end_blocks);
                }

                for (match_expr.arms, 0..) |arm, i| {
                    try self.output.writer(self.allocator).print("{s}:\n", .{arm_labels[i]});
                    try self.setCurrentBlock(arm_labels[i]);

                    // Extract payload and add pattern bindings to variable scope
                    switch (arm.pattern) {
                        .identifier => {},
                        .constructor => |constructor| {
                            // Find the variant to get payload types
                            var found_variant: ?parser.SumTypeVariant = null;
                            for (typedef.kind.sum_type) |variant| {
                                if (std.mem.eql(u8, variant.name, constructor.name)) {
                                    found_variant = variant;
                                    break;
                                }
                            }

                            if (found_variant) |variant| {
                                // Extract payload from sum type data field
                                // Sum type is { i64 tag, [N x i8] data }
                                // We need to extract the data field and access payloads at byte offsets

                                if (constructor.bindings.len > 0) {
                                    try self.extractPayloadsSequentially(
                                        scrutinee_type_str,
                                        scrutinee_val,
                                        constructor.bindings,
                                        variant.payload_types,
                                    );
                                }
                            }
                        },
                    }

                    arm_vals[i] = try self.generateExpression(arm.body);
                    arm_end_blocks[i] = try self.allocator.dupe(u8, self.current_block.?);
                    try self.output.writer(self.allocator).print("  br label %{s}\n", .{merge_label});

                    // Remove pattern bindings from variable scope
                    switch (arm.pattern) {
                        .identifier => {},
                        .constructor => |constructor| {
                            for (constructor.bindings) |binding| {
                                if (self.variables.get(binding)) |var_info| {
                                    self.allocator.free(var_info.llvm_ptr);
                                }
                                _ = self.variables.remove(binding);
                            }
                        },
                    }
                }

                // Merge block with phi
                try self.output.writer(self.allocator).print("{s}:\n", .{merge_label});
                try self.setCurrentBlock(merge_label);

                const result_type = self.inferExprType(expr);
                const result_type_str = try self.llvmTypeString(result_type);
                defer self.allocator.free(result_type_str);

                const result_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = phi {s}", .{ result_temp, result_type_str });

                for (arm_vals, arm_end_blocks, 0..) |val, block, i| {
                    if (i > 0) try self.output.appendSlice(self.allocator, ",");
                    try self.output.writer(self.allocator).print(" [ {s}, %{s} ]", .{ val, block });
                }
                try self.output.appendSlice(self.allocator, "\n");

                return result_temp;
            },
        }
    }

    fn generateConditionAsBool(self: *Codegen, condition: *const Expr) CodegenError![]const u8 {
        const condition_val = try self.generateExpression(condition);
        defer self.allocator.free(condition_val);
        const condition_type = self.inferExprType(condition);
        return try self.convertConditionToBool(condition_val, condition_type);
    }

    fn generateLogicalBinOp(self: *Codegen, op: parser.BinaryOp, left_expr: *const parser.Expr, left_val: []const u8, right_val: []const u8, result_name: []const u8) ![]const u8 {
        // Logical operators can work on both bools and integers
        // Determine the type from the left operand
        const operand_type = self.inferExprType(left_expr);

        if (operand_type.kind == .primitive and operand_type.kind.primitive == .bool) {
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

    fn llvmTypeString(self: *Codegen, typ: Type) ![]const u8 {
        switch (typ.kind) {
            .primitive => return try self.allocator.dupe(u8, typ.llvmTypeName()),
            .tuple => |elements| {
                var type_parts: std.ArrayList(u8) = .empty;
                try type_parts.ensureTotalCapacity(self.allocator, 256);
                defer type_parts.deinit(self.allocator);
                try type_parts.appendSlice(self.allocator, "{ ");

                for (elements, 0..) |elem, i| {
                    if (i > 0) try type_parts.appendSlice(self.allocator, ", ");
                    const elem_str = try self.llvmTypeString(elem.*);
                    defer self.allocator.free(elem_str);
                    try type_parts.appendSlice(self.allocator, elem_str);
                }
                try type_parts.appendSlice(self.allocator, " }");
                return try type_parts.toOwnedSlice(self.allocator);
            },
            .struct_type => |fields| {
                var type_parts: std.ArrayList(u8) = .empty;
                try type_parts.ensureTotalCapacity(self.allocator, 256);
                defer type_parts.deinit(self.allocator);
                try type_parts.appendSlice(self.allocator, "{ ");

                for (fields, 0..) |field, i| {
                    if (i > 0) try type_parts.appendSlice(self.allocator, ", ");
                    const field_str = try self.llvmTypeString(field.field_type.*);
                    defer self.allocator.free(field_str);
                    try type_parts.appendSlice(self.allocator, field_str);
                }
                try type_parts.appendSlice(self.allocator, " }");
                return try type_parts.toOwnedSlice(self.allocator);
            },
            .sum_type => |variants| {
                // Sum type is represented as: { i64 tag, [N x i8] data }
                // where N is max size of all variants
                var max_size: usize = 0;
                for (variants) |variant| {
                    var variant_size: usize = 0;
                    for (variant.payload_types) |payload_type| {
                        // Simplified size calculation (assumes all i32/i64)
                        if (payload_type.*.kind == .primitive) {
                            variant_size += payload_type.*.bitWidth() / 8;
                        } else {
                            variant_size += 8; // Pointer or struct size estimate
                        }
                    }
                    if (variant_size > max_size) {
                        max_size = variant_size;
                    }
                }

                // Allocate a string for "{ i64, [N x i8] }"
                return try std.fmt.allocPrint(self.allocator, "{{ i64, [{d} x i8] }}", .{max_size});
            },
            .named => |name| {
                const typedef = self.type_defs.get(name) orelse unreachable;
                return try self.llvmTypeString(typedef);
            },
        }
    }

    fn inferExprType(self: *Codegen, expr: *const Expr) Type {
        switch (expr.*) {
            .integer_literal => |lit| return lit.inferred_type,
            .bool_literal => return .{ .kind = .{ .primitive = .bool  }, .usage = .once },
            .tuple_literal => |elements| {
                const elem_types = self.allocator.alloc(*Type, elements.len) catch unreachable;
                for (elements, 0..) |elem, i| {
                    const elem_type_ptr = self.allocator.create(Type) catch unreachable;
                    elem_type_ptr.* = self.inferExprType(elem);
                    elem_types[i] = elem_type_ptr;
                }
                const tuple_type = Type{ .kind = .{ .tuple = elem_types }, .usage = .once };
                // Track ALL tuple types (including nested) for cleanup
                self.inferred_types.append(self.allocator, tuple_type) catch unreachable;
                return tuple_type;
            },
            .tuple_index => |index| {
                const tuple_type = self.inferExprType(index.tuple);
                return tuple_type.kind.tuple[index.index].*;
            },
            .variable => |name| {
                if (self.variables.get(name)) |var_info| {
                    return var_info.var_type;
                }
                return .{ .kind = .{ .primitive = .i32  }, .usage = .once };
            },
            .binary_op => |binop| {
                return if (binop.op.returns_bool()) .{ .kind = .{ .primitive = .bool  }, .usage = .once } else self.inferExprType(binop.left);
            },
            .unary_op => |unop| {
                return if (unop.op.returns_bool()) .{ .kind = .{ .primitive = .bool  }, .usage = .once } else self.inferExprType(unop.operand);
            },
            .function_call => |call| {
                if (self.functions.get(call.name)) |func_info| {
                    return func_info.return_type;
                }
                return .{ .kind = .{ .primitive = .i32  }, .usage = .once };
            },
            .if_expr => |if_expr| {
                return self.inferExprType(if_expr.then_branch);
            },
            .block_expr => |block| {
                if (block.result) |result| {
                    return self.inferExprType(result);
                } else {
                    return .{ .kind = .{ .primitive = .i32  }, .usage = .once }; // Unit value
                }
            },
            .struct_literal => |lit| {
                return .{ .kind = .{ .named = lit.type_name  }, .usage = .once };
            },
            .field_access => |access| {
                const object_type = self.inferExprType(access.object);
                const typedef = self.type_defs.get(object_type.kind.named) orelse unreachable;
                for (typedef.kind.struct_type) |field| {
                    if (std.mem.eql(u8, field.name, access.field_name)) {
                        return field.field_type.*;
                    }
                }
                unreachable;
            },
            .constructor_call => |call| {
                // Find which sum type this constructor belongs to
                var iter = self.type_defs.iterator();
                while (iter.next()) |entry| {
                    if (entry.value_ptr.*.kind == .sum_type) {
                        for (entry.value_ptr.*.kind.sum_type) |variant| {
                            if (std.mem.eql(u8, variant.name, call.name)) {
                                return .{ .kind = .{ .named = entry.key_ptr.*  }, .usage = .once };
                            }
                        }
                    }
                }
                unreachable;
            },
            .match_expr => |match_expr| {
                // Match result type is the type of the first arm
                return self.inferExprType(match_expr.arms[0].body);
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

    const source = "fn main() I32 { return 42 }";
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

    const source = "fn main() I32 { return 1 + 2 }";
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
        .{ .src = "fn f() I32 { return 10 - 5 }", .expected = "sub i32 10, 5" },
        .{ .src = "fn f() I32 { return 10 * 5 }", .expected = "mul i32 10, 5" },
        .{ .src = "fn f() I32 { return 10 / 5 }", .expected = "sdiv i32 10, 5" },
        .{ .src = "fn f() I32 { return 10 % 5 }", .expected = "srem i32 10, 5" },
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
        .{ .src = "fn f() I32 { return 5 == 3 }", .expected = "icmp eq i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 != 3 }", .expected = "icmp ne i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 < 3 }", .expected = "icmp slt i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 > 3 }", .expected = "icmp sgt i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 <= 3 }", .expected = "icmp sle i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 >= 3 }", .expected = "icmp sge i32 5, 3" },
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
        .{ .src = "fn f() I32 { return 5 & 3 }", .expected = "and i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 | 3 }", .expected = "or i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 ^ 3 }", .expected = "xor i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 << 3 }", .expected = "shl i32 5, 3" },
        .{ .src = "fn f() I32 { return 5 >> 3 }", .expected = "ashr i32 5, 3" },
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
        .{ .src = "fn f() I32 { return -5 }", .expected = "= sub i32 0, 5" },
        .{ .src = "fn f() I32 { return !5 }", .expected = "= icmp eq i32 0, 5" },
        .{ .src = "fn f() I32 { return ~5 }", .expected = "= xor i32 -1, 5" },
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
        .{ .src = "fn f() I32 { return 5 && 3 }", .expected_conversions = "icmp ne i32", .expected_op = "and i1" },
        .{ .src = "fn f() I32 { return 5 || 3 }", .expected_conversions = "icmp ne i32", .expected_op = "or i1" },
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

    const source = "fn f() I32 { return if true { 1 } else { 0 } }";
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

    const source = "fn f() I32 { return if true { 42 } else { 0 } }";
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
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "else1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge2:") != null);

    // Check phi node
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "phi i32") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "[ 42,") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "[ 0,") != null);
}

test "codegen: if with integer condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() I32 { return if 5 { 1 } else { 0 } }";
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

    const source = "fn f() I32 { return if 10 > 5 { 1 } else { 0 } }";
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

    const source = "fn f() I32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
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
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then3:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge2:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge5:") != null);

    // Check multiple phi nodes
    const phi_count = std.mem.count(u8, llvm_ir, "phi i32");
    try testing.expectEqual(@as(usize, 2), phi_count);
}

test "codegen: elseif creates nested if" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
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
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "else1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "then3:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "else4:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "merge5:") != null);

    // Check phi nodes with correct predecessors
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "phi i32 [ 2, %then3 ], [ 3, %else4 ]") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "phi i32 [ 1, %then0 ]") != null);
}

test "codegen: basic block tracking in elseif" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
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

    // The outer phi node should reference merge5 (not else1) as predecessor
    // because after evaluating the nested if in else1, we're in merge5
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "%merge5 ]") != null);
}

test "codegen: simple while loop" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() I32 { while true { 42 }; return 0 }";
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

    // Should have header, body, and exit labels
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_body1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_exit2:") != null);

    // Should have conditional branch in header
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "br i1") != null);

    // Body should branch back to header
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "br label %while_header0") != null);

    // Should return 0
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "ret i32 0") != null);
}

test "codegen: while loop with comparison condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() I32 { while 5 > 3 { 1 }; return 0 }";
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

    // Should have comparison in condition
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "icmp sgt i32 5, 3") != null);
}

test "codegen: nested while loops" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() I32 { while true { while false { 1 } }; return 0 }";
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

    // Should have two sets of while blocks (outer and inner)
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_header3:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_body1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_body4:") != null);
}

