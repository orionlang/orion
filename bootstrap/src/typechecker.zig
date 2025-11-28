const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;
const Pattern = parser.Pattern;

const FunctionSignature = struct {
    params: []const Type,
    return_type: Type,
    is_unsafe: bool,
};

const TypeCheckError = error{
    TypeMismatch,
    UndefinedVariable,
    UndefinedFunction,
    UndefinedType,
    ArgumentCountMismatch,
    EmptyFunctionBody,
    OutOfMemory,
    AssignmentToImmutable,
    LinearityViolation,
    UnsafeCallOutsideUnsafeContext,
    SpawnOutsideAsyncScope,
};

const VariableInfo = struct {
    var_type: Type,
    mutable: bool,
    use_count: u32,
};

const TypeDefInfo = struct {
    params: []const parser.TypeParameter,
    type_value: Type,
};

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(VariableInfo),
    functions: std.StringHashMap(FunctionSignature),
    type_defs: std.StringHashMap(TypeDefInfo),
    instance_methods: std.StringHashMap(FunctionSignature),
    allocated_tuple_types: std.ArrayList(Type),
    in_unsafe_block: bool,
    // Track allocations from substituteTypeParams for cleanup
    substituted_types: std.ArrayList(*Type),
    substituted_fields: std.ArrayList([]parser.StructField),
    // Track type_params allocations for Task[T] cleanup
    allocated_type_params: std.ArrayList([]parser.TypeParam),
    // Concurrency: track async scope depth for structured concurrency
    async_scope_depth: u32,

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(VariableInfo).init(allocator),
            .functions = std.StringHashMap(FunctionSignature).init(allocator),
            .type_defs = std.StringHashMap(TypeDefInfo).init(allocator),
            .instance_methods = std.StringHashMap(FunctionSignature).init(allocator),
            .allocated_tuple_types = .empty,
            .in_unsafe_block = false,
            .substituted_types = .empty,
            .substituted_fields = .empty,
            .allocated_type_params = .empty,
            .async_scope_depth = 0,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        var iter = self.functions.valueIterator();
        while (iter.next()) |sig| {
            self.allocator.free(sig.params);
        }
        // Free all tuple types (shallow - nested tuples tracked separately)
        for (self.allocated_tuple_types.items) |*tuple_type| {
            tuple_type.deinitShallow(self.allocator);
        }
        self.allocated_tuple_types.deinit(self.allocator);

        // Free instance method signatures
        var method_iter = self.instance_methods.iterator();
        while (method_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.params);
        }

        // Free substituted types from generic type parameter substitution
        for (self.substituted_types.items) |typ| {
            self.allocator.destroy(typ);
        }
        self.substituted_types.deinit(self.allocator);

        for (self.substituted_fields.items) |fields| {
            self.allocator.free(fields);
        }
        self.substituted_fields.deinit(self.allocator);

        // Free type_params allocated for Task[T] dependent types
        for (self.allocated_type_params.items) |type_params| {
            self.allocator.free(type_params);
        }
        self.allocated_type_params.deinit(self.allocator);

        self.variables.deinit();
        self.functions.deinit();
        self.type_defs.deinit();
        self.instance_methods.deinit();
    }

    pub fn check(self: *TypeChecker, ast: *const AST) !void {
        // First pass: collect type definitions (with type parameters for generics)
        for (ast.type_defs.items) |typedef| {
            try self.type_defs.put(typedef.name, .{
                .params = typedef.params,
                .type_value = typedef.type_value,
            });
        }

        // Second pass: collect instance methods
        for (ast.instances.items) |instance| {
            const type_name = blk: {
                switch (instance.type_name.kind) {
                    .named => |name| break :blk try self.allocator.dupe(u8, name),
                    .primitive => |prim| break :blk try self.allocator.dupe(u8, @tagName(prim)),
                    .dependent => |dep| {
                        // Mangle dependent types: Vec$ptr$8
                        var mangled = std.ArrayList(u8).empty;
                        defer mangled.deinit(self.allocator);
                        try mangled.appendSlice(self.allocator, dep.base);
                        for (dep.type_params) |param| {
                            try mangled.append(self.allocator, '$');
                            switch (param) {
                                .variable => |v| try mangled.appendSlice(self.allocator, v),
                                .concrete => |t| {
                                    const type_str = switch (t.kind) {
                                        .primitive => |prim| @tagName(prim),
                                        .named => |name| name,
                                        else => "unknown",
                                    };
                                    try mangled.appendSlice(self.allocator, type_str);
                                },
                            }
                        }
                        // Add value parameters
                        for (dep.value_params) |val_param| {
                            try mangled.append(self.allocator, '$');
                            if (val_param == .integer_literal) {
                                const int_str = try std.fmt.allocPrint(self.allocator, "{d}", .{val_param.integer_literal.value});
                                defer self.allocator.free(int_str);
                                try mangled.appendSlice(self.allocator, int_str);
                            }
                        }
                        break :blk try self.allocator.dupe(u8, mangled.items);
                    },
                    else => break :blk try self.allocator.dupe(u8, "unknown"),
                }
            };
            defer self.allocator.free(type_name);

            for (instance.methods) |method| {
                const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{type_name, method.name});
                const param_types = try self.allocator.alloc(Type, method.params.len);
                for (method.params, 0..) |param, i| {
                    param_types[i] = param.param_type;
                }
                try self.instance_methods.put(mangled_name, .{
                    .params = param_types,
                    .return_type = method.return_type,
                    .is_unsafe = false, // Methods inherit safety from context
                });
            }
        }

        // Third pass: collect function signatures (including extern)
        for (ast.extern_functions.items) |extern_func| {
            const param_types = try self.allocator.alloc(Type, extern_func.params.len);
            for (extern_func.params, 0..) |param, i| {
                param_types[i] = param.param_type;
            }
            try self.functions.put(extern_func.name, .{
                .params = param_types,
                .return_type = extern_func.return_type,
                .is_unsafe = true,
            });
        }
        for (ast.functions.items) |func| {
            const param_types = try self.allocator.alloc(Type, func.params.len);
            for (func.params, 0..) |param, i| {
                param_types[i] = param.param_type;
            }
            try self.functions.put(func.name, .{
                .params = param_types,
                .return_type = func.return_type,
                .is_unsafe = func.is_unsafe,
            });
        }

        // Fourth pass: check function bodies
        for (ast.functions.items) |func| {
            try self.checkFunction(&func);
        }
    }

    fn printTypeMismatch(context: []const u8, name: []const u8, expected: Type, actual: Type) void {
        std.debug.print("Type mismatch in {s} '{s}': expected {s}, got {s}\n", .{
            context,
            name,
            @tagName(expected.kind),
            @tagName(actual.kind),
        });
    }

    fn checkFunction(self: *TypeChecker, func: *const parser.FunctionDecl) !void {
        // Clear variables from previous function
        self.variables.clearRetainingCapacity();

        // Set unsafe context if function is unsafe
        const previous_unsafe = self.in_unsafe_block;
        if (func.is_unsafe) {
            self.in_unsafe_block = true;
        }
        defer self.in_unsafe_block = previous_unsafe;

        // Add parameters to variable environment (immutable)
        for (func.params) |param| {
            try self.variables.put(param.name, .{ .var_type = param.param_type, .mutable = false, .use_count = 0 });
        }

        // Check that function has a body
        if (func.body.items.len == 0) {
            return error.EmptyFunctionBody;
        }

        // Check all statements
        for (func.body.items) |stmt| {
            try self.checkStatement(stmt, func.return_type);
        }

        // Validate that at least one return statement exists and has correct type
        // Type validation happens in checkStatement for .return_stmt case

        // Check linearity constraints
        try self.checkLinearity();
    }

    fn checkLinearity(self: *TypeChecker) !void {
        // Skip linearity checks in unsafe blocks
        if (self.in_unsafe_block) {
            return;
        }

        var iter = self.variables.iterator();
        while (iter.next()) |entry| {
            const name = entry.key_ptr.*;
            const var_info = entry.value_ptr.*;

            // Mutable variables are not subject to linearity constraints (for now)
            if (var_info.mutable) {
                continue;
            }

            const usage = var_info.var_type.usage;
            const use_count = var_info.use_count;

            if (!usage.isValid(use_count)) {
                std.debug.print("Linearity violation for variable '{s}': ", .{name});
                switch (usage) {
                    .once => std.debug.print("expected exactly 1 use, got {d}\n", .{use_count}),
                    .exactly => |n| std.debug.print("expected exactly {d} uses, got {d}\n", .{ n, use_count }),
                    .optional => std.debug.print("expected 0 or 1 uses, got {d}\n", .{use_count}),
                    .unlimited => unreachable, // unlimited is always valid
                }
                return error.LinearityViolation;
            }
        }
    }

    fn checkStatement(self: *TypeChecker, stmt: Stmt, func_return_type: ?Type) TypeCheckError!void {
        switch (stmt) {
            .let_binding => |binding| {
                // Type check and infer the binding value
                // Only call one of the two functions to avoid double-counting linearity
                var var_type: Type = undefined;
                if (binding.type_annotation) |expected_type| {
                    try self.checkExprWithExpectedType(binding.value, expected_type);
                    var_type = expected_type;
                } else {
                    var_type = try self.inferExprType(binding.value);
                }

                // Bind pattern to variables
                try self.bindPattern(binding.pattern, var_type, binding.mutable);
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
                // Check the RHS expression against the variable's type
                // This also validates type compatibility and tracks linearity
                try self.checkExprWithExpectedType(assign.value, var_info.var_type);
            },
            .while_stmt => |while_stmt| {
                const condition_type = try self.inferExprType(while_stmt.condition);
                if (condition_type.kind != .primitive or condition_type.kind.primitive != .bool) {
                    std.debug.print("While condition must be bool, got {any}\n", .{condition_type});
                    return error.TypeMismatch;
                }
                _ = try self.inferExprType(while_stmt.body);
            },
            .if_stmt => |if_stmt| {
                const condition_type = try self.inferExprType(if_stmt.condition);
                if (condition_type.kind != .primitive or condition_type.kind.primitive != .bool) {
                    std.debug.print("If condition must be bool, got {any}\n", .{condition_type});
                    return error.TypeMismatch;
                }
                for (if_stmt.then_stmts) |*then_stmt| {
                    try self.checkStatement(then_stmt.*, func_return_type);
                }
                if (if_stmt.else_stmts) |else_stmts| {
                    for (else_stmts) |*else_stmt| {
                        try self.checkStatement(else_stmt.*, func_return_type);
                    }
                }
            },
            .return_stmt => |ret_expr| {
                if (func_return_type) |expected_type| {
                    // Use context to type literals
                    try self.checkExprWithExpectedType(ret_expr, expected_type);
                } else {
                    // No expected type, just infer
                    _ = try self.inferExprType(ret_expr);
                }
            },
            .expr => |expr| {
                _ = try self.inferExprType(expr);
            },
        }
    }

    fn bindPattern(self: *TypeChecker, pattern: Pattern, value_type: Type, mutable: bool) !void {
        switch (pattern) {
            .identifier => |name| {
                try self.variables.put(name, .{ .var_type = value_type, .mutable = mutable, .use_count = 0 });
            },
            .tuple => |sub_patterns| {
                if (value_type.kind != .tuple) {
                    std.debug.print("Cannot destructure non-tuple type\n", .{});
                    return error.TypeMismatch;
                }
                if (sub_patterns.len != value_type.kind.tuple.len) {
                    std.debug.print("Pattern has {d} elements but value has {d}\n", .{ sub_patterns.len, value_type.kind.tuple.len });
                    return error.TypeMismatch;
                }
                for (sub_patterns, value_type.kind.tuple) |sub_pattern, elem_type| {
                    try self.bindPattern(sub_pattern.*, elem_type.*, mutable);
                }
            },
        }
    }

    fn checkExprWithExpectedType(self: *TypeChecker, expr: *Expr, expected_type: Type) TypeCheckError!void {
        // For integer literals, update the inferred type and validate range
        switch (expr.*) {
            .string_literal => {
                // String literals are str type
                if (expected_type.kind != .primitive or expected_type.kind.primitive != .str) {
                    std.debug.print("String literal has type str but expected different type\n", .{});
                    return error.TypeMismatch;
                }
            },
            .tuple_literal => |elements| {
                if (expected_type.kind != .tuple) {
                    std.debug.print("Expected tuple type but got non-tuple\n", .{});
                    return error.TypeMismatch;
                }
                if (elements.len != expected_type.kind.tuple.len) {
                    std.debug.print("Tuple literal has {d} elements but expected {d}\n", .{ elements.len, expected_type.kind.tuple.len });
                    return error.TypeMismatch;
                }
                for (elements, expected_type.kind.tuple) |elem, expected_elem_type| {
                    try self.checkExprWithExpectedType(elem, expected_elem_type.*);
                }
            },
            .tuple_index => {
                // Tuple indexing result type is determined by the tuple structure, not expected type
            },
            .integer_literal => |*lit| {
                // Check if the value fits in the expected type
                const value = lit.value;
                const resolved_type = self.resolveType(expected_type);
                const min = resolved_type.minValue();
                const max = resolved_type.maxValue();

                if (value < min or value > max) {
                    // Get type name for error message
                    const type_name = switch (resolved_type.kind) {
                        .primitive => |prim| @tagName(prim),
                        else => @tagName(resolved_type.kind),
                    };

                    // Provide clear error messages for common cases
                    if (!resolved_type.isSigned() and value < 0) {
                        std.debug.print("Integer literal {d} is negative but type {s} is unsigned\n", .{
                            value,
                            type_name,
                        });
                    } else {
                        std.debug.print("Integer literal {d} does not fit in type {s} (range: {d} to {d})\n", .{
                            value,
                            type_name,
                            min,
                            max,
                        });
                    }
                    return error.TypeMismatch;
                }

                // Update the inferred type (but only for primitives to avoid double-free with heap-allocated types)
                lit.inferred_type = resolved_type;
            },
            .binary_op => |*binop| {
                if (!binop.op.returns_bool()) {
                    // For arithmetic binary operations, propagate expected type to both operands
                    // This allows `let x: u32 = 10 + 5` to type both literals as u32
                    try self.checkExprWithExpectedType(binop.left, expected_type);
                    try self.checkExprWithExpectedType(binop.right, expected_type);
                } else {
                    // For comparison/logical ops, just validate without context
                    _ = try self.inferExprType(expr);
                }
            },
            .unary_op => {
                // For unary operations, just validate without context
                _ = try self.inferExprType(expr);
            },
            .if_expr => |*if_expr| {
                // Check condition is bool and track linearity
                const bool_type = Type{ .kind = .{ .primitive = .bool }, .usage = .once };
                try self.checkExprWithExpectedType(if_expr.condition, bool_type);

                // Propagate expected type to both branches
                // Example: `let x: i64 = if cond { 42 } else { 100 }` should type both literals as i64
                try self.checkExprWithExpectedType(if_expr.then_branch, expected_type);
                if (if_expr.else_branch) |else_branch| {
                    try self.checkExprWithExpectedType(else_branch, expected_type);
                }
            },
            .block_expr => |*block| {
                // Propagate expected type to result expression
                // Example: `let x: i64 = { stmt; stmt; 42 }` should type 42 as i64
                if (block.result) |result| {
                    try self.checkExprWithExpectedType(result, expected_type);
                }
            },
            .unsafe_block => |*block| {
                // Propagate expected type to result expression
                if (block.result) |result| {
                    try self.checkExprWithExpectedType(result, expected_type);
                }
            },
            .struct_literal => |lit| {
                // Propagate expected types to struct fields
                // For generic types, resolve with substitution first
                const resolved = self.resolveType(expected_type);
                if (resolved.kind != .struct_type) return;

                // Propagate expected types to each field
                for (lit.fields) |field| {
                    for (resolved.kind.struct_type) |type_field| {
                        if (std.mem.eql(u8, field.name, type_field.name)) {
                            try self.checkExprWithExpectedType(field.value, type_field.field_type.*);
                            break;
                        }
                    }
                }
            },
            .function_call, .variable, .field_access => {
                // Function calls, variables, and field accesses need to track linearity
                _ = try self.inferExprType(expr);
            },
            .bool_literal, .constructor_call, .dependent_type_ref, .match_expr, .method_call, .intrinsic_call, .async_expr, .spawn_expr, .select_expr => {
                // These expressions have fixed types that can't be influenced by context.
                // Linearity tracking happens in inferExprType when needed.
            },
        }
    }

    fn inferExprType(self: *TypeChecker, expr: *const Expr) !Type {
        switch (expr.*) {
            .integer_literal => |lit| return lit.inferred_type,
            .bool_literal => return .{ .kind = .{ .primitive = .bool  }, .usage = .once },
            .string_literal => {
                // String literals are str type (unlimited usage like other primitives)
                return .{ .kind = .{ .primitive = .str }, .usage = .unlimited };
            },
            .tuple_literal => |elements| {
                const elem_types = try self.allocator.alloc(*Type, elements.len);
                for (elements, 0..) |elem, i| {
                    const elem_type_ptr = try self.allocator.create(Type);
                    elem_type_ptr.* = try self.inferExprType(elem);
                    elem_types[i] = elem_type_ptr;
                }
                const tuple_type = Type{ .kind = .{ .tuple = elem_types  }, .usage = .once };
                // Track ALL tuple types (including nested) for cleanup
                try self.allocated_tuple_types.append(self.allocator, tuple_type);
                return tuple_type;
            },
            .tuple_index => |index| {
                const tuple_type = try self.inferExprType(index.tuple);
                if (tuple_type.kind != .tuple) {
                    std.debug.print("Cannot index non-tuple type\n", .{});
                    return error.TypeMismatch;
                }
                if (index.index >= tuple_type.kind.tuple.len) {
                    std.debug.print("Tuple index {d} out of bounds (tuple has {d} elements)\n", .{ index.index, tuple_type.kind.tuple.len });
                    return error.TypeMismatch;
                }
                return tuple_type.kind.tuple[index.index].*;
            },
            .variable => |name| {
                if (self.variables.getPtr(name)) |var_info| {
                    var_info.use_count += 1;
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
                    if (left_type.kind == .primitive and left_type.kind.primitive == .bool and
                        right_type.kind == .primitive and right_type.kind.primitive == .bool)
                    {
                        return .{ .kind = .{ .primitive = .bool  }, .usage = .once };
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
                if (binop.op.returns_bool()) return .{ .kind = .{ .primitive = .bool  }, .usage = .once };

                // For arithmetic operators, both operands must be the same type
                if (!self.typesMatch(left_type, right_type)) {
                    std.debug.print("Binary operator type mismatch: left is {s}, right is {s}\n", .{
                        @tagName(left_type.kind),
                        @tagName(right_type.kind),
                    });
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
                return if (unop.op.returns_bool()) .{ .kind = .{ .primitive = .bool }, .usage = .once } else operand_type;
            },
            .function_call => |call| {
                // Look up function signature
                const sig = self.functions.get(call.name) orelse {
                    std.debug.print("Undefined function: {s}\n", .{call.name});
                    return error.UndefinedFunction;
                };

                // Check that unsafe functions are only called from unsafe contexts
                if (sig.is_unsafe and !self.in_unsafe_block) {
                    std.debug.print("Unsafe function '{s}' can only be called from unsafe blocks or unsafe functions\n", .{call.name});
                    return error.UnsafeCallOutsideUnsafeContext;
                }

                // Check argument count
                if (call.args.len != sig.params.len) {
                    std.debug.print("Function {s} expects {d} arguments, got {d}\n", .{
                        call.name,
                        sig.params.len,
                        call.args.len,
                    });
                    return error.ArgumentCountMismatch;
                }

                // Check argument types using inferExprType to track linearity
                // (we avoid checkExprWithExpectedType here because it would be called twice)
                for (call.args, 0..) |arg, i| {
                    const expected_type = sig.params[i];
                    const arg_type = try self.inferExprType(arg);
                    if (!self.canImplicitlyConvert(arg_type, expected_type)) {
                        std.debug.print("Function {s} argument {d}: type mismatch\n", .{
                            call.name,
                            i,
                        });
                        return error.TypeMismatch;
                    }
                }

                return sig.return_type;
            },
            .if_expr => |if_expr| {
                // Condition must be bool
                const condition_type = try self.inferExprType(if_expr.condition);
                if (condition_type.kind != .primitive or condition_type.kind.primitive != .bool) {
                    std.debug.print("If condition must be bool, got {any}\n", .{condition_type});
                    return error.TypeMismatch;
                }

                // Then branch type
                const then_type = try self.inferExprType(if_expr.then_branch);

                // If there's an else branch, both branches must have same type
                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.inferExprType(else_branch);
                    if (!self.typesMatch(then_type, else_type)) {
                        std.debug.print("If branches have different types: then={s}, else={s}\n", .{
                            @tagName(then_type.kind),
                            @tagName(else_type.kind),
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
                    return .{ .kind = .{ .primitive = .i32  }, .usage = .once };
                }
            },
            .unsafe_block => |block| {
                // Save current unsafe state
                const previous_unsafe = self.in_unsafe_block;
                self.in_unsafe_block = true;
                defer self.in_unsafe_block = previous_unsafe;

                // Track which variables existed before entering the unsafe block
                var existing_vars = std.StringHashMap(void).init(self.allocator);
                defer existing_vars.deinit();
                var iter = self.variables.keyIterator();
                while (iter.next()) |key| {
                    try existing_vars.put(key.*, {});
                }

                for (block.statements) |stmt| {
                    try self.checkStatement(stmt, null);
                }

                // Mark all variables used or declared in unsafe block as unlimited
                // This exempts them from linearity checking
                var var_iter = self.variables.iterator();
                while (var_iter.next()) |entry| {
                    // If variable was declared inside unsafe block, or existed before and might have been used inside
                    entry.value_ptr.var_type.usage = .unlimited;
                }

                if (block.result) |result| {
                    return try self.inferExprType(result);
                } else {
                    return .{ .kind = .{ .primitive = .i32  }, .usage = .once };
                }
            },
            .struct_literal => |lit| {
                const typedef_info = self.type_defs.get(lit.type_name) orelse {
                    std.debug.print("Undefined type: {s}\n", .{lit.type_name});
                    return error.UndefinedVariable;
                };

                // Build substitution map if type has type parameters
                var substitutions = std.StringHashMap(Type).init(self.allocator);
                defer substitutions.deinit();

                if (lit.type_params) |type_params| {
                    for (typedef_info.params, 0..) |param, i| {
                        if (param.kind == .type_param) {
                            if (i < type_params.len) {
                                switch (type_params[i]) {
                                    .concrete => |t| {
                                        substitutions.put(param.name, t.*) catch continue;
                                    },
                                    .variable => {},
                                }
                            }
                        }
                    }
                }

                // Apply substitutions to get resolved typedef
                const typedef = if (substitutions.count() > 0)
                    self.substituteTypeParams(typedef_info.type_value, substitutions)
                else
                    typedef_info.type_value;

                if (typedef.kind != .struct_type) {
                    std.debug.print("Type {s} is not a struct\n", .{lit.type_name});
                    return error.TypeMismatch;
                }

                if (lit.fields.len != typedef.kind.struct_type.len) {
                    std.debug.print("Struct literal for {s} has {d} fields but type has {d}\n", .{
                        lit.type_name,
                        lit.fields.len,
                        typedef.kind.struct_type.len,
                    });
                    return error.TypeMismatch;
                }

                for (lit.fields) |field| {
                    var found = false;
                    for (typedef.kind.struct_type) |type_field| {
                        if (std.mem.eql(u8, field.name, type_field.name)) {
                            // Check field value against expected type (includes linearity tracking)
                            try self.checkExprWithExpectedType(field.value, type_field.field_type.*);
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        std.debug.print("Struct {s} has no field named {s}\n", .{ lit.type_name, field.name });
                        return error.TypeMismatch;
                    }
                }

                // Return dependent type if we had type params
                if (lit.type_params != null) {
                    return .{
                        .kind = .{ .dependent = .{
                            .base = lit.type_name,
                            .type_params = lit.type_params.?,
                            .value_params = lit.value_params orelse &[_]Expr{},
                        } },
                        .usage = .once,
                    };
                }
                return .{ .kind = .{ .named = lit.type_name  }, .usage = .once };
            },
            .field_access => |access| {
                const object_type = try self.inferExprType(access.object);

                // Get the base type name (handle both .named and .dependent)
                const type_name = switch (object_type.kind) {
                    .named => |name| name,
                    .dependent => |dep| dep.base,
                    else => {
                        std.debug.print("Cannot access field of non-struct type\n", .{});
                        return error.TypeMismatch;
                    },
                };

                const typedef_info = self.type_defs.get(type_name) orelse {
                    std.debug.print("Undefined type: {s}\n", .{type_name});
                    return error.UndefinedVariable;
                };

                // Build substitution map if object is a dependent type
                var substitutions = std.StringHashMap(Type).init(self.allocator);
                defer substitutions.deinit();

                if (object_type.kind == .dependent) {
                    const dep = object_type.kind.dependent;
                    for (typedef_info.params, 0..) |param, i| {
                        if (param.kind == .type_param) {
                            if (i < dep.type_params.len) {
                                switch (dep.type_params[i]) {
                                    .concrete => |t| {
                                        substitutions.put(param.name, t.*) catch continue;
                                    },
                                    .variable => {},
                                }
                            }
                        }
                    }
                }

                // Apply substitutions to get resolved typedef
                const typedef = if (substitutions.count() > 0)
                    self.substituteTypeParams(typedef_info.type_value, substitutions)
                else
                    typedef_info.type_value;

                if (typedef.kind != .struct_type) {
                    std.debug.print("Type {s} is not a struct\n", .{type_name});
                    return error.TypeMismatch;
                }

                for (typedef.kind.struct_type) |field| {
                    if (std.mem.eql(u8, field.name, access.field_name)) {
                        return field.field_type.*;
                    }
                }

                std.debug.print("Struct {s} has no field named {s}\n", .{ type_name, access.field_name });
                return error.TypeMismatch;
            },
            .constructor_call => |call| {
                // Find which type this constructor belongs to
                var found_type: ?[]const u8 = null;
                var found_variant: ?parser.SumTypeVariant = null;

                var iter = self.type_defs.iterator();
                while (iter.next()) |entry| {
                    if (entry.value_ptr.*.type_value.kind == .sum_type) {
                        for (entry.value_ptr.*.type_value.kind.sum_type) |variant| {
                            if (std.mem.eql(u8, variant.name, call.name)) {
                                found_type = entry.key_ptr.*;
                                found_variant = variant;
                                break;
                            }
                        }
                        if (found_type != null) break;
                    }
                }

                if (found_type == null) {
                    std.debug.print("Undefined constructor: {s}\n", .{call.name});
                    return error.UndefinedVariable;
                }

                const variant = found_variant.?;
                if (call.args.len != variant.payload_types.len) {
                    std.debug.print("Constructor {s} expects {d} arguments but got {d}\n", .{
                        call.name,
                        variant.payload_types.len,
                        call.args.len,
                    });
                    return error.ArgumentCountMismatch;
                }

                for (call.args, variant.payload_types) |arg, expected_type| {
                    const arg_type = try self.inferExprType(arg);
                    if (!self.typesMatch(arg_type, expected_type.*)) {
                        std.debug.print("Constructor {s} argument has wrong type\n", .{call.name});
                        return error.TypeMismatch;
                    }
                }

                return .{ .kind = .{ .named = found_type.?  }, .usage = .once };
            },
            .match_expr => |match_expr| {
                const scrutinee_type = try self.inferExprType(match_expr.scrutinee);

                if (scrutinee_type.kind != .named) {
                    std.debug.print("Match expression requires a named type\n", .{});
                    return error.TypeMismatch;
                }

                const typedef_info = self.type_defs.get(scrutinee_type.kind.named) orelse {
                    std.debug.print("Undefined type: {s}\n", .{scrutinee_type.kind.named});
                    return error.UndefinedVariable;
                };
                const typedef = typedef_info.type_value;

                if (typedef.kind != .sum_type) {
                    std.debug.print("Match expression requires a sum type\n", .{});
                    return error.TypeMismatch;
                }

                if (match_expr.arms.len == 0) {
                    std.debug.print("Match expression must have at least one arm\n", .{});
                    return error.TypeMismatch;
                }

                // Check each arm and ensure all arms return the same type
                var result_type: ?Type = null;
                for (match_expr.arms) |arm| {
                    // Save current variables before adding bindings
                    const saved_vars_count = self.variables.count();

                    // Add pattern bindings to scope
                    switch (arm.pattern) {
                        .identifier => {},
                        .constructor => |constructor| {
                            var found = false;
                            var found_variant: ?parser.SumTypeVariant = null;
                            for (typedef.kind.sum_type) |variant| {
                                if (std.mem.eql(u8, variant.name, constructor.name)) {
                                    if (constructor.bindings.len != variant.payload_types.len) {
                                        std.debug.print("Constructor pattern {s} has {d} bindings but variant has {d} payloads\n", .{
                                            constructor.name,
                                            constructor.bindings.len,
                                            variant.payload_types.len,
                                        });
                                        return error.TypeMismatch;
                                    }
                                    found = true;
                                    found_variant = variant;
                                    break;
                                }
                            }
                            if (!found) {
                                std.debug.print("Unknown constructor in pattern: {s}\n", .{constructor.name});
                                return error.UndefinedVariable;
                            }

                            // Add bindings to variable scope
                            if (found_variant) |variant| {
                                for (constructor.bindings, variant.payload_types) |binding, payload_type| {
                                    try self.variables.put(binding, .{ .var_type = payload_type.*, .mutable = false, .use_count = 0 });
                                }
                            }
                        },
                    }

                    // Infer arm body type
                    const arm_type = try self.inferExprType(arm.body);
                    if (result_type) |rt| {
                        if (!self.typesMatch(rt, arm_type)) {
                            std.debug.print("Match arms must all return the same type\n", .{});
                            return error.TypeMismatch;
                        }
                    } else {
                        result_type = arm_type;
                    }

                    // Restore variables after arm (remove bindings)
                    // Simple approach: remove all variables added after saved count
                    while (self.variables.count() > saved_vars_count) {
                        // Remove last added variable
                        var iter = self.variables.iterator();
                        var last_key: ?[]const u8 = null;
                        while (iter.next()) |entry| {
                            last_key = entry.key_ptr.*;
                        }
                        if (last_key) |key| {
                            _ = self.variables.remove(key);
                        }
                    }
                }

                return result_type.?;
            },
            .method_call => |method_call| {
                // Special case: if object is a dependent_type_ref, construct the type name directly
                const type_name = if (method_call.object.* == .dependent_type_ref) blk: {
                    const ref = method_call.object.dependent_type_ref;
                    // Mangle: Vec$ptr$8
                    var mangled = std.ArrayList(u8).empty;
                    defer mangled.deinit(self.allocator);
                    try mangled.appendSlice(self.allocator, ref.type_name);
                    for (ref.type_params) |param| {
                        try mangled.append(self.allocator, '$');
                        switch (param) {
                            .variable => |v| try mangled.appendSlice(self.allocator, v),
                            .concrete => |t| {
                                const type_str = switch (t.kind) {
                                    .primitive => |prim| @tagName(prim),
                                    .named => |name| name,
                                    else => "unknown",
                                };
                                try mangled.appendSlice(self.allocator, type_str);
                            },
                        }
                    }
                    // Add value parameters to mangling
                    for (ref.value_params) |val_param| {
                        try mangled.append(self.allocator, '$');
                        // For now, only handle integer literals
                        if (val_param == .integer_literal) {
                            const int_str = try std.fmt.allocPrint(self.allocator, "{d}", .{val_param.integer_literal.value});
                            defer self.allocator.free(int_str);
                            try mangled.appendSlice(self.allocator, int_str);
                        }
                    }
                    break :blk try self.allocator.dupe(u8, mangled.items);
                } else blk: {
                    // Infer the type of the object
                    const object_type = try self.inferExprType(method_call.object);

                    // Extract type name
                    const name = switch (object_type.kind) {
                        .named => |name| name,
                        .primitive => |prim| @tagName(prim),
                        .dependent => |dep| {
                            // Mangle dependent type: Vec$ptr$8
                            var mangled = std.ArrayList(u8).empty;
                            defer mangled.deinit(self.allocator);
                            try mangled.appendSlice(self.allocator, dep.base);
                            for (dep.type_params) |param| {
                                try mangled.append(self.allocator, '$');
                                switch (param) {
                                    .variable => |v| try mangled.appendSlice(self.allocator, v),
                                    .concrete => |t| {
                                        const type_str = switch (t.kind) {
                                            .primitive => |prim| @tagName(prim),
                                            .named => |n| n,
                                            else => "unknown",
                                        };
                                        try mangled.appendSlice(self.allocator, type_str);
                                    },
                                }
                            }
                            // Add value parameters to mangling
                            for (dep.value_params) |val_param| {
                                try mangled.append(self.allocator, '$');
                                if (val_param == .integer_literal) {
                                    const int_str = try std.fmt.allocPrint(self.allocator, "{d}", .{val_param.integer_literal.value});
                                    defer self.allocator.free(int_str);
                                    try mangled.appendSlice(self.allocator, int_str);
                                }
                            }
                            break :blk try self.allocator.dupe(u8, mangled.items);
                        },
                        else => "unknown",
                    };
                    break :blk try self.allocator.dupe(u8, name);
                };
                defer self.allocator.free(type_name);

                // Look up the method in instance_methods
                const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{type_name, method_call.method_name});
                defer self.allocator.free(mangled_name);

                if (self.instance_methods.get(mangled_name)) |sig| {
                    // Check argument types (excluding self parameter which is first)
                    // For instance methods, params[0] is self, rest are method args
                    // For type methods (static), all params are method args
                    const is_type_method = method_call.object.* == .dependent_type_ref;
                    const param_offset: usize = if (is_type_method) 0 else 1;
                    const expected_arg_count = sig.params.len - param_offset;

                    if (method_call.args.len != expected_arg_count) {
                        std.debug.print("Method {s} expects {d} arguments, got {d}\n", .{
                            method_call.method_name,
                            expected_arg_count,
                            method_call.args.len,
                        });
                        return error.ArgumentCountMismatch;
                    }

                    // Check argument types with expected type context
                    for (method_call.args, 0..) |arg, i| {
                        const expected_type = sig.params[i + param_offset];

                        // Use checkExprWithExpectedType to allow integer literals to coerce
                        try self.checkExprWithExpectedType(arg, expected_type);

                        // Now verify the actual type matches or can convert
                        const arg_type = try self.inferExprType(arg);
                        if (!self.canImplicitlyConvert(arg_type, expected_type)) {
                            const expected_str = switch (expected_type.kind) {
                                .primitive => |p| @tagName(p),
                                else => @tagName(expected_type.kind),
                            };
                            const got_str = switch (arg_type.kind) {
                                .primitive => |p| @tagName(p),
                                else => @tagName(arg_type.kind),
                            };
                            std.debug.print("Method {s} argument {d}: expected {s}, got {s}\n", .{
                                method_call.method_name,
                                i,
                                expected_str,
                                got_str,
                            });
                            return error.TypeMismatch;
                        }
                    }

                    return sig.return_type;
                } else {
                    // Method not found
                    std.debug.print("Method {s} not found on type {s}\n", .{method_call.method_name, type_name});
                    return error.UndefinedFunction;
                }
            },
            .intrinsic_call => |call| {
                // Type check intrinsic calls
                // @ptr_of(value: T) -> Ptr[T]  (for now, returns a named type "Ptr")
                // @ptr_read(ptr: Ptr[T]) -> T  (for now, returns i32)
                // @ptr_write(ptr: Ptr[T], value: T) -> ()
                // @ptr_offset(ptr: Ptr[T], offset: i32) -> Ptr[T]

                if (std.mem.eql(u8, call.name, "@ptr_of")) {
                    if (call.args.len != 1) {
                        std.debug.print("@ptr_of expects 1 argument but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    _ = try self.inferExprType(call.args[0]);
                    // Return ptr primitive type
                    return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@type")) {
                    if (call.args.len != 1) {
                        std.debug.print("@type expects 1 argument but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    // @type returns Type primitive
                    return .{ .kind = .{ .primitive = .type }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_read")) {
                    if (call.args.len != 2) {
                        std.debug.print("@ptr_read expects 2 arguments but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    _ = try self.inferExprType(call.args[0]);

                    // Second argument should be @type(TypeName)
                    const type_arg = call.args[1];
                    if (type_arg.* != .intrinsic_call or !std.mem.eql(u8, type_arg.intrinsic_call.name, "@type")) {
                        std.debug.print("@ptr_read second argument must be @type(...)\n", .{});
                        return error.TypeMismatch;
                    }
                    const type_name_expr = type_arg.intrinsic_call.args[0];
                    const type_name = switch (type_name_expr.*) {
                        .variable => |v| v,
                        .constructor_call => |c| c.name,
                        else => {
                            std.debug.print("@type argument must be a type name, got: {s}\n", .{@tagName(type_name_expr.*)});
                            return error.TypeMismatch;
                        },
                    };

                    // Map type name to primitive type using parser's type_name_map
                    const type_map = std.StaticStringMap(parser.PrimitiveType).initComptime(.{
                        .{ "bool", .bool },
                        .{ "i8", .i8 },
                        .{ "i16", .i16 },
                        .{ "i32", .i32 },
                        .{ "i64", .i64 },
                        .{ "u8", .u8 },
                        .{ "u16", .u16 },
                        .{ "u32", .u32 },
                        .{ "u64", .u64 },
                    });

                    const prim_type = type_map.get(type_name) orelse {
                        std.debug.print("Unknown type name in @type: {s}\n", .{type_name});
                        return error.TypeMismatch;
                    };

                    return .{ .kind = .{ .primitive = prim_type }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_write")) {
                    if (call.args.len != 2) {
                        std.debug.print("@ptr_write expects 2 arguments but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    _ = try self.inferExprType(call.args[0]);
                    _ = try self.inferExprType(call.args[1]);
                    // Returns unit type (empty tuple)
                    return .{ .kind = .{ .tuple = &[_]*parser.Type{} }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_offset")) {
                    if (call.args.len != 2) {
                        std.debug.print("@ptr_offset expects 2 arguments but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    _ = try self.inferExprType(call.args[0]);
                    const offset_type = try self.inferExprType(call.args[1]);
                    // Offset must be an integer
                    if (!self.isIntegerType(offset_type)) {
                        std.debug.print("@ptr_offset offset must be an integer type\n", .{});
                        return error.TypeMismatch;
                    }
                    return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@int_to_ptr")) {
                    if (call.args.len != 1) {
                        std.debug.print("@int_to_ptr expects 1 argument but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    const int_type = try self.inferExprType(call.args[0]);
                    // Argument must be an integer
                    if (!self.isIntegerType(int_type)) {
                        std.debug.print("@int_to_ptr argument must be an integer type\n", .{});
                        return error.TypeMismatch;
                    }
                    return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_to_int")) {
                    if (call.args.len != 1) {
                        std.debug.print("@ptr_to_int expects 1 argument but got {d}\n", .{call.args.len});
                        return error.ArgumentCountMismatch;
                    }
                    const ptr_type = try self.inferExprType(call.args[0]);
                    // Argument must be a pointer
                    if (ptr_type.kind != .primitive or ptr_type.kind.primitive != .ptr) {
                        std.debug.print("@ptr_to_int argument must be a pointer type\n", .{});
                        return error.TypeMismatch;
                    }
                    return .{ .kind = .{ .primitive = .u64 }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@join")) {
                    // @join(task, @type(T)) - wait for task and return result of type T
                    if (call.args.len != 2) {
                        std.debug.print("@join expects 2 arguments (task, @type(T))\n", .{});
                        return error.ArgumentCountMismatch;
                    }
                    // First arg is task handle (consumes the linear Task)
                    const task_type = try self.inferExprType(call.args[0]);
                    _ = task_type; // Task is consumed here

                    // Second arg is @type(T) - determines return type
                    const type_arg = call.args[1];
                    if (type_arg.* != .intrinsic_call or !std.mem.eql(u8, type_arg.intrinsic_call.name, "@type")) {
                        std.debug.print("@join second argument must be @type(T)\n", .{});
                        return error.TypeMismatch;
                    }

                    // Extract the type from @type(T)
                    const type_name_expr = type_arg.intrinsic_call.args[0];
                    const type_name = switch (type_name_expr.*) {
                        .variable => |v| v,
                        .constructor_call => |c| c.name,
                        else => {
                            std.debug.print("Invalid type in @type\n", .{});
                            return error.TypeMismatch;
                        },
                    };

                    // Map type names to primitives
                    const type_map = std.StaticStringMap(parser.PrimitiveType).initComptime(.{
                        .{ "i8", .i8 },
                        .{ "i16", .i16 },
                        .{ "i32", .i32 },
                        .{ "i64", .i64 },
                        .{ "u8", .u8 },
                        .{ "u16", .u16 },
                        .{ "u32", .u32 },
                        .{ "u64", .u64 },
                        .{ "bool", .bool },
                        .{ "ptr", .ptr },
                    });

                    if (type_map.get(type_name)) |prim| {
                        return .{ .kind = .{ .primitive = prim }, .usage = .once };
                    } else {
                        std.debug.print("Unknown type in @join: {s}\n", .{type_name});
                        return error.UndefinedType;
                    }
                } else if (std.mem.eql(u8, call.name, "@yield")) {
                    // @yield() - suspend coroutine, return control to scheduler
                    if (call.args.len != 0) {
                        std.debug.print("@yield expects no arguments\n", .{});
                        return error.ArgumentCountMismatch;
                    }
                    if (self.async_scope_depth == 0) {
                        std.debug.print("@yield can only be called within async scope\n", .{});
                        return error.TypeMismatch;
                    }
                    // Returns unit type
                    return .{ .kind = .{ .tuple = &[_]*parser.Type{} }, .usage = .once };
                } else {
                    std.debug.print("Unknown intrinsic: {s}\n", .{call.name});
                    return error.UndefinedFunction;
                }
            },
            .dependent_type_ref => {
                // A dependent type reference should only appear as the object of a method call
                // e.g., Vec[ptr, 8].new() - the reference itself doesn't have a type
                std.debug.print("Dependent type reference cannot be used as a value\n", .{});
                return error.TypeMismatch;
            },
            .async_expr => |async_expr| {
                // async { body } returns the type of body
                // Increment scope depth to allow spawn inside
                self.async_scope_depth += 1;
                defer self.async_scope_depth -= 1;
                return try self.inferExprType(async_expr.body);
            },
            .spawn_expr => |spawn_expr| {
                // spawn { body } returns Task[T] where T is body's type
                // spawn must be inside an async block (structured concurrency)
                if (self.async_scope_depth == 0) {
                    std.debug.print("spawn must be inside async block\n", .{});
                    return error.SpawnOutsideAsyncScope;
                }

                const body_type = try self.inferExprType(spawn_expr.body);

                // Create Task[T] dependent type
                // Allocate type param for the body type and track for cleanup
                const body_type_ptr = try self.allocator.create(Type);
                body_type_ptr.* = body_type;
                try self.substituted_types.append(self.allocator, body_type_ptr);

                var type_params = try self.allocator.alloc(parser.TypeParam, 1);
                type_params[0] = .{ .concrete = body_type_ptr };
                try self.allocated_type_params.append(self.allocator, type_params);

                return .{
                    .kind = .{ .dependent = .{
                        .base = "Task",
                        .type_params = type_params,
                        .value_params = &[_]Expr{},
                    } },
                    .usage = .once, // Task handles are linear - must be joined exactly once
                };
            },
            .select_expr => |select_expr| {
                // Select returns unified type of all arms (similar to match)
                if (select_expr.arms.len == 0) {
                    if (select_expr.default_arm) |default| {
                        return try self.inferExprType(default);
                    }
                    std.debug.print("Select expression must have at least one arm or default\n", .{});
                    return error.TypeMismatch;
                }
                return try self.inferExprType(select_expr.arms[0].body);
            },
        }
    }

    fn canImplicitlyConvert(self: *TypeChecker, from: Type, to: Type) bool {
        // Resolve dependent types first
        const resolved_from = self.resolveType(from);
        const resolved_to = self.resolveType(to);

        // Exact match is always allowed
        if (resolved_from.eql(resolved_to)) return true;

        // bool can convert to any integer type (safe: 0 or 1)
        if (resolved_from.kind == .primitive and resolved_from.kind.primitive == .bool and resolved_to.isInteger()) return true;

        // Allow widening conversions between integer types (no data loss)
        // Signed can widen to larger signed, unsigned can widen to larger unsigned
        // We don't allow mixing signed/unsigned to prevent unexpected behavior
        if (resolved_from.isInteger() and resolved_to.isInteger()) {
            // Same signedness and target is wider (not equal - handled by exact match above)
            if (resolved_from.isSigned() == resolved_to.isSigned() and resolved_to.bitWidth() > resolved_from.bitWidth()) {
                return true;
            }
        }

        // No other implicit conversions allowed
        return false;
    }

    /// Resolve dependent and named types to their underlying type definition
    /// For generic types like Box[i32], substitutes type parameters
    fn resolveType(self: *TypeChecker, typ: Type) Type {
        switch (typ.kind) {
            .dependent => |dep| {
                // Look up the type definition
                if (self.type_defs.get(dep.base)) |typedef_info| {
                    // Build substitution map from type parameters
                    var substitutions = std.StringHashMap(Type).init(self.allocator);
                    defer substitutions.deinit();

                    for (typedef_info.params, 0..) |param, i| {
                        if (param.kind == .type_param) {
                            if (i < dep.type_params.len) {
                                switch (dep.type_params[i]) {
                                    .concrete => |t| {
                                        substitutions.put(param.name, t.*) catch continue;
                                    },
                                    .variable => {}, // Type variable, don't substitute
                                }
                            }
                        }
                    }

                    // Apply substitutions to the type definition
                    if (substitutions.count() > 0) {
                        return self.substituteTypeParams(typedef_info.type_value, substitutions);
                    }
                    return typedef_info.type_value;
                }
                return typ;
            },
            .named => |name| {
                // Look up the type definition and return its underlying type
                if (self.type_defs.get(name)) |typedef_info| {
                    return typedef_info.type_value;
                }
                return typ;
            },
            else => return typ,
        }
    }

    /// Substitute type parameters in a type with concrete types
    /// Used for generic struct instantiation: Box[T] with T=i32 -> Box[i32]
    fn substituteTypeParams(self: *TypeChecker, typ: Type, substitutions: std.StringHashMap(Type)) Type {
        switch (typ.kind) {
            .named => |name| {
                // If this name is a type parameter, substitute it
                if (substitutions.get(name)) |replacement| {
                    return replacement;
                }
                return typ;
            },
            .struct_type => |fields| {
                // Recursively substitute in each field type
                const new_fields = self.allocator.alloc(parser.StructField, fields.len) catch return typ;
                self.substituted_fields.append(self.allocator, new_fields) catch {};
                for (fields, 0..) |field, i| {
                    const new_field_type = self.allocator.create(Type) catch return typ;
                    self.substituted_types.append(self.allocator, new_field_type) catch {};
                    new_field_type.* = self.substituteTypeParams(field.field_type.*, substitutions);
                    new_fields[i] = .{
                        .name = field.name,
                        .field_type = new_field_type,
                    };
                }
                return Type{ .kind = .{ .struct_type = new_fields }, .usage = typ.usage };
            },
            .tuple => |elems| {
                // Recursively substitute in each element type
                const new_elems = self.allocator.alloc(*Type, elems.len) catch return typ;
                for (elems, 0..) |elem, i| {
                    const new_type = self.allocator.create(Type) catch return typ;
                    self.substituted_types.append(self.allocator, new_type) catch {};
                    new_type.* = self.substituteTypeParams(elem.*, substitutions);
                    new_elems[i] = new_type;
                }
                return Type{ .kind = .{ .tuple = new_elems }, .usage = typ.usage };
            },
            .sum_type => |variants| {
                // Recursively substitute in variant payload types
                const new_variants = self.allocator.alloc(parser.SumTypeVariant, variants.len) catch return typ;
                for (variants, 0..) |variant, i| {
                    const new_payloads = self.allocator.alloc(*Type, variant.payload_types.len) catch return typ;
                    for (variant.payload_types, 0..) |payload, j| {
                        const new_type = self.allocator.create(Type) catch return typ;
                        self.substituted_types.append(self.allocator, new_type) catch {};
                        new_type.* = self.substituteTypeParams(payload.*, substitutions);
                        new_payloads[j] = new_type;
                    }
                    new_variants[i] = .{
                        .name = variant.name,
                        .payload_types = new_payloads,
                    };
                }
                return Type{ .kind = .{ .sum_type = new_variants }, .usage = typ.usage };
            },
            .dependent => |dep| {
                // Recursively substitute in type_params
                const new_type_params = self.allocator.alloc(parser.TypeParam, dep.type_params.len) catch return typ;
                for (dep.type_params, 0..) |param, i| {
                    switch (param) {
                        .concrete => |t| {
                            const new_t = self.allocator.create(Type) catch return typ;
                            self.substituted_types.append(self.allocator, new_t) catch {};
                            new_t.* = self.substituteTypeParams(t.*, substitutions);
                            new_type_params[i] = .{ .concrete = new_t };
                        },
                        .variable => |v| {
                            // Check if variable should be substituted
                            if (substitutions.get(v)) |replacement| {
                                const new_t = self.allocator.create(Type) catch return typ;
                                self.substituted_types.append(self.allocator, new_t) catch {};
                                new_t.* = replacement;
                                new_type_params[i] = .{ .concrete = new_t };
                            } else {
                                new_type_params[i] = param;
                            }
                        },
                    }
                }
                return Type{
                    .kind = .{ .dependent = .{
                        .base = dep.base,
                        .type_params = new_type_params,
                        .value_params = dep.value_params,
                    } },
                    .usage = typ.usage,
                };
            },
            // Primitives don't need substitution
            .primitive => return typ,
        }
    }

    fn typesMatch(self: *TypeChecker, a: Type, b: Type) bool {
        const resolved_a = self.resolveType(a);
        const resolved_b = self.resolveType(b);
        return resolved_a.eql(resolved_b);
    }

    fn isIntegerType(self: *TypeChecker, typ: Type) bool {
        const resolved = self.resolveType(typ);
        return resolved.isInteger();
    }

    const type_name_map = std.StaticStringMap(parser.PrimitiveType).initComptime(.{
        .{ "bool", .bool },
        .{ "i8", .i8 },
        .{ "i16", .i16 },
        .{ "i32", .i32 },
        .{ "i64", .i64 },
        .{ "ptr", .ptr },
        .{ "str", .str },
        .{ "Type", .type },
        .{ "u8", .u8 },
        .{ "u16", .u16 },
        .{ "u32", .u32 },
        .{ "u64", .u64 },
    });

    /// Evaluate @type(TypeName) expression at compile time to extract the Type
    pub fn evaluateTypeExpr(self: *TypeChecker, expr: *const Expr) ?Type {
        switch (expr.*) {
            .intrinsic_call => |call| {
                if (std.mem.eql(u8, call.name, "@type")) {
                    // @type(TypeName) -> extract TypeName
                    const type_name_expr = call.args[0];
                    const type_name = switch (type_name_expr.*) {
                        .variable => |v| v,
                        .constructor_call => |c| c.name,
                        else => return null,
                    };

                    // Check primitive types
                    if (type_name_map.get(type_name)) |prim| {
                        return Type{ .kind = .{ .primitive = prim }, .usage = .once };
                    }

                    // Check if it's a named type
                    if (self.type_defs.get(type_name)) |typedef_info| {
                        return typedef_info.type_value;
                    }

                    return null;
                }
                return null;
            },
            else => return null,
        }
    }
};

test "typechecker: valid function" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn main() i32 { return 42 }";
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

    const source = "fn main() bool { return 5 > 3 }";
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
        "fn f() bool { return 5 == 3 }",
        "fn f() bool { return 5 != 3 }",
        "fn f() bool { return 5 < 3 }",
        "fn f() bool { return 5 > 3 }",
        "fn f() bool { return 5 <= 3 }",
        "fn f() bool { return 5 >= 3 }",
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
        "fn f() i32 { return 5 && 3 }",
        "fn f() i32 { return 5 || 3 }",
        "fn f() i32 { return !5 }",
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
        "fn f() bool { return (5 > 3) && (2 < 4) }",
        "fn f() bool { return (5 > 3) || (2 < 4) }",
        "fn f() bool { return true && false }",
        "fn f() bool { return true || false }",
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
        "fn f() i32 { return 5 > 3 }",
        "fn f() i32 { return 5 && 3 }",
        "fn f() i32 { return !5 }",
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

