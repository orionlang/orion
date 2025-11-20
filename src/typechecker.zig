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
};

const TypeCheckError = error{
    TypeMismatch,
    UndefinedVariable,
    UndefinedFunction,
    ArgumentCountMismatch,
    EmptyFunctionBody,
    OutOfMemory,
    AssignmentToImmutable,
    LinearityViolation,
};

const VariableInfo = struct {
    var_type: Type,
    mutable: bool,
    use_count: u32,
};

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(VariableInfo),
    functions: std.StringHashMap(FunctionSignature),
    type_defs: std.StringHashMap(Type),
    instance_methods: std.StringHashMap(Type),
    allocated_tuple_types: std.ArrayList(Type),

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(VariableInfo).init(allocator),
            .functions = std.StringHashMap(FunctionSignature).init(allocator),
            .type_defs = std.StringHashMap(Type).init(allocator),
            .instance_methods = std.StringHashMap(Type).init(allocator),
            .allocated_tuple_types = .empty,
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

        // Free instance method keys
        var method_iter = self.instance_methods.keyIterator();
        while (method_iter.next()) |key| {
            self.allocator.free(key.*);
        }

        self.variables.deinit();
        self.functions.deinit();
        self.type_defs.deinit();
        self.instance_methods.deinit();
    }

    pub fn check(self: *TypeChecker, ast: *const AST) !void {
        // First pass: collect type definitions
        for (ast.type_defs.items) |typedef| {
            try self.type_defs.put(typedef.name, typedef.type_value);
        }

        // Second pass: collect instance methods
        for (ast.instances.items) |instance| {
            const type_name = switch (instance.type_name.kind) {
                .named => |name| name,
                .primitive => |prim| @tagName(prim),
                else => "unknown",
            };

            for (instance.methods) |method| {
                const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{type_name, method.name});
                try self.instance_methods.put(mangled_name, method.return_type);
            }
        }

        // Third pass: collect function signatures
        for (ast.functions.items) |func| {
            const param_types = try self.allocator.alloc(Type, func.params.len);
            for (func.params, 0..) |param, i| {
                param_types[i] = param.param_type;
            }
            try self.functions.put(func.name, .{
                .params = param_types,
                .return_type = func.return_type,
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
            // Propagate expected types to return statement expressions before checking
            if (stmt == .return_stmt) {
                try self.checkExprWithExpectedType(stmt.return_stmt, func.return_type);
            }

            try self.checkStatement(stmt, func.return_type);
        }

        // Validate that at least one return statement exists and has correct type
        // Type validation happens in checkStatement for .return_stmt case

        // Check linearity constraints
        try self.checkLinearity();
    }

    fn checkLinearity(self: *TypeChecker) !void {
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
                // For literals, update their inferred type to match context
                if (binding.type_annotation) |expected_type| {
                    try self.checkExprWithExpectedType(binding.value, expected_type);
                }
                // Verify the final type matches (catches type errors for non-literal expressions)
                const value_type = try self.inferExprType(binding.value);
                const var_type = binding.type_annotation orelse value_type;
                if (binding.type_annotation) |expected_type| {
                    if (!self.canImplicitlyConvert(value_type, expected_type)) {
                        std.debug.print("Type mismatch in let binding\n", .{});
                        return error.TypeMismatch;
                    }
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
                // Propagate expected type to RHS expression (for literal inference)
                try self.checkExprWithExpectedType(assign.value, var_info.var_type);

                const value_type = try self.inferExprType(assign.value);
                if (!self.canImplicitlyConvert(value_type, var_info.var_type)) {
                    printTypeMismatch("assignment to", assign.name, var_info.var_type, value_type);
                    return error.TypeMismatch;
                }
            },
            .while_stmt => |while_stmt| {
                const condition_type = try self.inferExprType(while_stmt.condition);
                if (condition_type.kind != .primitive or condition_type.kind.primitive != .bool) {
                    std.debug.print("While condition must be bool, got {any}\n", .{condition_type});
                    return error.TypeMismatch;
                }
                _ = try self.inferExprType(while_stmt.body);
            },
            .return_stmt => |ret_expr| {
                const expr_type = try self.inferExprType(ret_expr);
                if (func_return_type) |expected_type| {
                    if (!self.canImplicitlyConvert(expr_type, expected_type)) {
                        std.debug.print("Type mismatch in return statement: expected {s}, got {s}\n", .{
                            @tagName(expected_type.kind),
                            @tagName(expr_type.kind),
                        });
                        return error.TypeMismatch;
                    }
                }
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

    fn checkExprWithExpectedType(self: *TypeChecker, expr: *Expr, expected_type: Type) !void {
        // For integer literals, update the inferred type and validate range
        switch (expr.*) {
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
                const min = expected_type.minValue();
                const max = expected_type.maxValue();

                if (value < min or value > max) {
                    // Provide clear error messages for common cases
                    if (!expected_type.isSigned() and value < 0) {
                        std.debug.print("Integer literal {d} is negative but type {s} is unsigned\n", .{
                            value,
                            @tagName(expected_type.kind),
                        });
                    } else {
                        std.debug.print("Integer literal {d} does not fit in type {s} (range: {d} to {d})\n", .{
                            value,
                            @tagName(expected_type.kind),
                            min,
                            max,
                        });
                    }
                    return error.TypeMismatch;
                }

                // Update the inferred type
                lit.inferred_type = expected_type;
            },
            .binary_op => |*binop| {
                // For arithmetic binary operations, propagate expected type to both operands
                // This allows `let x: U32 = 10 + 5` to type both literals as U32
                if (!binop.op.returns_bool()) {
                    try self.checkExprWithExpectedType(binop.left, expected_type);
                    try self.checkExprWithExpectedType(binop.right, expected_type);
                }
                // For comparison ops, we don't propagate type (result is bool anyway)
            },
            .unary_op => |*unop| {
                // TODO: Propagate expected type to operand for unary minus
                // Example: `let x: I64 = -42` should type the literal 42 as I64 before negating
                _ = unop;
            },
            .if_expr => |*if_expr| {
                // TODO: Propagate expected type to both branches
                // Example: `let x: I64 = if cond { 42 } else { 100 }` should type both literals as I64
                _ = if_expr;
            },
            .block_expr => |*block| {
                // TODO: Propagate expected type to result expression
                // Example: `let x: I64 = { stmt; stmt; 42 }` should type 42 as I64
                _ = block;
            },
            .bool_literal, .variable, .function_call, .struct_literal, .field_access, .constructor_call, .match_expr, .method_call => {
                // These expressions have fixed types that can't be influenced by context:
                // - bool_literal: always Bool
                // - variable: type determined at declaration
                // - function_call: return type is fixed by function signature
                // - struct_literal: type determined by struct definition
                // - field_access: type determined by field type
                // - constructor_call: type determined by sum type definition
                // - match_expr: type is the unified type of all arms
            },
        }
    }

    fn inferExprType(self: *TypeChecker, expr: *const Expr) !Type {
        switch (expr.*) {
            .integer_literal => |lit| return lit.inferred_type,
            .bool_literal => return .{ .kind = .{ .primitive = .bool  }, .usage = .once },
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

                // Check argument count
                if (call.args.len != sig.params.len) {
                    std.debug.print("Function {s} expects {d} arguments, got {d}\n", .{
                        call.name,
                        sig.params.len,
                        call.args.len,
                    });
                    return error.ArgumentCountMismatch;
                }

                // Check argument types
                for (call.args, 0..) |arg, i| {
                    const arg_type = try self.inferExprType(arg);
                    const expected_type = sig.params[i];

                    if (!self.canImplicitlyConvert(arg_type, expected_type)) {
                        std.debug.print("Function {s} argument {d}: expected {s}, got {s}\n", .{
                            call.name,
                            i,
                            @tagName(expected_type.kind),
                            @tagName(arg_type.kind),
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
            .struct_literal => |lit| {
                const typedef = self.type_defs.get(lit.type_name) orelse {
                    std.debug.print("Undefined type: {s}\n", .{lit.type_name});
                    return error.UndefinedVariable;
                };

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
                            const field_value_type = try self.inferExprType(field.value);
                            if (!self.typesMatch(field_value_type, type_field.field_type.*)) {
                                std.debug.print("Struct field {s} has wrong type\n", .{field.name});
                                return error.TypeMismatch;
                            }
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        std.debug.print("Struct {s} has no field named {s}\n", .{ lit.type_name, field.name });
                        return error.TypeMismatch;
                    }
                }

                return .{ .kind = .{ .named = lit.type_name  }, .usage = .once };
            },
            .field_access => |access| {
                const object_type = try self.inferExprType(access.object);

                if (object_type.kind != .named) {
                    std.debug.print("Cannot access field of non-struct type\n", .{});
                    return error.TypeMismatch;
                }

                const typedef = self.type_defs.get(object_type.kind.named) orelse {
                    std.debug.print("Undefined type: {s}\n", .{object_type.kind.named});
                    return error.UndefinedVariable;
                };

                if (typedef.kind != .struct_type) {
                    std.debug.print("Type {s} is not a struct\n", .{object_type.kind.named});
                    return error.TypeMismatch;
                }

                for (typedef.kind.struct_type) |field| {
                    if (std.mem.eql(u8, field.name, access.field_name)) {
                        return field.field_type.*;
                    }
                }

                std.debug.print("Struct {s} has no field named {s}\n", .{ object_type.kind.named, access.field_name });
                return error.TypeMismatch;
            },
            .constructor_call => |call| {
                // Find which type this constructor belongs to
                var found_type: ?[]const u8 = null;
                var found_variant: ?parser.SumTypeVariant = null;

                var iter = self.type_defs.iterator();
                while (iter.next()) |entry| {
                    if (entry.value_ptr.*.kind == .sum_type) {
                        for (entry.value_ptr.*.kind.sum_type) |variant| {
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

                const typedef = self.type_defs.get(scrutinee_type.kind.named) orelse {
                    std.debug.print("Undefined type: {s}\n", .{scrutinee_type.kind.named});
                    return error.UndefinedVariable;
                };

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
                // Infer the type of the object
                const object_type = try self.inferExprType(method_call.object);

                // Extract type name
                const type_name = switch (object_type.kind) {
                    .named => |name| name,
                    .primitive => |prim| @tagName(prim),
                    else => "unknown",
                };

                // Look up the method in instance_methods
                const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{type_name, method_call.method_name});
                defer self.allocator.free(mangled_name);

                if (self.instance_methods.get(mangled_name)) |return_type| {
                    return return_type;
                } else {
                    // Method not found, return object type as fallback
                    return object_type;
                }
            },
        }
    }

    fn canImplicitlyConvert(_: *TypeChecker, from: Type, to: Type) bool {

        // Exact match is always allowed
        if (from.eql(to)) return true;

        // Bool can convert to any integer type (safe: 0 or 1)
        if (from.kind == .primitive and from.kind.primitive == .bool and to.isInteger()) return true;

        // Allow widening conversions between integer types (no data loss)
        // Signed can widen to larger signed, unsigned can widen to larger unsigned
        // We don't allow mixing signed/unsigned to prevent unexpected behavior
        if (from.isInteger() and to.isInteger()) {
            // Same signedness and target is wider (not equal - handled by exact match above)
            if (from.isSigned() == to.isSigned() and to.bitWidth() > from.bitWidth()) {
                return true;
            }
        }

        // No other implicit conversions allowed
        return false;
    }

    fn typesMatch(_: *TypeChecker, a: Type, b: Type) bool {
        return a.eql(b);
    }

    fn isIntegerType(_: *TypeChecker, typ: Type) bool {
        return typ.isInteger();
    }
};

test "typechecker: valid function" {
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

    const source = "fn main() Bool { return 5 > 3 }";
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
        "fn f() Bool { return 5 == 3 }",
        "fn f() Bool { return 5 != 3 }",
        "fn f() Bool { return 5 < 3 }",
        "fn f() Bool { return 5 > 3 }",
        "fn f() Bool { return 5 <= 3 }",
        "fn f() Bool { return 5 >= 3 }",
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
        "fn f() I32 { return 5 && 3 }",
        "fn f() I32 { return 5 || 3 }",
        "fn f() I32 { return !5 }",
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
        "fn f() Bool { return (5 > 3) && (2 < 4) }",
        "fn f() Bool { return (5 > 3) || (2 < 4) }",
        "fn f() Bool { return true && false }",
        "fn f() Bool { return true || false }",
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
        "fn f() I32 { return 5 > 3 }",
        "fn f() I32 { return 5 && 3 }",
        "fn f() I32 { return !5 }",
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

