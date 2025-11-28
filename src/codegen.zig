const std = @import("std");
const parser = @import("parser.zig");
const AST = parser.AST;
const Type = parser.Type;
const Expr = parser.Expr;
const Stmt = parser.Stmt;
const Pattern = parser.Pattern;
const target_module = @import("target.zig");
const TargetTriple = target_module.TargetTriple;
const TargetInfo = target_module.TargetInfo;

pub const CodegenError = error{
    OutOfMemory,
    UndefinedVariable,
    UndefinedConstructor,
    UndefinedType,
    TupleNotImplemented,
    TupleDestructuringNotImplemented,
};

const VarInfo = struct {
    llvm_ptr: []const u8,
    var_type: Type,
};

const FunctionInfo = struct {
    return_type: Type,
    param_types: []const Type,
};

const TypeDefInfo = struct {
    params: []const parser.TypeParameter,
    type_value: Type,
};

pub const Codegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    next_temp: usize,
    next_string: usize,
    string_literals: std.ArrayList(u8), // Global string constants
    variables: std.StringHashMap(VarInfo),
    functions: std.StringHashMap(FunctionInfo),
    type_defs: std.StringHashMap(TypeDefInfo),
    current_block: ?[]const u8,
    block_terminated: bool,
    inferred_types: std.ArrayList(Type),
    instance_methods: std.StringHashMap(Type),
    target: TargetTriple,
    target_info: TargetInfo,
    current_function_return_type: ?Type,
    ast: ?*const AST,
    // Track allocations from substituteTypeParams for cleanup
    substituted_types: std.ArrayList(*Type),
    substituted_fields: std.ArrayList([]parser.StructField),
    // Concurrency: track coroutines
    needs_coro_intrinsics: bool,
    next_coro: usize,
    next_suspend: usize,
    deferred_coros: std.ArrayList(u8), // Buffer for outlined coroutine functions
    current_coro_handle: ?[]const u8, // Handle for current coroutine (used by @yield)
    // Runtime scheduler for Go-style concurrency
    needs_scheduler: bool,
    in_async_block: bool, // Track if we're generating code inside async block

    pub fn init(allocator: std.mem.Allocator, target: TargetTriple) Codegen {
        return .{
            .allocator = allocator,
            .output = .empty,
            .next_temp = 0,
            .next_string = 0,
            .string_literals = .empty,
            .variables = std.StringHashMap(VarInfo).init(allocator),
            .functions = std.StringHashMap(FunctionInfo).init(allocator),
            .type_defs = std.StringHashMap(TypeDefInfo).init(allocator),
            .current_block = null,
            .block_terminated = false,
            .inferred_types = .empty,
            .instance_methods = std.StringHashMap(Type).init(allocator),
            .target = target,
            .target_info = target.getTargetInfo(),
            .current_function_return_type = null,
            .ast = null,
            .substituted_types = .empty,
            .substituted_fields = .empty,
            .needs_coro_intrinsics = false,
            .next_coro = 0,
            .next_suspend = 0,
            .deferred_coros = .empty,
            .current_coro_handle = null,
            .needs_scheduler = false,
            .in_async_block = false,
        };
    }

    pub fn deinit(self: *Codegen) void {
        // Free all allocated variable names
        var iter = self.variables.valueIterator();
        while (iter.next()) |var_info| {
            self.allocator.free(var_info.llvm_ptr);
        }

        // Free string literals
        self.string_literals.deinit(self.allocator);

        // Free all inferred tuple types (shallow - nested tuples tracked separately)
        for (self.inferred_types.items) |*typ| {
            typ.deinitShallow(self.allocator);
        }
        self.inferred_types.deinit(self.allocator);

        if (self.current_block) |block| {
            self.allocator.free(block);
        }

        // Free instance_methods keys
        var it = self.instance_methods.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }

        // Free function param_types arrays
        var func_iter = self.functions.valueIterator();
        while (func_iter.next()) |func_info| {
            self.allocator.free(func_info.param_types);
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

        self.deferred_coros.deinit(self.allocator);
        self.output.deinit(self.allocator);
        self.variables.deinit();
        self.functions.deinit();
        self.type_defs.deinit();
        self.instance_methods.deinit();
    }

    fn allocTempName(self: *Codegen) ![]const u8 {
        // Use %.t prefix to avoid collision with user variable names like t1, t2
        const temp_name = try std.fmt.allocPrint(self.allocator, "%.t{d}", .{self.next_temp});
        self.next_temp += 1;
        return temp_name;
    }

    fn generateStringLiteral(self: *Codegen, lexeme: []const u8) ![]const u8 {
        // lexeme includes quotes, e.g. "hello world"
        // Strip quotes and process escape sequences
        const content = lexeme[1 .. lexeme.len - 1];

        // Process escape sequences and calculate final length
        var processed = std.ArrayList(u8).empty;
        defer processed.deinit(self.allocator);

        var i: usize = 0;
        while (i < content.len) {
            if (content[i] == '\\' and i + 1 < content.len) {
                const next = content[i + 1];
                switch (next) {
                    'n' => try processed.append(self.allocator, '\n'),
                    't' => try processed.append(self.allocator, '\t'),
                    'r' => try processed.append(self.allocator, '\r'),
                    '\\' => try processed.append(self.allocator, '\\'),
                    '"' => try processed.append(self.allocator, '"'),
                    '0' => try processed.append(self.allocator, 0),
                    else => {
                        // Unknown escape, keep as-is
                        try processed.append(self.allocator, '\\');
                        try processed.append(self.allocator, next);
                    },
                }
                i += 2;
            } else {
                try processed.append(self.allocator, content[i]);
                i += 1;
            }
        }

        // Add null terminator
        try processed.append(self.allocator, 0);

        // Generate global constant
        const str_id = self.next_string;
        self.next_string += 1;

        const global_name = try std.fmt.allocPrint(self.allocator, "@.str.{d}", .{str_id});
        defer self.allocator.free(global_name);

        // Write to string_literals section
        try self.string_literals.writer(self.allocator).print("{s} = private unnamed_addr constant [{d} x i8] c\"", .{ global_name, processed.items.len });

        // Write escaped bytes
        for (processed.items) |byte| {
            if (byte >= 32 and byte <= 126 and byte != '\\' and byte != '"') {
                try self.string_literals.writer(self.allocator).writeByte(byte);
            } else {
                try self.string_literals.writer(self.allocator).print("\\{X:0>2}", .{byte});
            }
        }

        try self.string_literals.writer(self.allocator).print("\"\n", .{});

        // Return a pointer to the global
        return try std.fmt.allocPrint(self.allocator, "getelementptr inbounds ([{d} x i8], ptr {s}, i32 0, i32 0)", .{ processed.items.len, global_name });
    }

    fn allocLabel(self: *Codegen, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ prefix, self.next_temp });
        self.next_temp += 1;
        return label;
    }

    fn setCurrentBlock(self: *Codegen, label: []const u8) !void {
        if (self.current_block) |old_block| self.allocator.free(old_block);
        self.current_block = try self.allocator.dupe(u8, label);
        self.block_terminated = false;
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
            const arg_val = try self.generateExpression(arg, null);
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
        // Store AST reference for instance lookups
        self.ast = ast;

        // First pass: collect type definitions (with type parameters for generics)
        for (ast.type_defs.items) |typedef| {
            try self.type_defs.put(typedef.name, .{
                .params = typedef.params,
                .type_value = typedef.type_value,
            });
        }

        // Second pass: collect function signatures (including extern)
        for (ast.extern_functions.items) |extern_func| {
            // Extract parameter types
            const param_types = try self.allocator.alloc(Type, extern_func.params.len);
            for (extern_func.params, 0..) |param, i| {
                param_types[i] = param.param_type;
            }
            try self.functions.put(extern_func.name, .{
                .return_type = extern_func.return_type,
                .param_types = param_types,
            });
        }
        for (ast.functions.items) |func| {
            // Extract parameter types
            const param_types = try self.allocator.alloc(Type, func.params.len);
            for (func.params, 0..) |param, i| {
                param_types[i] = param.param_type;
            }
            try self.functions.put(func.name, .{
                .return_type = func.return_type,
                .param_types = param_types,
            });
        }

        // Third pass: collect instance method signatures
        for (ast.instances.items) |instance| {
            const type_name = try self.mangledTypeName(instance.type_name);
            defer self.allocator.free(type_name);

            for (instance.methods) |method| {
                const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{ type_name, method.name });
                const saved_name = try self.allocator.dupe(u8, mangled_name);
                self.allocator.free(mangled_name);
                try self.instance_methods.put(saved_name, method.return_type);
            }
        }

        // LLVM IR header
        try self.output.appendSlice(self.allocator, "; Bootstrap Orion Compiler Output\n");

        const target_triple_str = try self.target.toLLVMTriple(self.allocator);
        defer self.allocator.free(target_triple_str);
        try self.output.writer(self.allocator).print("target triple = \"{s}\"\n\n", .{target_triple_str});

        // Generate extern function declarations
        for (ast.extern_functions.items) |extern_func| {
            try self.generateExternDeclaration(&extern_func);
        }
        if (ast.extern_functions.items.len > 0) {
            try self.output.appendSlice(self.allocator, "\n");
        }

        // Generate LLVM coroutine intrinsic declarations for concurrency
        // Always emit since these are only used if async/spawn appear
        try self.generateCoroIntrinsicDeclarations();

        // Generate each function
        for (ast.functions.items) |func| {
            try self.generateFunction(&func);
        }

        // Output string literals after functions but they'll be moved to top later
        // For now, just append them at the end
        if (self.string_literals.items.len > 0) {
            try self.output.appendSlice(self.allocator, "\n");
            try self.output.appendSlice(self.allocator, self.string_literals.items);
        }

        // Generate instance methods
        for (ast.instances.items) |instance| {
            try self.generateInstanceMethods(&instance);
        }

        // Emit scheduler runtime if needed
        if (self.needs_scheduler) {
            try self.generateSchedulerRuntime();
        }

        // Emit deferred coroutine functions (generated by spawn expressions)
        if (self.deferred_coros.items.len > 0) {
            try self.output.appendSlice(self.allocator, "\n; Coroutine functions for spawned tasks\n");
            try self.output.appendSlice(self.allocator, self.deferred_coros.items);
            // Emit coroutine attribute group
            try self.output.appendSlice(self.allocator, "attributes #0 = { presplitcoroutine }\n");
        }

        // Generate _start entry point that calls __libc_start_main
        // This replaces crt1.o so we can link with just lld + libc
        try self.generateStartFunction();

        return try self.allocator.dupe(u8, self.output.items);
    }

    fn generateInstanceMethods(self: *Codegen, instance: *const parser.InstanceDecl) !void {
        // Extract type name from instance.type_name
        const type_name = try self.mangledTypeName(instance.type_name);
        defer self.allocator.free(type_name);

        // Generate each method as a function with mangled name: TypeName__methodName
        for (instance.methods) |method| {
            // Clear variables from previous function
            var iter = self.variables.valueIterator();
            while (iter.next()) |var_info| {
                self.allocator.free(var_info.llvm_ptr);
            }
            self.variables.clearRetainingCapacity();

            // Set current function return type
            self.current_function_return_type = method.return_type;

            // Generate mangled function name
            const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{ type_name, method.name });
            defer self.allocator.free(mangled_name);

            // Function signature
            const return_type_str = try self.llvmTypeString(method.return_type);
            defer self.allocator.free(return_type_str);
            try self.output.writer(self.allocator).print("define {s} @{s}(", .{ return_type_str, mangled_name });

            // Generate parameter list
            for (method.params, 0..) |param, i| {
                if (i > 0) try self.output.appendSlice(self.allocator, ", ");
                const param_type_str = try self.llvmTypeString(param.param_type);
                defer self.allocator.free(param_type_str);
                try self.output.writer(self.allocator).print("{s} %{s}", .{ param_type_str, param.name });
            }

            try self.output.appendSlice(self.allocator, ") {\nentry:\n");

            // Allocate and store parameters
            for (method.params) |param| {
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
            for (method.body.items) |stmt| {
                try self.generateStatement(&stmt, method.return_type);
            }

            try self.output.appendSlice(self.allocator, "}\n\n");
        }
    }

    // Find a typeclass instance by class name and target type
    fn findInstance(self: *Codegen, class_name: []const u8, target_type: Type) ?*const parser.InstanceDecl {
        const ast = self.ast orelse return null;
        for (ast.instances.items) |*instance| {
            if (std.mem.eql(u8, instance.class_name, class_name) and instance.type_name.eql(target_type)) {
                return instance;
            }
        }
        return null;
    }

    /// Substitute type parameters in a type with concrete types
    /// Used for generic struct instantiation: Box[T] with T=i32 -> Box[i32]
    fn substituteTypeParams(self: *Codegen, typ: Type, substitutions: std.StringHashMap(Type)) Type {
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

    /// Build substitution map for a dependent type from its typedef
    fn buildSubstitutionMap(self: *Codegen, typedef_info: TypeDefInfo, dep: parser.DependentType) std.StringHashMap(Type) {
        var substitutions = std.StringHashMap(Type).init(self.allocator);
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
        return substitutions;
    }

    fn generateExternDeclaration(self: *Codegen, extern_func: *const parser.ExternFunctionDecl) !void {
        const return_type_str = try self.llvmTypeString(extern_func.return_type);
        defer self.allocator.free(return_type_str);
        try self.output.writer(self.allocator).print("declare {s} @{s}(", .{ return_type_str, extern_func.name });

        for (extern_func.params, 0..) |param, i| {
            if (i > 0) try self.output.appendSlice(self.allocator, ", ");
            const param_type_str = try self.llvmTypeString(param.param_type);
            defer self.allocator.free(param_type_str);
            try self.output.writer(self.allocator).print("{s}", .{param_type_str});
        }

        try self.output.appendSlice(self.allocator, ")\n");
    }

    fn generateCoroIntrinsicDeclarations(self: *Codegen) !void {
        // LLVM coroutine intrinsics for concurrency
        try self.output.appendSlice(self.allocator,
            \\; Coroutine intrinsics for structured concurrency
            \\declare token @llvm.coro.id(i32, ptr, ptr, ptr)
            \\declare i64 @llvm.coro.size.i64()
            \\declare ptr @llvm.coro.begin(token, ptr)
            \\declare token @llvm.coro.save(ptr)
            \\declare i8 @llvm.coro.suspend(token, i1)
            \\declare i1 @llvm.coro.end(ptr, i1, token)
            \\declare ptr @llvm.coro.free(token, ptr)
            \\declare void @llvm.coro.resume(ptr)
            \\declare void @llvm.coro.destroy(ptr)
            \\declare ptr @llvm.coro.promise(ptr, i32, i1)
            \\declare i1 @llvm.coro.done(ptr)
            \\; Memory allocation for coroutine frames
            \\declare ptr @malloc(i64)
            \\declare void @free(ptr)
            \\
        );
        try self.output.appendSlice(self.allocator, "\n");
    }

    fn generateSchedulerRuntime(self: *Codegen) !void {
        // Extern declarations for runtime scheduler (linked from liborion_runtime.a)
        // The runtime provides a Go-style work-stealing scheduler with M:N threading
        try self.output.appendSlice(self.allocator,
            \\; Runtime scheduler declarations (linked from liborion_runtime.a)
            \\declare void @orion_scheduler_init()
            \\declare void @orion_scheduler_spawn(ptr)
            \\declare void @orion_scheduler_yield()
            \\declare void @orion_scheduler_run()
            \\declare void @orion_scheduler_run_until(ptr)
            \\declare void @orion_scheduler_shutdown()
            \\
            \\; Wrapper functions for LLVM coroutine intrinsics (called by runtime)
            \\define i1 @orion_coro_done(ptr %hdl) {
            \\  %done = call i1 @llvm.coro.done(ptr %hdl)
            \\  ret i1 %done
            \\}
            \\
            \\define void @orion_coro_resume(ptr %hdl) {
            \\  call void @llvm.coro.resume(ptr %hdl)
            \\  ret void
            \\}
            \\
        );
        try self.output.appendSlice(self.allocator, "\n");
    }

    fn generateStartFunction(self: *Codegen) !void {
        // Generate _start entry point that calls __libc_start_main
        // This replaces crt1.o so we can link directly with lld + libc
        //
        // On x86_64 Linux, when _start is called:
        // - %rsp points to argc
        // - %rsp+8 is argv[0], %rsp+16 is argv[1], etc.
        //
        // __libc_start_main signature:
        // int __libc_start_main(
        //     int (*main)(int, char**, char**),
        //     int argc,
        //     char **argv,
        //     void (*init)(void),
        //     void (*fini)(void),
        //     void (*rtld_fini)(void),
        //     void *stack_end
        // );
        try self.output.appendSlice(self.allocator,
            \\; Entry point - replaces crt1.o
            \\declare i32 @__libc_start_main(ptr, i32, ptr, ptr, ptr, ptr, ptr)
            \\
            \\define void @_start() {
            \\entry:
            \\  ; Get argc from stack (first thing on stack at program start)
            \\  %argc_ptr = call ptr asm "mov %rsp, $0", "=r"()
            \\  %argc = load i32, ptr %argc_ptr
            \\  ; argv is at rsp + 8
            \\  %argv = getelementptr i8, ptr %argc_ptr, i64 8
            \\  ; Call __libc_start_main(main, argc, argv, 0, 0, 0, 0)
            \\  %ret = call i32 @__libc_start_main(ptr @main, i32 %argc, ptr %argv, ptr null, ptr null, ptr null, ptr null)
            \\  ; __libc_start_main never returns, but just in case
            \\  unreachable
            \\}
            \\
        );
    }

    fn generateCoroutineFunction(self: *Codegen, coro_name: []const u8, body: *const Expr, result_type_str: []const u8) CodegenError!void {
        // Generate a coroutine function for a spawn expression body
        // The coroutine follows LLVM's stackless coroutine model:
        // 1. Create coroutine ID
        // 2. Allocate frame with malloc
        // 3. Begin coroutine
        // 4. Execute body and store result in promise
        // 5. Final suspend
        // 6. Cleanup on destroy

        const w = self.deferred_coros.writer(self.allocator);

        // Function header with presplitcoroutine attribute (#0)
        try w.print("define ptr @{s}() #0 {{\n", .{coro_name});
        try w.print("entry:\n", .{});

        // Create coroutine ID
        try w.print("  %coro.id = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null)\n", .{});

        // Get coroutine frame size
        try w.print("  %coro.size = call i64 @llvm.coro.size.i64()\n", .{});

        // Allocate frame
        try w.print("  %coro.alloc = call ptr @malloc(i64 %coro.size)\n", .{});

        // Begin coroutine - returns handle
        try w.print("  %coro.hdl = call ptr @llvm.coro.begin(token %coro.id, ptr %coro.alloc)\n", .{});

        // Initial suspend - coroutine starts suspended, body runs after first resume
        // This allows the caller to get the handle before any work is done
        // Switch cases:
        //   default/-1 (ramp path): return handle immediately (coroutine suspended)
        //   0 (resume): continue to body
        //   1 (destroy): cleanup
        try w.print("  %coro.save.init = call token @llvm.coro.save(ptr %coro.hdl)\n", .{});
        try w.print("  %coro.init = call i8 @llvm.coro.suspend(token %coro.save.init, i1 false)\n", .{});
        try w.print("  switch i8 %coro.init, label %coro.ret [\n", .{});
        try w.print("    i8 0, label %coro.body\n", .{});
        try w.print("    i8 1, label %coro.final.cleanup\n", .{});
        try w.print("  ]\n", .{});
        try w.print("coro.body:\n", .{});

        // Save current output and generate body into a temp buffer
        const saved_output = self.output;
        self.output = .empty;

        // Set coroutine handle for @yield to use
        const saved_coro_handle = self.current_coro_handle;
        self.current_coro_handle = "%coro.hdl";

        // Generate the body expression
        const body_val = try self.generateExpression(body, null);
        defer self.allocator.free(body_val);

        // Restore coroutine handle
        self.current_coro_handle = saved_coro_handle;

        // Get the generated body code
        const body_code = try self.allocator.dupe(u8, self.output.items);
        defer self.allocator.free(body_code);

        // Restore output
        self.output.deinit(self.allocator);
        self.output = saved_output;

        // Write body code to coroutine
        try w.print("{s}", .{body_code});

        // Store result in promise area (for retrieval by join)
        try w.print("  %coro.promise = call ptr @llvm.coro.promise(ptr %coro.hdl, i32 8, i1 false)\n", .{});
        try w.print("  store {s} {s}, ptr %coro.promise\n", .{ result_type_str, body_val });

        // Final suspend - coroutine is done
        try w.print("  %coro.final = call i8 @llvm.coro.suspend(token none, i1 true)\n", .{});
        try w.print("  switch i8 %coro.final, label %coro.ret [\n", .{});
        try w.print("    i8 0, label %coro.final.resume\n", .{});
        try w.print("    i8 1, label %coro.final.cleanup\n", .{});
        try w.print("  ]\n", .{});

        // Resume after final suspend shouldn't happen
        try w.print("coro.final.resume:\n", .{});
        try w.print("  unreachable\n", .{});

        // Cleanup - free the coroutine frame
        try w.print("coro.final.cleanup:\n", .{});
        try w.print("  %coro.mem = call ptr @llvm.coro.free(token %coro.id, ptr %coro.hdl)\n", .{});
        try w.print("  call void @free(ptr %coro.mem)\n", .{});
        try w.print("  br label %coro.ret\n", .{});

        // Return the handle
        try w.print("coro.ret:\n", .{});
        try w.print("  %coro.done = call i1 @llvm.coro.end(ptr %coro.hdl, i1 false, token none)\n", .{});
        try w.print("  ret ptr %coro.hdl\n", .{});

        try w.print("}}\n\n", .{});
    }

    fn generateFunction(self: *Codegen, func: *const parser.FunctionDecl) !void {
        // Clear variables from previous function and free allocated names
        var iter = self.variables.valueIterator();
        while (iter.next()) |var_info| {
            self.allocator.free(var_info.llvm_ptr);
        }
        self.variables.clearRetainingCapacity();

        // Set current function return type
        self.current_function_return_type = func.return_type;

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
                // If type annotation provided, use it as expected type for bidirectional checking
                const expected_type = binding.type_annotation;

                // Generate value expression with expected type
                const value = try self.generateExpression(binding.value, expected_type);
                defer self.allocator.free(value);

                // Infer actual type for variable storage
                const value_expr_type = self.inferExprType(binding.value);
                const var_type = expected_type orelse value_expr_type;

                // Convert value type if necessary (fallback for expressions that don't use expected)
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
                const value = try self.generateExpression(assign.value, null);
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

                const body_val = try self.generateExpression(while_stmt.body, null);
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
            .if_stmt => |if_stmt| {
                const then_label = try self.allocLabel("if_then");
                const else_label = try self.allocLabel("if_else");
                const end_label = try self.allocLabel("if_end");

                // Evaluate condition
                const cond_bool = try self.generateConditionAsBool(if_stmt.condition);
                defer self.allocator.free(cond_bool);

                // Branch based on condition
                if (if_stmt.else_stmts) |_| {
                    try self.output.writer(self.allocator).print("  br i1 {s}, label %{s}, label %{s}\n", .{ cond_bool, then_label, else_label });
                } else {
                    try self.output.writer(self.allocator).print("  br i1 {s}, label %{s}, label %{s}\n", .{ cond_bool, then_label, end_label });
                }

                // Then block
                try self.output.writer(self.allocator).print("{s}:\n", .{then_label});
                try self.setCurrentBlock(then_label);
                for (if_stmt.then_stmts) |*then_stmt| {
                    try self.generateStatement(then_stmt, return_type);
                }
                try self.output.writer(self.allocator).print("  br label %{s}\n", .{end_label});

                // Else block (if present)
                if (if_stmt.else_stmts) |else_stmts| {
                    try self.output.writer(self.allocator).print("{s}:\n", .{else_label});
                    try self.setCurrentBlock(else_label);
                    for (else_stmts) |*else_stmt| {
                        try self.generateStatement(else_stmt, return_type);
                    }
                    try self.output.writer(self.allocator).print("  br label %{s}\n", .{end_label});
                }

                // End block
                try self.output.writer(self.allocator).print("{s}:\n", .{end_label});
                try self.setCurrentBlock(end_label);

                // Clean up labels
                self.allocator.free(then_label);
                self.allocator.free(else_label);
                self.allocator.free(end_label);
            },
            .return_stmt => |expr| {
                // Pass return type as expected for bidirectional checking
                const value = try self.generateExpression(expr, return_type);
                defer self.allocator.free(value);
                const expr_type = self.inferExprType(expr);
                const return_type_str = try self.llvmTypeString(return_type);
                defer self.allocator.free(return_type_str);

                // Convert type if needed (fallback for expressions that don't use expected)
                const converted = try self.convertIfNeeded(value, expr_type, return_type);
                defer self.allocator.free(converted);
                try self.output.writer(self.allocator).print("  ret {s} {s}\n", .{ return_type_str, converted });
                self.block_terminated = true;
            },
            .expr => |expr| {
                const value = try self.generateExpression(expr, null);
                self.allocator.free(value);
            },
        }
    }

    fn bindPattern(self: *Codegen, pattern: Pattern, value_type: Type, value_llvm: []const u8, mutable: bool) !void {
        switch (pattern) {
            .identifier => |name| {
                // Skip binding for underscore pattern - value is discarded
                if (std.mem.eql(u8, name, "_")) {
                    return;
                }

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
        // Convert primitive types
        if (from_type.kind == .primitive and to_type.kind == .primitive) {
            return try self.convertType(value, from_type, to_type);
        }
        // Convert tuples
        if (from_type.kind == .tuple and to_type.kind == .tuple) {
            const from_elems = from_type.kind.tuple;
            const to_elems = to_type.kind.tuple;
            if (from_elems.len != to_elems.len) {
                return try self.allocator.dupe(u8, value);
            }

            // Extract, convert, and rebuild tuple
            const from_type_str = try self.llvmTypeString(from_type);
            defer self.allocator.free(from_type_str);
            const to_type_str = try self.llvmTypeString(to_type);
            defer self.allocator.free(to_type_str);

            var current_val: []const u8 = try std.fmt.allocPrint(self.allocator, "undef", .{});

            for (0..from_elems.len) |i| {
                // Extract element from source tuple
                const extract_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = extractvalue {s} {s}, {d}\n", .{
                    extract_temp,
                    from_type_str,
                    value,
                    i,
                });
                defer self.allocator.free(extract_temp);

                // Convert element type if needed
                const elem_converted = try self.convertIfNeeded(extract_temp, from_elems[i].*, to_elems[i].*);
                defer self.allocator.free(elem_converted);

                const elem_type_str = try self.llvmTypeString(to_elems[i].*);
                defer self.allocator.free(elem_type_str);

                // Insert into new tuple
                const insert_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = insertvalue {s} {s}, {s} {s}, {d}\n", .{
                    insert_temp,
                    to_type_str,
                    current_val,
                    elem_type_str,
                    elem_converted,
                    i,
                });

                self.allocator.free(current_val);
                if (i < from_elems.len - 1) {
                    current_val = try self.allocator.dupe(u8, insert_temp);
                    self.allocator.free(insert_temp);
                } else {
                    return insert_temp;
                }
            }
        }
        // For non-primitives, just return the value if kind matches
        return try self.allocator.dupe(u8, value);
    }

    fn convertType(self: *Codegen, value: []const u8, from_type: Type, to_type: Type) ![]const u8 {
        const from_llvm = self.llvmType(from_type);
        const to_llvm = self.llvmType(to_type);

        // Handle ptr <-> integer conversions
        // Note: both .ptr and .str map to LLVM ptr type
        const from_is_ptr = from_type.kind == .primitive and
            (from_type.kind.primitive == .ptr or from_type.kind.primitive == .str);
        const to_is_ptr = to_type.kind == .primitive and
            (to_type.kind.primitive == .ptr or to_type.kind.primitive == .str);

        // If both are pointer types (ptr or str), no conversion needed
        if (from_is_ptr and to_is_ptr) {
            return try self.allocator.dupe(u8, value);
        }

        if (from_is_ptr and !to_is_ptr) {
            // ptr/str -> integer: use ptrtoint
            const temp = try self.allocTempName();
            try self.output.writer(self.allocator).print("  {s} = ptrtoint {s} {s} to {s}\n", .{ temp, from_llvm, value, to_llvm });
            return temp;
        } else if (!from_is_ptr and to_is_ptr) {
            // integer -> ptr/str: use inttoptr
            const temp = try self.allocTempName();
            try self.output.writer(self.allocator).print("  {s} = inttoptr {s} {s} to {s}\n", .{ temp, from_llvm, value, to_llvm });
            return temp;
        }

        // bool uses zero extension (unsigned)
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

    fn generateExpression(self: *Codegen, expr: *const Expr, expected: ?Type) ![]const u8 {
        switch (expr.*) {
            .integer_literal => |lit| {
                // Integer literals just emit the value; type is determined by context
                // The expected type will be used by convertIfNeeded at the call site
                return try std.fmt.allocPrint(self.allocator, "{d}", .{lit.value});
            },
            .bool_literal => |value| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{if (value) @as(i32, 1) else @as(i32, 0)});
            },
            .string_literal => |lexeme| {
                // Check if expected type has StringLike instance for conversion
                if (expected) |exp| {
                    // Only convert if expected is not str
                    const is_str = exp.kind == .primitive and exp.kind.primitive == .str;
                    if (!is_str) {
                        if (self.findInstance("StringLike", exp)) |_| {
                            // Generate: TypeName__from_str("literal")
                            const str_val = try self.generateStringLiteral(lexeme);
                            defer self.allocator.free(str_val);

                            const type_name = try self.mangledTypeName(exp);
                            defer self.allocator.free(type_name);

                            const result_temp = try self.allocTempName();
                            const result_type_str = try self.llvmTypeString(exp);
                            defer self.allocator.free(result_type_str);

                            try self.output.writer(self.allocator).print("  {s} = call {s} @{s}__from_str(ptr {s})\n", .{
                                result_temp,
                                result_type_str,
                                type_name,
                                str_val,
                            });

                            return result_temp;
                        }
                    }
                }
                // Generate global string constant (default case)
                return try self.generateStringLiteral(lexeme);
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
                // Use expected type if available for bidirectional checking
                const tuple_type = expected orelse self.inferExprType(expr);
                const tuple_type_str = try self.llvmTypeString(tuple_type);
                defer self.allocator.free(tuple_type_str);

                // Extract expected element types if available
                const expected_elem_types: ?[]*Type = if (expected) |exp| blk: {
                    if (exp.kind == .tuple) {
                        break :blk exp.kind.tuple;
                    }
                    break :blk null;
                } else null;

                // Empty tuple case
                if (elements.len == 0) {
                    return try std.fmt.allocPrint(self.allocator, "undef", .{});
                }

                // Start with undef
                var current_val: []const u8 = try std.fmt.allocPrint(self.allocator, "undef", .{});

                for (elements, 0..) |elem, i| {
                    // Pass expected element type if available
                    const elem_expected: ?Type = if (expected_elem_types) |types| types[i].* else null;
                    const elem_val = try self.generateExpression(elem, elem_expected);
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
                const tuple_val = try self.generateExpression(index.tuple, null);
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
                const left_val = try self.generateExpression(binop.left, null);
                defer self.allocator.free(left_val);
                const right_val = try self.generateExpression(binop.right, null);
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
                const operand_val = try self.generateExpression(unop.operand, null);
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
                // Look up function info for parameter types (bidirectional checking)
                const func_info = self.functions.get(call.name);

                // Evaluate arguments with expected parameter types
                var arg_values = try self.allocator.alloc([]const u8, call.args.len);
                defer {
                    for (arg_values) |val| {
                        self.allocator.free(val);
                    }
                    self.allocator.free(arg_values);
                }

                for (call.args, 0..) |arg, i| {
                    // Pass parameter type as expected if available
                    const param_expected: ?Type = if (func_info) |info|
                        if (i < info.param_types.len) info.param_types[i] else null
                    else
                        null;
                    arg_values[i] = try self.generateExpression(arg, param_expected);
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

                const then_val = try self.generateExpression(if_expr.then_branch, null);

                // After generating then_branch, current_block tells us where we are
                const then_end_block = try self.allocator.dupe(u8, self.current_block.?);
                try self.output.writer(self.allocator).print("  br label %{s}\n", .{merge_label});

                // Else block
                try self.output.writer(self.allocator).print("{s}:\n", .{else_label});
                try self.setCurrentBlock(else_label);

                // Else branch is guaranteed to exist by typechecker
                const else_val = try self.generateExpression(if_expr.else_branch.?, null);

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
                // Generate all statements - use current function return type
                const ret_type = if (self.current_function_return_type) |rt| rt else parser.Type{ .kind = .{ .primitive = .i32  }, .usage = .once };
                for (block.statements) |*stmt| {
                    try self.generateStatement(stmt, ret_type);
                }

                // Generate result expression or unit value
                if (block.result) |result| {
                    return try self.generateExpression(result, null);
                } else {
                    return try self.unitValue();
                }
            },
            .unsafe_block => |block| {
                // Generate all statements (same as regular block) - use current function return type
                const ret_type = if (self.current_function_return_type) |rt| rt else parser.Type{ .kind = .{ .primitive = .i32  }, .usage = .once };
                for (block.statements) |*stmt| {
                    try self.generateStatement(stmt, ret_type);
                }

                // Generate result expression or unit value
                if (block.result) |result| {
                    return try self.generateExpression(result, null);
                } else {
                    return try self.unitValue();
                }
            },
            .struct_literal => |lit| {
                const struct_type = self.inferExprType(expr);
                const struct_type_str = try self.llvmTypeString(struct_type);
                defer self.allocator.free(struct_type_str);

                const typedef_info = self.type_defs.get(lit.type_name) orelse unreachable;

                // Build substitution map from type_params
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

                // Apply substitutions to get the resolved type
                const resolved_type = self.substituteTypeParams(typedef_info.type_value, substitutions);
                const struct_fields = resolved_type.kind.struct_type;

                var current_val: []const u8 = try std.fmt.allocPrint(self.allocator, "undef", .{});

                for (struct_fields, 0..) |type_field, field_idx| {
                    var field_value: ?[]const u8 = null;
                    for (lit.fields) |lit_field| {
                        if (std.mem.eql(u8, lit_field.name, type_field.name)) {
                            field_value = try self.generateExpression(lit_field.value, null);
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

                    if (field_idx < struct_fields.len - 1) {
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
                const object_val = try self.generateExpression(access.object, null);
                defer self.allocator.free(object_val);

                const object_type = self.inferExprType(access.object);
                const object_type_str = try self.llvmTypeString(object_type);
                defer self.allocator.free(object_type_str);

                const type_name = switch (object_type.kind) {
                    .named => |name| name,
                    .dependent => |dep| dep.base,
                    else => unreachable,
                };
                const typedef_info = self.type_defs.get(type_name) orelse unreachable;

                // Build substitution map if object_type is dependent
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

                // Apply substitutions to get the resolved type
                const resolved_type = self.substituteTypeParams(typedef_info.type_value, substitutions);
                const struct_fields = resolved_type.kind.struct_type;

                var field_idx: usize = 0;
                for (struct_fields, 0..) |field, idx| {
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
                    if (entry.value_ptr.*.type_value.kind == .sum_type) {
                        for (entry.value_ptr.*.type_value.kind.sum_type, 0..) |variant, idx| {
                            if (std.mem.eql(u8, variant.name, call.name)) {
                                found_type_name = entry.key_ptr.*;
                                found_typedef = entry.value_ptr.*.type_value;
                                variant_index = idx;
                                break;
                            }
                        }
                        if (found_type_name != null) break;
                    }
                }

                if (found_typedef == null) {
                    std.debug.print("Undefined constructor: {s}\n", .{call.name});
                    return error.UndefinedConstructor;
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
                const scrutinee_val = try self.generateExpression(match_expr.scrutinee, null);
                defer self.allocator.free(scrutinee_val);

                const scrutinee_type = self.inferExprType(match_expr.scrutinee);
                const scrutinee_type_str = try self.llvmTypeString(scrutinee_type);
                defer self.allocator.free(scrutinee_type_str);

                const type_name = switch (scrutinee_type.kind) {
                    .named => |name| name,
                    .dependent => |dep| dep.base,
                    else => unreachable,
                };
                const typedef_info = self.type_defs.get(type_name) orelse unreachable;

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
                            for (typedef_info.type_value.kind.sum_type, 0..) |variant, variant_idx| {
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

                // Determine the match result type first (before generating arms)
                const result_type = self.inferExprType(expr);

                // Track which arms terminated early (returned)
                var arm_terminated = try self.allocator.alloc(bool, match_expr.arms.len);
                defer self.allocator.free(arm_terminated);

                for (match_expr.arms, 0..) |arm, i| {
                    try self.output.writer(self.allocator).print("{s}:\n", .{arm_labels[i]});
                    try self.setCurrentBlock(arm_labels[i]);

                    // Extract payload and add pattern bindings to variable scope
                    switch (arm.pattern) {
                        .identifier => {},
                        .constructor => |constructor| {
                            // Find the variant to get payload types
                            var found_variant: ?parser.SumTypeVariant = null;
                            for (typedef_info.type_value.kind.sum_type) |variant| {
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

                    // Pass match result type as expected for bidirectional checking
                    const arm_val = try self.generateExpression(arm.body, result_type);
                    defer self.allocator.free(arm_val);

                    // Convert arm value to match result type if needed (fallback)
                    if (!self.block_terminated) {
                        const arm_type = self.inferExprType(arm.body);
                        arm_vals[i] = try self.convertIfNeeded(arm_val, arm_type, result_type);
                        arm_end_blocks[i] = try self.allocator.dupe(u8, self.current_block.?);
                        try self.output.writer(self.allocator).print("  br label %{s}\n", .{merge_label});
                        arm_terminated[i] = false;
                    } else {
                        arm_vals[i] = try self.allocator.dupe(u8, arm_val);
                        arm_end_blocks[i] = try self.allocator.dupe(u8, self.current_block.?);
                        arm_terminated[i] = true;
                    }

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

                const result_type_str = try self.llvmTypeString(result_type);
                defer self.allocator.free(result_type_str);

                const result_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = phi {s}", .{ result_temp, result_type_str });

                // Only include non-terminated arms in phi node
                var first = true;
                for (arm_vals, arm_end_blocks, arm_terminated) |val, block, terminated| {
                    if (!terminated) {
                        if (!first) try self.output.appendSlice(self.allocator, ",");
                        try self.output.writer(self.allocator).print(" [ {s}, %{s} ]", .{ val, block });
                        first = false;
                    }
                }
                try self.output.appendSlice(self.allocator, "\n");

                return result_temp;
            },
            .method_call => |method_call| {
                // Special case: if object is a dependent_type_ref, don't generate object value
                const is_type_method = method_call.object.* == .dependent_type_ref;

                // Generate method call as: Type__method_name(object, args...)
                const type_name = if (is_type_method) blk: {
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
                    const object_type = self.inferExprType(method_call.object);
                    break :blk try self.mangledTypeName(object_type);
                };
                defer self.allocator.free(type_name);

                // Generate mangled method name: TypeName__methodName
                const mangled_name = try std.fmt.allocPrint(self.allocator, "{s}__{s}", .{ type_name, method_call.method_name });
                defer self.allocator.free(mangled_name);

                // Generate arguments: object is the first arg (unless it's a type method)
                const object_val = if (!is_type_method) try self.generateExpression(method_call.object, null) else null;
                defer if (object_val) |val| self.allocator.free(val);

                var arg_vals: std.ArrayList([]const u8) = .empty;
                defer {
                    for (arg_vals.items) |val| {
                        self.allocator.free(val);
                    }
                    arg_vals.deinit(self.allocator);
                }

                // First argument is the object (self parameter), unless it's a type method
                if (object_val) |val| {
                    try arg_vals.append(self.allocator, try self.allocator.dupe(u8, val));
                }

                // Then the rest of the arguments
                for (method_call.args) |arg| {
                    const val = try self.generateExpression(arg, null);
                    try arg_vals.append(self.allocator, val);
                }

                // Determine return type
                const return_type = self.inferExprType(expr);
                const return_type_str = try self.llvmTypeString(return_type);
                defer self.allocator.free(return_type_str);

                const result_temp = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = call {s} @{s}(", .{
                    result_temp,
                    return_type_str,
                    mangled_name,
                });

                // Write arguments with types
                for (arg_vals.items, 0..) |val, i| {
                    if (i > 0) try self.output.appendSlice(self.allocator, ", ");
                    // For type methods, all args come from method_call.args
                    // For instance methods, first arg is object, rest are from method_call.args
                    const arg_expr = if (is_type_method) method_call.args[i] else if (i == 0) method_call.object else method_call.args[i - 1];
                    const arg_type = self.inferExprType(arg_expr);
                    const arg_type_str = try self.llvmTypeString(arg_type);
                    defer self.allocator.free(arg_type_str);
                    try self.output.writer(self.allocator).print("{s} {s}", .{ arg_type_str, val });
                }

                try self.output.appendSlice(self.allocator, ")\n");
                return result_temp;
            },
            .intrinsic_call => |call| {
                // Generate LLVM IR for intrinsic calls
                if (std.mem.eql(u8, call.name, "@ptr_of")) {
                    // @ptr_of(value) -> alloca + store
                    const value_val = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(value_val);

                    const value_type = self.inferExprType(call.args[0]);
                    const value_type_str = try self.llvmTypeString(value_type);
                    defer self.allocator.free(value_type_str);

                    // Allocate stack space
                    const ptr_temp = try self.allocTempName();
                    try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{ ptr_temp, value_type_str });

                    // Store value to the pointer
                    try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ value_type_str, value_val, ptr_temp });

                    return ptr_temp;
                } else if (std.mem.eql(u8, call.name, "@type")) {
                    // @type(TypeName) -> returns Type value (represented as i64)
                    // Extract type name and return a constant representing it
                    const type_name_expr = call.args[0];
                    const type_name = switch (type_name_expr.*) {
                        .variable => |v| v,
                        .constructor_call => |c| c.name,
                        else => unreachable,
                    };

                    // Map type names to integer constants
                    const type_id: i64 = if (std.mem.eql(u8, type_name, "bool")) 1
                        else if (std.mem.eql(u8, type_name, "i8")) 2
                        else if (std.mem.eql(u8, type_name, "i16")) 3
                        else if (std.mem.eql(u8, type_name, "i32")) 4
                        else if (std.mem.eql(u8, type_name, "i64")) 5
                        else if (std.mem.eql(u8, type_name, "u8")) 6
                        else if (std.mem.eql(u8, type_name, "u16")) 7
                        else if (std.mem.eql(u8, type_name, "u32")) 8
                        else if (std.mem.eql(u8, type_name, "u64")) 9
                        else if (std.mem.eql(u8, type_name, "ptr")) 10
                        else unreachable;

                    return try std.fmt.allocPrint(self.allocator, "{d}", .{type_id});
                } else if (std.mem.eql(u8, call.name, "@ptr_read")) {
                    // @ptr_read(ptr, type_val) -> load based on type
                    const ptr_val = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(ptr_val);

                    const type_arg = call.args[1];

                    // Check if second arg is compile-time @type(...) or runtime Type variable
                    if (type_arg.* == .intrinsic_call and std.mem.eql(u8, type_arg.intrinsic_call.name, "@type")) {
                        // Compile-time known type - direct load
                        const type_name_expr = type_arg.intrinsic_call.args[0];
                        const type_name = switch (type_name_expr.*) {
                            .variable => |v| v,
                            .constructor_call => |c| c.name,
                            else => unreachable,
                        };

                        const type_str = if (std.mem.eql(u8, type_name, "i8")) "i8"
                            else if (std.mem.eql(u8, type_name, "i16")) "i16"
                            else if (std.mem.eql(u8, type_name, "i32")) "i32"
                            else if (std.mem.eql(u8, type_name, "i64")) "i64"
                            else if (std.mem.eql(u8, type_name, "u8")) "i8"
                            else if (std.mem.eql(u8, type_name, "u16")) "i16"
                            else if (std.mem.eql(u8, type_name, "u32")) "i32"
                            else if (std.mem.eql(u8, type_name, "u64")) "i64"
                            else if (std.mem.eql(u8, type_name, "bool")) "i1"
                            else if (std.mem.eql(u8, type_name, "ptr")) "ptr"
                            else unreachable;

                        const loaded_val = try self.allocTempName();
                        try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ loaded_val, type_str, ptr_val });

                        // Ptr types don't need conversion, others extend to i64
                        if (std.mem.eql(u8, type_str, "ptr")) {
                            return loaded_val;
                        }

                        // Extend/convert to i64 if needed
                        const i64_val = if (std.mem.eql(u8, type_str, "i64"))
                            loaded_val
                        else blk: {
                            const extended = try self.allocTempName();
                            const is_signed = std.mem.eql(u8, type_name, "i8") or
                                            std.mem.eql(u8, type_name, "i16") or
                                            std.mem.eql(u8, type_name, "i32");
                            const ext_op = if (is_signed) "sext" else "zext";
                            try self.output.writer(self.allocator).print("  {s} = {s} {s} {s} to i64\n",
                                .{ extended, ext_op, type_str, loaded_val });
                            self.allocator.free(loaded_val);
                            break :blk extended;
                        };

                        // Return the i64 value directly (not converted to ptr)
                        return i64_val;
                    } else {
                        // Runtime Type variable - generate runtime dispatch based on type ID
                        const type_val = try self.generateExpression(type_arg, null);
                        defer self.allocator.free(type_val);

                        // Generate switch on type ID
                        // Create basic blocks for each type case and a default case
                        const result_temp = try self.allocTempName();
                        defer self.allocator.free(result_temp);
                        try self.output.writer(self.allocator).print("  {s} = alloca {s}\n", .{ result_temp, self.target_info.native_int_type });

                        const type_id_temp = try self.allocTempName();
                        defer self.allocator.free(type_id_temp);
                        try self.output.writer(self.allocator).print("  {s} = icmp eq {s} {s}, 1\n", .{ type_id_temp, self.target_info.native_int_type, type_val });

                        const bool_label = try self.allocLabel("bool_case");
                        defer self.allocator.free(bool_label);
                        const i8_label = try self.allocLabel("i8_check");
                        defer self.allocator.free(i8_label);
                        const continue_label = try self.allocLabel("continue");
                        defer self.allocator.free(continue_label);

                        try self.output.writer(self.allocator).print("  br i1 {s}, label %{s}, label %{s}\n\n", .{ type_id_temp, bool_label, i8_label });

                        // bool case (type ID 1)
                        try self.output.writer(self.allocator).print("{s}:\n", .{bool_label});
                        const bool_val = try self.allocTempName();
                        defer self.allocator.free(bool_val);
                        try self.output.writer(self.allocator).print("  {s} = load i1, ptr {s}\n", .{ bool_val, ptr_val });
                        const bool_ext = try self.allocTempName();
                        defer self.allocator.free(bool_ext);
                        try self.output.writer(self.allocator).print("  {s} = zext i1 {s} to {s}\n", .{ bool_ext, bool_val, self.target_info.native_int_type });
                        try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ self.target_info.native_int_type, bool_ext, result_temp });
                        try self.output.writer(self.allocator).print("  br label %{s}\n\n", .{continue_label});

                        // i8 case (type ID 2) - continue with other types
                        try self.output.writer(self.allocator).print("{s}:\n", .{i8_label});
                        const i8_check = try self.allocTempName();
                        defer self.allocator.free(i8_check);
                        try self.output.writer(self.allocator).print("  {s} = icmp eq {s} {s}, 2\n", .{ i8_check, self.target_info.native_int_type, type_val });
                        const i8_block = try self.allocLabel("i8_block");
                        defer self.allocator.free(i8_block);
                        const i16_label = try self.allocLabel("default");
                        defer self.allocator.free(i16_label);
                        try self.output.writer(self.allocator).print("  br i1 {s}, label %{s}, label %{s}\n\n", .{ i8_check, i8_block, i16_label });

                        try self.output.writer(self.allocator).print("{s}:\n", .{i8_block});
                        const i8_val = try self.allocTempName();
                        defer self.allocator.free(i8_val);
                        try self.output.writer(self.allocator).print("  {s} = load i8, ptr {s}\n", .{ i8_val, ptr_val });
                        const i8_ext = try self.allocTempName();
                        defer self.allocator.free(i8_ext);
                        try self.output.writer(self.allocator).print("  {s} = sext i8 {s} to {s}\n", .{ i8_ext, i8_val, self.target_info.native_int_type });
                        try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ self.target_info.native_int_type, i8_ext, result_temp });
                        try self.output.writer(self.allocator).print("  br label %{s}\n\n", .{continue_label});

                        // Generate remaining type cases (i16, i32, i64, u8, u16, u32, u64)
                        // For simplicity, defaulting to native int load for now
                        try self.output.writer(self.allocator).print("{s}:\n", .{i16_label});
                        const default_val = try self.allocTempName();
                        defer self.allocator.free(default_val);
                        try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ default_val, self.target_info.native_int_type, ptr_val });
                        try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ self.target_info.native_int_type, default_val, result_temp });
                        try self.output.writer(self.allocator).print("  br label %{s}\n\n", .{continue_label});

                        // Continue label
                        try self.output.writer(self.allocator).print("{s}:\n", .{continue_label});
                        const final_result = try self.allocTempName();
                        defer self.allocator.free(final_result);
                        try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ final_result, self.target_info.native_int_type, result_temp });

                        // Convert native int result to ptr for runtime dispatch (type unknown at compile time)
                        const ptr_result = try self.allocTempName();
                        try self.output.writer(self.allocator).print("  {s} = inttoptr {s} {s} to ptr\n", .{ ptr_result, self.target_info.native_int_type, final_result });

                        return ptr_result;
                    }
                } else if (std.mem.eql(u8, call.name, "@ptr_write")) {
                    // @ptr_write(ptr, value) -> store
                    const ptr_val = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(ptr_val);

                    const value_val = try self.generateExpression(call.args[1], null);
                    defer self.allocator.free(value_val);

                    // Infer the type of the value to determine if conversion is needed
                    const value_type = self.inferExprType(call.args[1]);

                    // Determine the LLVM type string for the value
                    const value_llvm_type = self.llvmType(value_type);

                    // If value is ptr type, convert to i64 before storing
                    // Otherwise, store the value directly
                    const store_val: []const u8 = if (value_type.kind == .primitive and value_type.kind.primitive == .ptr) blk: {
                        const int_val = try self.allocTempName();
                        try self.output.writer(self.allocator).print("  {s} = ptrtoint ptr {s} to {s}\n", .{ int_val, value_val, self.target_info.native_int_type });
                        break :blk int_val;
                    } else blk: {
                        break :blk value_val;
                    };
                    defer if (value_type.kind == .primitive and value_type.kind.primitive == .ptr) self.allocator.free(store_val);

                    // Use the actual value type for the store, not always native_int_type
                    const store_type = if (value_type.kind == .primitive and value_type.kind.primitive == .ptr) self.target_info.native_int_type else value_llvm_type;
                    try self.output.writer(self.allocator).print("  store {s} {s}, ptr {s}\n", .{ store_type, store_val, ptr_val });

                    // Return unit value (undef for empty tuple)
                    return try std.fmt.allocPrint(self.allocator, "undef", .{});
                } else if (std.mem.eql(u8, call.name, "@ptr_offset")) {
                    // @ptr_offset(ptr, offset) -> getelementptr
                    const ptr_val = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(ptr_val);

                    const offset_val = try self.generateExpression(call.args[1], null);
                    defer self.allocator.free(offset_val);

                    const result_temp = try self.allocTempName();
                    // Use native int size for pointer arithmetic
                    try self.output.writer(self.allocator).print("  {s} = getelementptr {s}, ptr {s}, {s} {s}\n", .{ result_temp, self.target_info.native_int_type, ptr_val, self.target_info.native_int_type, offset_val });

                    return result_temp;
                } else if (std.mem.eql(u8, call.name, "@int_to_ptr")) {
                    // @int_to_ptr(int_val) -> inttoptr
                    const int_val = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(int_val);

                    const result_temp = try self.allocTempName();
                    // Convert integer to pointer
                    try self.output.writer(self.allocator).print("  {s} = inttoptr {s} {s} to ptr\n", .{ result_temp, self.target_info.native_int_type, int_val });

                    return result_temp;
                } else if (std.mem.eql(u8, call.name, "@ptr_to_int")) {
                    // @ptr_to_int(ptr_val) -> ptrtoint
                    const ptr_val = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(ptr_val);

                    const result_temp = try self.allocTempName();
                    // Convert pointer to integer
                    try self.output.writer(self.allocator).print("  {s} = ptrtoint ptr {s} to {s}\n", .{ result_temp, ptr_val, self.target_info.native_int_type });

                    return result_temp;
                } else if (std.mem.eql(u8, call.name, "@join")) {
                    // @join(task, @type(T)) - wait for coroutine completion and get result
                    // task is a coroutine handle (ptr)
                    const task_handle = try self.generateExpression(call.args[0], null);
                    defer self.allocator.free(task_handle);

                    // Get the result type from the task (second arg is @type(T))
                    const type_arg = call.args[1];
                    if (type_arg.* != .intrinsic_call or !std.mem.eql(u8, type_arg.intrinsic_call.name, "@type")) {
                        unreachable;
                    }
                    const type_name_expr = type_arg.intrinsic_call.args[0];
                    const type_name = switch (type_name_expr.*) {
                        .variable => |v| v,
                        .constructor_call => |c| c.name,
                        else => unreachable,
                    };

                    const llvm_type_map = std.StaticStringMap([]const u8).initComptime(.{
                        .{ "i8", "i8" },
                        .{ "i16", "i16" },
                        .{ "i32", "i32" },
                        .{ "i64", "i64" },
                        .{ "u8", "i8" },
                        .{ "u16", "i16" },
                        .{ "u32", "i32" },
                        .{ "u64", "i64" },
                        .{ "bool", "i1" },
                        .{ "ptr", "ptr" },
                    });
                    const result_type_str = llvm_type_map.get(type_name) orelse unreachable;

                    // Wait for task completion - use scheduler if in async block
                    if (self.in_async_block and self.needs_scheduler) {
                        // Use scheduler to run tasks until this one completes
                        // This allows other tasks to interleave
                        try self.output.writer(self.allocator).print("  call void @orion_scheduler_run_until(ptr {s})\n", .{task_handle});
                    } else {
                        // Direct resume loop (no scheduler)
                        const join_id = self.next_suspend;
                        self.next_suspend += 1;
                        try self.output.writer(self.allocator).print("  br label %join.loop.{d}\n", .{join_id});
                        try self.output.writer(self.allocator).print("join.loop.{d}:\n", .{join_id});
                        try self.output.writer(self.allocator).print("  %join.done.{d} = call i1 @llvm.coro.done(ptr {s})\n", .{ join_id, task_handle });
                        try self.output.writer(self.allocator).print("  br i1 %join.done.{d}, label %join.exit.{d}, label %join.resume.{d}\n", .{ join_id, join_id, join_id });
                        try self.output.writer(self.allocator).print("join.resume.{d}:\n", .{join_id});
                        try self.output.writer(self.allocator).print("  call void @llvm.coro.resume(ptr {s})\n", .{task_handle});
                        try self.output.writer(self.allocator).print("  br label %join.loop.{d}\n", .{join_id});
                        try self.output.writer(self.allocator).print("join.exit.{d}:\n", .{join_id});
                    }

                    // Get promise pointer (where result is stored)
                    // Alignment is 8 bytes for i64 results
                    const promise_ptr = try self.allocTempName();
                    defer self.allocator.free(promise_ptr);
                    try self.output.writer(self.allocator).print("  {s} = call ptr @llvm.coro.promise(ptr {s}, i32 8, i1 false)\n", .{ promise_ptr, task_handle });

                    // Load result from promise
                    const result_val = try self.allocTempName();
                    try self.output.writer(self.allocator).print("  {s} = load {s}, ptr {s}\n", .{ result_val, result_type_str, promise_ptr });

                    // Destroy the coroutine (cleanup)
                    try self.output.writer(self.allocator).print("  call void @llvm.coro.destroy(ptr {s})\n", .{task_handle});

                    return result_val;
                } else if (std.mem.eql(u8, call.name, "@yield")) {
                    // @yield() - suspend coroutine, return control to scheduler
                    // Must be inside a coroutine (spawn body)
                    const coro_hdl = self.current_coro_handle orelse {
                        std.debug.print("@yield called outside coroutine context\n", .{});
                        unreachable;
                    };

                    const suspend_id = self.next_suspend;
                    self.next_suspend += 1;

                    // Generate suspension point with correct LLVM coroutine switch pattern:
                    // - default: suspend (branch to coro.ret for single return point)
                    // - i8 0: resume (continue execution after yield)
                    // - i8 1: cleanup (destroy path)
                    try self.output.writer(self.allocator).print("  %save.{d} = call token @llvm.coro.save(ptr {s})\n", .{ suspend_id, coro_hdl });
                    try self.output.writer(self.allocator).print("  %suspend.{d} = call i8 @llvm.coro.suspend(token %save.{d}, i1 false)\n", .{ suspend_id, suspend_id });
                    try self.output.writer(self.allocator).print("  switch i8 %suspend.{d}, label %coro.ret [\n", .{suspend_id});
                    try self.output.writer(self.allocator).print("    i8 0, label %suspend.{d}.resume\n", .{suspend_id});
                    try self.output.writer(self.allocator).print("    i8 1, label %coro.final.cleanup\n", .{});
                    try self.output.writer(self.allocator).print("  ]\n", .{});
                    // Resume block - continue execution after yield
                    try self.output.writer(self.allocator).print("suspend.{d}.resume:\n", .{suspend_id});

                    // @yield returns unit (empty struct in LLVM)
                    return try self.allocator.dupe(u8, "zeroinitializer");
                } else {
                    // Unknown intrinsic
                    unreachable;
                }
            },
            .dependent_type_ref => {
                // Dependent type references should only appear in method call context
                unreachable;
            },
            .async_expr => |async_expr| {
                // async { body } - creates a scope for structured concurrency
                // Track that we're in async block for spawn to register with scheduler
                const was_in_async = self.in_async_block;
                const had_scheduler = self.needs_scheduler;
                self.in_async_block = true;

                // Generate body
                const result = try self.generateExpression(async_expr.body, expected);

                // Restore async block state
                self.in_async_block = was_in_async;

                // If we spawned tasks in this async block, run the scheduler
                // to ensure all tasks complete before async block returns
                if (self.needs_scheduler and !had_scheduler) {
                    try self.output.writer(self.allocator).print("  call void @orion_scheduler_run()\n", .{});
                }

                return result;
            },
            .spawn_expr => |spawn_expr| {
                // spawn { body } - creates a Task[T] using LLVM coroutines
                // The body is outlined into a separate coroutine function

                // Get the body result type
                const body_type = self.inferExprType(spawn_expr.body);
                const body_type_str = try self.llvmTypeString(body_type);
                defer self.allocator.free(body_type_str);

                // Generate unique coroutine function name
                const coro_name = try std.fmt.allocPrint(self.allocator, "orion_coro_{d}", .{self.next_coro});
                defer self.allocator.free(coro_name);
                self.next_coro += 1;
                self.needs_coro_intrinsics = true;

                // Outline the body into a coroutine function
                try self.generateCoroutineFunction(coro_name, spawn_expr.body, body_type_str);

                // Call the coroutine to get the handle
                const coro_handle = try self.allocTempName();
                try self.output.writer(self.allocator).print("  {s} = call ptr @{s}()\n", .{ coro_handle, coro_name });

                // Register with scheduler for Go-style concurrency
                if (self.in_async_block) {
                    self.needs_scheduler = true;
                    try self.output.writer(self.allocator).print("  call void @orion_scheduler_spawn(ptr {s})\n", .{coro_handle});
                }

                // Return the coroutine handle (Task[T])
                return coro_handle;
            },
            .select_expr => |select_expr| {
                // select { arms... default? } - multi-channel wait
                // Eager evaluation: for now, execute default or first arm immediately
                // Full async select requires scheduler integration in Phase 5d

                if (select_expr.default_arm) |default_body| {
                    // Non-blocking select: execute default immediately
                    return try self.generateExpression(default_body, expected);
                }

                if (select_expr.arms.len == 0) {
                    // Empty select with no default - return unit
                    return try self.allocator.dupe(u8, "void");
                }

                // Execute first arm (placeholder for real channel polling)
                const first_arm = select_expr.arms[0];

                // Handle binding for recv operations
                if (first_arm.binding) |binding_name| {
                    switch (first_arm.operation) {
                        .recv => |channel_expr| {
                            // Generate channel expression (to evaluate side effects)
                            const channel_val = try self.generateExpression(channel_expr, null);
                            defer self.allocator.free(channel_val);

                            // Recv binding: allocate local and store placeholder value
                            // Full impl would call channel_recv() and store actual value
                            const binding_ptr = try self.allocTempName();
                            try self.output.writer(self.allocator).print("  {s} = alloca i64\n", .{binding_ptr});
                            try self.output.writer(self.allocator).print("  store i64 0, ptr {s} ; placeholder recv\n", .{binding_ptr});

                            // Register binding in scope (i64 placeholder type)
                            const binding_ptr_dupe = try self.allocator.dupe(u8, binding_ptr);
                            try self.variables.put(binding_name, .{
                                .llvm_ptr = binding_ptr_dupe,
                                .var_type = .{ .kind = .{ .primitive = .i64 }, .usage = .unlimited },
                            });
                        },
                        .send => {},
                    }
                } else {
                    // No binding - still evaluate channel for side effects
                    switch (first_arm.operation) {
                        .recv => |channel_expr| {
                            const channel_val = try self.generateExpression(channel_expr, null);
                            self.allocator.free(channel_val);
                        },
                        .send => |send_op| {
                            const channel_val = try self.generateExpression(send_op.channel, null);
                            defer self.allocator.free(channel_val);
                            const value_val = try self.generateExpression(send_op.value, null);
                            self.allocator.free(value_val);
                            // Full impl would call channel_send(channel, value)
                        },
                    }
                }

                // Execute arm body
                return try self.generateExpression(first_arm.body, expected);
            },
        }
    }

    fn generateConditionAsBool(self: *Codegen, condition: *const Expr) CodegenError![]const u8 {
        const condition_val = try self.generateExpression(condition, null);
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

    fn llvmType(self: *Codegen, typ: Type) []const u8 {
        switch (typ.kind) {
            .primitive => return typ.llvmTypeName(),
            .dependent => |dep| {
                // Look up underlying type
                if (self.type_defs.get(dep.base)) |typedef_info| {
                    var substitutions = self.buildSubstitutionMap(typedef_info, dep);
                    defer substitutions.deinit();
                    const resolved = self.substituteTypeParams(typedef_info.type_value, substitutions);
                    return self.llvmType(resolved);
                }
                return "ptr"; // Default fallback
            },
            else => return "ptr", // Default for non-primitives
        }
    }

    /// Generate mangled type name for dependent types: Vec$$Int$5$$
    fn mangledTypeName(self: *Codegen, typ: Type) ![]const u8 {
        switch (typ.kind) {
            .primitive => |prim| return try self.allocator.dupe(u8, @tagName(prim)),
            .named => |name| return try self.allocator.dupe(u8, name),
            .dependent => |dep| {
                var result = std.ArrayList(u8).empty;
                errdefer result.deinit(self.allocator);

                // Start with base type name
                try result.appendSlice(self.allocator, dep.base);

                // Start dependent type parameters section with $$
                try result.appendSlice(self.allocator, "$$");

                // Add type parameters
                for (dep.type_params, 0..) |type_param, i| {
                    if (i > 0) try result.append(self.allocator, '$');

                    switch (type_param) {
                        .variable => |v| try result.appendSlice(self.allocator, v),
                        .concrete => |t| {
                            const type_name = try self.mangledTypeName(t.*);
                            defer self.allocator.free(type_name);
                            try result.appendSlice(self.allocator, type_name);
                        },
                    }
                }

                // Add value parameters (as compile-time constants for now)
                for (dep.value_params) |value_param| {
                    try result.append(self.allocator, '$');

                    // For now, just handle integer literals
                    // TODO: evaluate constant expressions
                    switch (value_param) {
                        .integer_literal => |lit| {
                            const val_str = try std.fmt.allocPrint(self.allocator, "{d}", .{lit.value});
                            defer self.allocator.free(val_str);
                            try result.appendSlice(self.allocator, val_str);
                        },
                        else => {
                            // For non-literal expressions, use placeholder for now
                            try result.appendSlice(self.allocator, "expr");
                        },
                    }
                }

                // End dependent type parameters section with $$
                try result.appendSlice(self.allocator, "$$");

                return try result.toOwnedSlice(self.allocator);
            },
            else => return try self.allocator.dupe(u8, "unknown"),
        }
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
                const typedef_info = self.type_defs.get(name) orelse {
                    std.debug.print("Type not found in type_defs: {s}\n", .{name});
                    std.debug.print("Available types:\n", .{});
                    var iter = self.type_defs.iterator();
                    while (iter.next()) |entry| {
                        std.debug.print("  - {s}\n", .{entry.key_ptr.*});
                    }
                    return error.UndefinedType;
                };
                return try self.llvmTypeString(typedef_info.type_value);
            },
            .dependent => |dep| {
                // Dependent types resolve to their underlying type definition
                // Look up the type definition and get its LLVM type
                const typedef_info = self.type_defs.get(dep.base) orelse {
                    // If not found, default to ptr
                    return try self.allocator.dupe(u8, "ptr");
                };
                // Build substitution map and apply
                var substitutions = self.buildSubstitutionMap(typedef_info, dep);
                defer substitutions.deinit();
                const resolved = self.substituteTypeParams(typedef_info.type_value, substitutions);
                return try self.llvmTypeString(resolved);
            },
        }
    }

    fn exprReturns(expr: *const Expr) bool {
        switch (expr.*) {
            .block_expr => |block| {
                // Check statements for return
                for (block.statements) |stmt| {
                    if (stmt == .return_stmt) return true;
                }
                // Check result expression
                if (block.result) |result| {
                    return exprReturns(result);
                }
                return false;
            },
            .unsafe_block => |block| {
                // Check statements for return
                for (block.statements) |stmt| {
                    if (stmt == .return_stmt) return true;
                }
                // Check result expression
                if (block.result) |result| {
                    return exprReturns(result);
                }
                return false;
            },
            .if_expr => |if_expr| {
                // If both branches return, the if returns
                const then_returns = exprReturns(if_expr.then_branch);
                if (if_expr.else_branch) |else_branch| {
                    const else_returns = exprReturns(else_branch);
                    return then_returns and else_returns;
                }
                return false;
            },
            else => return false,
        }
    }

    fn inferExprType(self: *Codegen, expr: *const Expr) Type {
        switch (expr.*) {
            .integer_literal => |lit| return lit.inferred_type,
            .bool_literal => return .{ .kind = .{ .primitive = .bool  }, .usage = .once },
            .string_literal => return .{ .kind = .{ .primitive = .str }, .usage = .unlimited },
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
            .unsafe_block => |block| {
                if (block.result) |result| {
                    return self.inferExprType(result);
                } else {
                    return .{ .kind = .{ .primitive = .i32  }, .usage = .once }; // Unit value
                }
            },
            .struct_literal => |lit| {
                // Return dependent type if type_params present
                if (lit.type_params) |type_params| {
                    return Type{
                        .kind = .{ .dependent = .{
                            .base = lit.type_name,
                            .type_params = type_params,
                            .value_params = &.{},
                        } },
                        .usage = .once,
                    };
                }
                return .{ .kind = .{ .named = lit.type_name  }, .usage = .once };
            },
            .field_access => |access| {
                const object_type = self.inferExprType(access.object);
                const type_name = switch (object_type.kind) {
                    .named => |name| name,
                    .dependent => |dep| dep.base,
                    else => unreachable,
                };
                const typedef_info = self.type_defs.get(type_name) orelse unreachable;

                // Build substitution map if object_type is dependent
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

                // Apply substitutions and look up field
                const resolved = self.substituteTypeParams(typedef_info.type_value, substitutions);
                for (resolved.kind.struct_type) |field| {
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
                    if (entry.value_ptr.*.type_value.kind == .sum_type) {
                        for (entry.value_ptr.*.type_value.kind.sum_type) |variant| {
                            if (std.mem.eql(u8, variant.name, call.name)) {
                                return .{ .kind = .{ .named = entry.key_ptr.*  }, .usage = .once };
                            }
                        }
                    }
                }
                std.debug.print("Cannot infer type for constructor: {s} (type not found in current module)\n", .{call.name});
                // Return a dummy type to avoid panic
                return .{ .kind = .{ .primitive = .i32 }, .usage = .once };
            },
            .match_expr => |match_expr| {
                // If we're in a function and all arms return, use the function return type
                if (self.current_function_return_type) |ret_type| {
                    var all_return = true;
                    for (match_expr.arms) |arm| {
                        if (!exprReturns(arm.body)) {
                            all_return = false;
                            break;
                        }
                    }
                    if (all_return) {
                        return ret_type;
                    }
                }

                // Match result type is the type of the first non-returning arm
                for (match_expr.arms) |arm| {
                    if (!exprReturns(arm.body)) {
                        return self.inferExprType(arm.body);
                    }
                }
                // If all arms return, use the first arm's type (shouldn't happen in practice)
                return self.inferExprType(match_expr.arms[0].body);
            },
            .method_call => |method_call| {
                // Look up method return type from instance_methods
                const type_name = if (method_call.object.* == .dependent_type_ref) blk: {
                    const ref = method_call.object.dependent_type_ref;
                    // Mangle: Vec$ptr$8
                    var mangled = std.ArrayList(u8).empty;
                    defer mangled.deinit(self.allocator);
                    mangled.appendSlice(self.allocator, ref.type_name) catch unreachable;
                    for (ref.type_params) |param| {
                        mangled.append(self.allocator, '$') catch unreachable;
                        switch (param) {
                            .variable => |v| mangled.appendSlice(self.allocator, v) catch unreachable,
                            .concrete => |t| {
                                const type_str = switch (t.kind) {
                                    .primitive => |prim| @tagName(prim),
                                    .named => |name| name,
                                    else => "unknown",
                                };
                                mangled.appendSlice(self.allocator, type_str) catch unreachable;
                            },
                        }
                    }
                    // Add value parameters to mangling
                    for (ref.value_params) |val_param| {
                        mangled.append(self.allocator, '$') catch unreachable;
                        // For now, only handle integer literals
                        if (val_param == .integer_literal) {
                            const int_str = std.fmt.allocPrint(self.allocator, "{d}", .{val_param.integer_literal.value}) catch unreachable;
                            defer self.allocator.free(int_str);
                            mangled.appendSlice(self.allocator, int_str) catch unreachable;
                        }
                    }
                    break :blk self.allocator.dupe(u8, mangled.items) catch unreachable;
                } else blk: {
                    const object_type = self.inferExprType(method_call.object);
                    break :blk self.mangledTypeName(object_type) catch unreachable;
                };
                defer self.allocator.free(type_name);

                const mangled_name = std.fmt.allocPrint(self.allocator, "{s}__{s}", .{ type_name, method_call.method_name }) catch unreachable;
                defer self.allocator.free(mangled_name);

                if (self.instance_methods.get(mangled_name)) |return_type| {
                    return return_type;
                } else {
                    return Type{ .kind = .{ .primitive = .i32 }, .usage = .once };
                }
            },
            .intrinsic_call => |call| {
                // Infer type of intrinsic calls
                if (std.mem.eql(u8, call.name, "@ptr_of")) {
                    return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@type")) {
                    // @type returns Type primitive
                    return .{ .kind = .{ .primitive = .type }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_read")) {
                    // Return type depends on whether type arg is compile-time or runtime
                    const type_arg = call.args[1];
                    if (type_arg.* == .intrinsic_call and std.mem.eql(u8, type_arg.intrinsic_call.name, "@type")) {
                        // Compile-time: extract the actual type from @type(TypeName)
                        const type_name_expr = type_arg.intrinsic_call.args[0];
                        const type_name = switch (type_name_expr.*) {
                            .variable => |v| v,
                            .constructor_call => |c| c.name,
                            else => {
                                // Default to i64 if we can't determine
                                return .{ .kind = .{ .primitive = .i64 }, .usage = .once };
                            },
                        };

                        // Map type name to actual type
                        // For integer types, we extend to i64
                        // For ptr, we return ptr
                        if (std.mem.eql(u8, type_name, "ptr")) {
                            return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                        } else if (std.mem.eql(u8, type_name, "bool")) {
                            return .{ .kind = .{ .primitive = .bool }, .usage = .once };
                        } else {
                            // Integer types get extended to i64
                            return .{ .kind = .{ .primitive = .i64 }, .usage = .once };
                        }
                    } else {
                        // Runtime: we return ptr since we can't know the type
                        return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                    }
                } else if (std.mem.eql(u8, call.name, "@ptr_write")) {
                    return .{ .kind = .{ .tuple = &[_]*parser.Type{} }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_offset")) {
                    return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@int_to_ptr")) {
                    return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@ptr_to_int")) {
                    return .{ .kind = .{ .primitive = .u64 }, .usage = .once };
                } else if (std.mem.eql(u8, call.name, "@join")) {
                    // @join returns the type specified in the second argument
                    // For now, assume it's determined by the @type() argument
                    // The actual type inference is handled by the call site
                    return .{ .kind = .{ .primitive = .i32 }, .usage = .once }; // Default, overridden by context
                } else if (std.mem.eql(u8, call.name, "@yield")) {
                    // @yield returns unit (empty tuple)
                    return .{ .kind = .{ .tuple = &[_]*parser.Type{} }, .usage = .once };
                } else {
                    std.debug.print("Unknown intrinsic in inferExprType: {s}\n", .{call.name});
                    return .{ .kind = .{ .primitive = .i32 }, .usage = .once };
                }
            },
            .dependent_type_ref => {
                // Dependent type references should only appear in method call context
                unreachable;
            },
            .async_expr => |async_expr| {
                // async { body } returns the type of body
                return self.inferExprType(async_expr.body);
            },
            .spawn_expr => {
                // spawn { body } returns Task[T] which is represented as ptr (coroutine handle)
                return .{ .kind = .{ .primitive = .ptr }, .usage = .once };
            },
            .select_expr => |select_expr| {
                // Select returns the type of the first arm body
                if (select_expr.arms.len > 0) {
                    return self.inferExprType(select_expr.arms[0].body);
                }
                if (select_expr.default_arm) |default| {
                    return self.inferExprType(default);
                }
                return .{ .kind = .{ .tuple = &[_]*parser.Type{} }, .usage = .once };
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

    const source = "fn main() i32 { return 42 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn main() i32 { return 1 + 2 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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
        .{ .src = "fn f() i32 { return 10 - 5 }", .expected = "sub i32 10, 5" },
        .{ .src = "fn f() i32 { return 10 * 5 }", .expected = "mul i32 10, 5" },
        .{ .src = "fn f() i32 { return 10 / 5 }", .expected = "sdiv i32 10, 5" },
        .{ .src = "fn f() i32 { return 10 % 5 }", .expected = "srem i32 10, 5" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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
        .{ .src = "fn f() i32 { return 5 == 3 }", .expected = "icmp eq i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 != 3 }", .expected = "icmp ne i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 < 3 }", .expected = "icmp slt i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 > 3 }", .expected = "icmp sgt i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 <= 3 }", .expected = "icmp sle i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 >= 3 }", .expected = "icmp sge i32 5, 3" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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
        .{ .src = "fn f() i32 { return 5 & 3 }", .expected = "and i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 | 3 }", .expected = "or i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 ^ 3 }", .expected = "xor i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 << 3 }", .expected = "shl i32 5, 3" },
        .{ .src = "fn f() i32 { return 5 >> 3 }", .expected = "ashr i32 5, 3" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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
        .{ .src = "fn f() i32 { return -5 }", .expected = "= sub i32 0, 5" },
        .{ .src = "fn f() i32 { return !5 }", .expected = "= icmp eq i32 0, 5" },
        .{ .src = "fn f() i32 { return ~5 }", .expected = "= xor i32 -1, 5" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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
        .{ .src = "fn f() i32 { return 5 && 3 }", .expected_conversions = "icmp ne i32", .expected_op = "and i1" },
        .{ .src = "fn f() i32 { return 5 || 3 }", .expected_conversions = "icmp ne i32", .expected_op = "or i1" },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { return if true { 1 } else { 0 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
    defer codegen.deinit();

    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    // bool literal should be used directly in br
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "br i1 1") != null);
}

test "codegen: simple if expression" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    const source = "fn f() i32 { return if true { 42 } else { 0 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { return if 5 { 1 } else { 0 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { return if 10 > 5 { 1 } else { 0 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { return if false { 1 } elseif true { 2 } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { return if false { 1 } elseif true { 2 } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { while true { 42 }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { while 5 > 3 { 1 }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
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

    const source = "fn f() i32 { while true { while false { 1 } }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const target = target_module.detectHostTriple();
    var codegen = Codegen.init(testing.allocator, target);
    defer codegen.deinit();

    const llvm_ir = try codegen.generate(&ast);
    defer testing.allocator.free(llvm_ir);

    // Should have two sets of while blocks (outer and inner)
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_header0:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_header3:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_body1:") != null);
    try testing.expect(std.mem.indexOf(u8, llvm_ir, "while_body4:") != null);
}

