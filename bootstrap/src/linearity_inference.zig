const std = @import("std");
const parser = @import("parser.zig");

const AST = parser.AST;
const FunctionDecl = parser.FunctionDecl;
const Stmt = parser.Stmt;
const Expr = parser.Expr;
const Type = parser.Type;
const UsageAnnotation = parser.UsageAnnotation;
const Pattern = parser.Pattern;

/// Infers multiplicity annotations for all function parameters
/// by counting direct uses in each function's scope
pub const LinearityInferenceEngine = struct {
    allocator: std.mem.Allocator,
    current_function: ?*FunctionDecl,
    parameter_uses: std.StringHashMap(u32), // parameter name -> use count
    in_unbounded_loop: bool, // tracks if we're in a loop with unknown iteration count
    loop_depth: u32, // for distinguishing nested loops
    inferred_annotations: std.StringHashMap(UsageAnnotation), // parameter name -> inferred annotation

    pub fn init(allocator: std.mem.Allocator) LinearityInferenceEngine {
        return .{
            .allocator = allocator,
            .current_function = null,
            .parameter_uses = std.StringHashMap(u32).init(allocator),
            .in_unbounded_loop = false,
            .loop_depth = 0,
            .inferred_annotations = std.StringHashMap(UsageAnnotation).init(allocator),
        };
    }

    pub fn deinit(self: *LinearityInferenceEngine) void {
        self.parameter_uses.deinit();
        self.inferred_annotations.deinit();
    }

    /// Infer multiplicities for all functions in the AST
    pub fn inferAll(self: *LinearityInferenceEngine, ast: *AST) !void {
        for (ast.functions.items) |*func| {
            try self.inferFunction(func);
        }
    }

    /// Infer multiplicities for a single function
    fn inferFunction(self: *LinearityInferenceEngine, func: *FunctionDecl) !void {
        self.current_function = func;
        self.parameter_uses.clearRetainingCapacity();
        self.inferred_annotations.clearRetainingCapacity();
        self.in_unbounded_loop = false;
        self.loop_depth = 0;

        // Initialize use counts for all parameters
        for (func.params) |param| {
            try self.parameter_uses.put(param.name, 0);
        }

        // Count uses in function body
        for (func.body.items) |stmt| {
            self.countStmtUses(stmt);
        }

        // Assign inferred annotations to parameters (respecting explicit annotations)
        for (func.params) |*param| {
            const param_name = param.name;
            if (self.parameter_uses.get(param_name)) |use_count| {
                const inferred = self.usageFromCount(use_count);

                // Store inferred value for migration tool
                try self.inferred_annotations.put(param_name, inferred);

                if (param.param_type.annotation_source == .explicit) {
                    // Validate explicit annotation matches inferred
                    const explicit = param.param_type.usage;
                    if (!self.annotationsMatch(explicit, inferred)) {
                        // Suggest inferred annotation
                        std.debug.print("suggestion: function '{s}' parameter '{s}' has annotation ", .{ func.name, param_name });
                        self.printAnnotation(explicit);
                        std.debug.print(" but inferred value is ", .{});
                        self.printAnnotation(inferred);
                        std.debug.print("\n", .{});
                    }
                } else {
                    // Assign inferred annotation
                    param.param_type.usage = inferred;
                }
            }
        }
    }

    fn annotationsMatch(self: *LinearityInferenceEngine, explicit: parser.UsageAnnotation, inferred: parser.UsageAnnotation) bool {
        _ = self;
        return switch (explicit) {
            .once => switch (inferred) {
                .once => true,
                else => false,
            },
            .optional => switch (inferred) {
                .optional => true,
                else => false,
            },
            .unlimited => switch (inferred) {
                .unlimited => true,
                else => false,
            },
            .exactly => |explicit_n| switch (inferred) {
                .exactly => |inferred_n| explicit_n == inferred_n,
                else => false,
            },
        };
    }

    fn printAnnotation(self: *LinearityInferenceEngine, annotation: parser.UsageAnnotation) void {
        _ = self;
        switch (annotation) {
            .once => std.debug.print("@1", .{}),
            .optional => std.debug.print("@?", .{}),
            .unlimited => std.debug.print("@*", .{}),
            .exactly => |n| std.debug.print("@{d}", .{n}),
        }
    }

    pub fn getInferredAnnotation(self: *LinearityInferenceEngine, param_name: []const u8) ?UsageAnnotation {
        return self.inferred_annotations.get(param_name);
    }

    /// Count parameter uses in a statement
    fn countStmtUses(self: *LinearityInferenceEngine, stmt: Stmt) void {
        switch (stmt) {
            .let_binding => |binding| {
                self.countExprUses(binding.value.*);
            },
            .assignment => |assign| {
                self.countExprUses(assign.value.*);
            },
            .while_stmt => |while_loop| {
                self.countExprUses(while_loop.condition.*);
                // Mark as unbounded - in real implementation would analyze bounds
                self.in_unbounded_loop = true;
                self.loop_depth += 1;
                self.countExprUses(while_loop.body.*);
                self.loop_depth -= 1;
                if (self.loop_depth == 0) {
                    self.in_unbounded_loop = false;
                }
            },
            .if_stmt => |if_stmt| {
                self.countExprUses(if_stmt.condition.*);
                for (if_stmt.then_stmts) |s| {
                    self.countStmtUses(s);
                }
                if (if_stmt.else_stmts) |else_stmts| {
                    for (else_stmts) |s| {
                        self.countStmtUses(s);
                    }
                }
            },
            .return_stmt => |expr| {
                self.countExprUses(expr.*);
            },
            .expr => |expr| {
                self.countExprUses(expr.*);
            },
        }
    }


    /// Count parameter uses in an expression
    fn countExprUses(self: *LinearityInferenceEngine, expr: Expr) void {
        switch (expr) {
            .variable => |name| {
                if (self.parameter_uses.getPtr(name)) |count_ptr| {
                    // Only count if not in unbounded loop body
                    if (!self.in_unbounded_loop) {
                        count_ptr.* += 1;
                    }
                }
            },
            .function_call => |call| {
                // Each parameter passed to a function counts as 1 use
                for (call.args) |arg| {
                    self.countExprUses(arg.*);
                }
            },
            .field_access => |access| {
                // Accessing a field of a parameter counts as 1 use
                self.countExprUses(access.object.*);
            },
            .method_call => |method| {
                // Method call on a parameter counts as 1 use
                self.countExprUses(method.object.*);
                for (method.args) |arg| {
                    self.countExprUses(arg.*);
                }
            },
            .if_expr => |if_expr| {
                self.countExprUses(if_expr.condition.*);
                self.countExprUses(if_expr.then_branch.*);
                if (if_expr.else_branch) |else_branch| {
                    self.countExprUses(else_branch.*);
                }
            },
            .match_expr => |match_expr| {
                // The match itself counts as 1 use of the matched value
                self.countExprUses(match_expr.scrutinee.*);
                // Each arm counts uses in its body
                for (match_expr.arms) |arm| {
                    self.countExprUses(arm.body.*);
                }
            },
            .binary_op => |binop| {
                self.countExprUses(binop.left.*);
                self.countExprUses(binop.right.*);
            },
            .unary_op => |unop| {
                self.countExprUses(unop.operand.*);
            },
            .tuple_literal => |tuple| {
                for (tuple) |elem| {
                    self.countExprUses(elem.*);
                }
            },
            .struct_literal => |lit| {
                for (lit.fields) |field| {
                    self.countExprUses(field.value.*);
                }
            },
            .block_expr => |block| {
                for (block.statements) |stmt| {
                    self.countStmtUses(stmt);
                }
                if (block.result) |result_expr| {
                    self.countExprUses(result_expr.*);
                }
            },
            .unsafe_block => |block| {
                for (block.statements) |stmt| {
                    self.countStmtUses(stmt);
                }
                if (block.result) |result_expr| {
                    self.countExprUses(result_expr.*);
                }
            },
            .constructor_call => |call| {
                // Constructor calls with parameter arguments
                for (call.args) |arg| {
                    self.countExprUses(arg.*);
                }
            },
            .tuple_index => |ti| {
                self.countExprUses(ti.tuple.*);
            },
            .intrinsic_call => |call| {
                for (call.args) |arg| {
                    self.countExprUses(arg.*);
                }
            },
            .async_expr => |ae| {
                self.countExprUses(ae.body.*);
            },
            .spawn_expr => |se| {
                self.countExprUses(se.body.*);
            },
            .select_expr => |se| {
                for (se.arms) |arm| {
                    self.countExprUses(arm.body.*);
                }
                if (se.default_arm) |def| {
                    self.countExprUses(def.*);
                }
            },
            else => {
                // Literals, dependent type refs, etc. don't directly use parameters
            },
        }
    }

    /// Convert use count to UsageAnnotation
    fn usageFromCount(self: *LinearityInferenceEngine, count: u32) UsageAnnotation {
        _ = self;
        if (count == 0) {
            return UsageAnnotation{ .optional = {} };
        } else if (count == 1) {
            return UsageAnnotation{ .once = {} };
        } else {
            return UsageAnnotation{ .exactly = count };
        }
    }
};
