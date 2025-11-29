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

    pub fn init(allocator: std.mem.Allocator) LinearityInferenceEngine {
        return .{
            .allocator = allocator,
            .current_function = null,
            .parameter_uses = std.StringHashMap(u32).init(allocator),
            .in_unbounded_loop = false,
            .loop_depth = 0,
        };
    }

    pub fn deinit(self: *LinearityInferenceEngine) void {
        self.parameter_uses.deinit();
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
        self.in_unbounded_loop = false;
        self.loop_depth = 0;

        // Initialize use counts for all parameters
        for (func.params) |param| {
            try self.parameter_uses.put(param.name, 0);
        }

        // Count uses in function body
        for (func.body.items) |stmt| {
            try self.countStmtUses(stmt);
        }

        // Assign inferred annotations to parameters
        var iter = self.parameter_uses.iterator();
        for (func.params) |*param| {
            const param_name = param.name;
            if (self.parameter_uses.get(param_name)) |use_count| {
                param.param_type.usage = try self.usageFromCount(use_count);
            }
        }
    }

    /// Count parameter uses in a statement
    fn countStmtUses(self: *LinearityInferenceEngine, stmt: Stmt) !void {
        switch (stmt) {
            .let_binding => |binding| {
                try self.countExprUses(binding.value.*);
            },
            .assignment => |assign| {
                try self.countExprUses(assign.value.*);
            },
            .while_stmt => |while_loop| {
                try self.countWhileUses(while_loop);
            },
            .if_stmt => |if_stmt| {
                try self.countExprUses(if_stmt.condition.*);
                for (if_stmt.then_stmts) |s| {
                    try self.countStmtUses(s);
                }
                if (if_stmt.else_stmts) |else_stmts| {
                    for (else_stmts) |s| {
                        try self.countStmtUses(s);
                    }
                }
            },
            .return_stmt => |expr| {
                try self.countExprUses(expr.*);
            },
            .expr => |expr| {
                try self.countExprUses(expr.*);
            },
        }
    }

    /// Count parameter uses in a while loop
    /// For unbounded loops, uses in loop body are @* and not counted
    /// Only condition uses count
    fn countWhileUses(self: *LinearityInferenceEngine, while_loop: struct {
        condition: *Expr,
        body: *Expr,
    }) !void {
        // Always count condition uses
        try self.countExprUses(while_loop.condition.*);

        // Check if loop is bounded (can be statically determined)
        const is_bounded = self.isLoopBounded(while_loop);

        if (!is_bounded) {
            // For unbounded loops, body uses don't count toward multiplicity
            // They're implicitly @* borrowed
            self.in_unbounded_loop = true;
            self.loop_depth += 1;
        }

        // Count body uses (may be discarded if unbounded)
        try self.countExprUses(while_loop.body.*);

        if (!is_bounded) {
            self.loop_depth -= 1;
            if (self.loop_depth == 0) {
                self.in_unbounded_loop = false;
            }
        }
    }

    /// Heuristic: check if loop iteration count can be statically determined
    /// Conservative: only return true for obvious cases
    fn isLoopBounded(self: *LinearityInferenceEngine, while_loop: struct {
        condition: *Expr,
        body: *Expr,
    }) bool {
        _ = self;
        _ = while_loop;
        // For now, conservatively assume all loops are unbounded unless we can prove otherwise
        // TODO: implement bounds analysis for cases like:
        // - while i < 10 (with var i initialized to 0)
        // - for i in 0..10 (if we add traditional for loops)
        return false;
    }

    /// Count parameter uses in an expression
    fn countExprUses(self: *LinearityInferenceEngine, expr: Expr) !void {
        switch (expr) {
            .variable => |name| {
                if (self.parameter_uses.get(name)) |*count| {
                    // Only count if not in unbounded loop body
                    if (!self.in_unbounded_loop) {
                        count.* += 1;
                    }
                }
            },
            .function_call => |call| {
                // Each parameter passed to a function counts as 1 use
                for (call.args) |arg| {
                    try self.countExprUses(arg.*);
                }
            },
            .field_access => |access| {
                // Accessing a field of a parameter counts as 1 use
                try self.countExprUses(access.object.*);
            },
            .method_call => |method| {
                // Method call on a parameter counts as 1 use
                try self.countExprUses(method.object.*);
                for (method.args) |arg| {
                    try self.countExprUses(arg.*);
                }
            },
            .if_expr => |if_expr| {
                try self.countExprUses(if_expr.condition.*);
                try self.countExprUses(if_expr.then_branch.*);
                if (if_expr.else_branch) |else_branch| {
                    try self.countExprUses(else_branch.*);
                }
            },
            .match_expr => |match_expr| {
                // The match itself counts as 1 use of the matched value
                try self.countExprUses(match_expr.scrutinee.*);
                // Each arm counts uses in its body
                for (match_expr.arms) |arm| {
                    try self.countExprUses(arm.body.*);
                }
            },
            .binary_op => |binop| {
                try self.countExprUses(binop.left.*);
                try self.countExprUses(binop.right.*);
            },
            .unary_op => |unop| {
                try self.countExprUses(unop.operand.*);
            },
            .tuple_literal => |tuple| {
                for (tuple) |elem| {
                    try self.countExprUses(elem.*);
                }
            },
            .struct_literal => |lit| {
                for (lit.fields) |field| {
                    try self.countExprUses(field.value.*);
                }
            },
            .block_expr => |block| {
                for (block.statements) |stmt| {
                    try self.countStmtUses(stmt);
                }
                if (block.result) |result_expr| {
                    try self.countExprUses(result_expr.*);
                }
            },
            .unsafe_block => |block| {
                for (block.statements) |stmt| {
                    try self.countStmtUses(stmt);
                }
                if (block.result) |result_expr| {
                    try self.countExprUses(result_expr.*);
                }
            },
            .constructor_call => |call| {
                // Constructor calls with parameter arguments
                for (call.args) |arg| {
                    try self.countExprUses(arg.*);
                }
            },
            .tuple_index => |ti| {
                try self.countExprUses(ti.tuple.*);
            },
            .intrinsic_call => |call| {
                for (call.args) |arg| {
                    try self.countExprUses(arg.*);
                }
            },
            .async_expr => |ae| {
                try self.countExprUses(ae.body.*);
            },
            .spawn_expr => |se| {
                try self.countExprUses(se.body.*);
            },
            .select_expr => |se| {
                for (se.arms) |arm| {
                    try self.countExprUses(arm.expr.*);
                }
                if (se.default_arm) |def| {
                    try self.countExprUses(def.*);
                }
            },
            else => {
                // Literals, dependent type refs, etc. don't directly use parameters
            },
        }
    }

    /// Convert use count to UsageAnnotation
    fn usageFromCount(self: *LinearityInferenceEngine, count: u32) !UsageAnnotation {
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
