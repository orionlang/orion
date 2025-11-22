const std = @import("std");
const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenKind = lexer.TokenKind;

pub const UsageAnnotation = union(enum) {
    once,
    exactly: u32,
    optional,
    unlimited,

    pub fn allowsZeroUses(self: UsageAnnotation) bool {
        return switch (self) {
            .optional, .unlimited => true,
            .once, .exactly => false,
        };
    }

    pub fn isValid(self: UsageAnnotation, use_count: u32) bool {
        return switch (self) {
            .once => use_count == 1,
            .exactly => |n| use_count == n,
            .optional => use_count == 0 or use_count == 1,
            .unlimited => true,
        };
    }
};

pub const PrimitiveType = enum {
    bool,
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    u64,
    ptr, // Opaque pointer type
    str, // String type (null-terminated C string, represented as ptr in LLVM)
    type, // Type value (for compile-time type information)

    pub fn isSigned(self: PrimitiveType) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64 => true,
            .bool, .u8, .u16, .u32, .u64, .ptr, .str, .type => false,
        };
    }

    pub fn bitWidth(self: PrimitiveType) u32 {
        return switch (self) {
            .bool => 1,
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32 => 32,
            .i64, .u64 => 64,
            .ptr => 64, // Pointer width (target-dependent, using 64 for now)
            .str => 64, // str is a pointer
            .type => 64, // Type represented as i64 at runtime
        };
    }

    pub fn isInteger(self: PrimitiveType) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => true,
            .bool, .ptr, .str, .type => false,
        };
    }

    pub fn llvmTypeName(self: PrimitiveType) []const u8 {
        return switch (self) {
            .bool => "i1",
            .i8, .u8 => "i8",
            .i16, .u16 => "i16",
            .i32, .u32 => "i32",
            .i64, .u64 => "i64",
            .ptr => "ptr",
            .str => "ptr", // str is represented as ptr in LLVM
            .type => "i64", // Type represented as i64 at runtime
        };
    }

    pub fn minValue(self: PrimitiveType) i64 {
        if (!self.isInteger()) {
            if (self == .bool) return 0;
            return 0;
        }
        if (!self.isSigned()) return 0; // Unsigned types have min of 0

        // Signed types: -2^(bits-1)
        return switch (self) {
            .i8 => -128,
            .i16 => -32768,
            .i32 => -2147483648,
            .i64 => std.math.minInt(i64),
            else => 0,
        };
    }

    pub fn maxValue(self: PrimitiveType) i64 {
        return switch (self) {
            .bool => 1,
            .i8 => 127,
            .i16 => 32767,
            .i32 => 2147483647,
            .i64 => std.math.maxInt(i64),
            .u8 => 255,
            .u16 => 65535,
            .u32 => 4294967295,
            .u64 => std.math.maxInt(i64), // Limited by i64 storage
            .ptr => 0, // Pointers don't have meaningful numeric ranges
            .str => 0, // Strings don't have meaningful numeric ranges
            .type => 9, // Type IDs range from 1-9
        };
    }
};

pub const StructField = struct {
    name: []const u8,
    field_type: *Type,
};

pub const SumTypeVariant = struct {
    name: []const u8,
    payload_types: []*Type,
};

pub const TypeParam = union(enum) {
    variable: []const u8,
    concrete: *Type,
};

pub const DependentType = struct {
    base: []const u8,
    type_params: []TypeParam,
    value_params: []Expr,
};

pub const Type = struct {
    kind: TypeKind,
    usage: UsageAnnotation,

    pub const TypeKind = union(enum) {
        primitive: PrimitiveType,
        tuple: []*Type,
        struct_type: []StructField,
        sum_type: []SumTypeVariant,
        named: []const u8,
        dependent: DependentType,
    };

    pub fn deinit(self: *Type, allocator: std.mem.Allocator) void {
        switch (self.kind) {
            .primitive => {},
            .tuple => |elements| {
                // Recursively free nested tuples
                for (elements) |elem| {
                    elem.deinit(allocator);
                    allocator.destroy(elem);
                }
                allocator.free(elements);
            },
            .struct_type => |fields| {
                for (fields) |field| {
                    field.field_type.deinit(allocator);
                    allocator.destroy(field.field_type);
                }
                allocator.free(fields);
            },
            .sum_type => |variants| {
                for (variants) |variant| {
                    for (variant.payload_types) |payload_type| {
                        payload_type.deinit(allocator);
                        allocator.destroy(payload_type);
                    }
                    allocator.free(variant.payload_types);
                }
                allocator.free(variants);
            },
            .named => {},
            .dependent => |dep| {
                for (dep.type_params) |type_param| {
                    if (type_param == .concrete) {
                        type_param.concrete.deinit(allocator);
                        allocator.destroy(type_param.concrete);
                    }
                }
                allocator.free(dep.type_params);
                // Note: value_params (Expr) are freed elsewhere in AST cleanup
                allocator.free(dep.value_params);
            },
        }
    }

    pub fn deinitShallow(self: *Type, allocator: std.mem.Allocator) void {
        switch (self.kind) {
            .primitive => {},
            .tuple => |elements| {
                // Only free element pointers and array, not recursive deinit
                // Used when nested tuples are tracked separately
                for (elements) |elem| {
                    allocator.destroy(elem);
                }
                allocator.free(elements);
            },
            .struct_type => |fields| {
                for (fields) |field| {
                    allocator.destroy(field.field_type);
                }
                allocator.free(fields);
            },
            .sum_type => |variants| {
                for (variants) |variant| {
                    for (variant.payload_types) |payload_type| {
                        allocator.destroy(payload_type);
                    }
                    allocator.free(variant.payload_types);
                }
                allocator.free(variants);
            },
            .named => {},
            .dependent => |dep| {
                for (dep.type_params) |type_param| {
                    if (type_param == .concrete) {
                        allocator.destroy(type_param.concrete);
                    }
                }
                allocator.free(dep.type_params);
                allocator.free(dep.value_params);
            },
        }
    }

    pub fn isSigned(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p.isSigned(),
            .tuple, .struct_type, .sum_type, .named, .dependent => false,
        };
    }

    pub fn bitWidth(self: Type) u32 {
        return switch (self.kind) {
            .primitive => |p| p.bitWidth(),
            .tuple, .struct_type, .sum_type, .named, .dependent => 0,
        };
    }

    pub fn isInteger(self: Type) bool {
        return switch (self.kind) {
            .primitive => |p| p.isInteger(),
            .tuple, .struct_type, .sum_type, .named, .dependent => false,
        };
    }

    pub fn llvmTypeName(self: Type) []const u8 {
        return switch (self.kind) {
            .primitive => |p| p.llvmTypeName(),
            .tuple, .struct_type, .sum_type, .named, .dependent => unreachable,
        };
    }

    pub fn minValue(self: Type) i64 {
        return switch (self.kind) {
            .primitive => |p| p.minValue(),
            .tuple, .struct_type, .sum_type, .named, .dependent => 0,
        };
    }

    pub fn maxValue(self: Type) i64 {
        return switch (self.kind) {
            .primitive => |p| p.maxValue(),
            .tuple, .struct_type, .sum_type, .named, .dependent => 0,
        };
    }

    pub fn eql(self: Type, other: Type) bool {
        if (@as(@typeInfo(TypeKind).@"union".tag_type.?, self.kind) != @as(@typeInfo(TypeKind).@"union".tag_type.?, other.kind)) {
            return false;
        }
        return switch (self.kind) {
            .primitive => |p| p == other.kind.primitive,
            .tuple => |elems| {
                if (elems.len != other.kind.tuple.len) return false;
                for (elems, other.kind.tuple) |a, b| {
                    if (!a.eql(b.*)) return false;
                }
                return true;
            },
            .struct_type => |fields| {
                if (fields.len != other.kind.struct_type.len) return false;
                for (fields, other.kind.struct_type) |a, b| {
                    if (!std.mem.eql(u8, a.name, b.name)) return false;
                    if (!a.field_type.eql(b.field_type.*)) return false;
                }
                return true;
            },
            .sum_type => |variants| {
                if (variants.len != other.kind.sum_type.len) return false;
                for (variants, other.kind.sum_type) |a, b| {
                    if (!std.mem.eql(u8, a.name, b.name)) return false;
                    if (a.payload_types.len != b.payload_types.len) return false;
                    for (a.payload_types, b.payload_types) |a_type, b_type| {
                        if (!a_type.eql(b_type.*)) return false;
                    }
                }
                return true;
            },
            .named => |name| std.mem.eql(u8, name, other.kind.named),
            .dependent => |dep| {
                const other_dep = other.kind.dependent;
                if (!std.mem.eql(u8, dep.base, other_dep.base)) return false;
                if (dep.type_params.len != other_dep.type_params.len) return false;
                if (dep.value_params.len != other_dep.value_params.len) return false;

                for (dep.type_params, other_dep.type_params) |a, b| {
                    if (@as(@typeInfo(TypeParam).@"union".tag_type.?, a) != @as(@typeInfo(TypeParam).@"union".tag_type.?, b)) return false;
                    switch (a) {
                        .variable => |v| if (!std.mem.eql(u8, v, b.variable)) return false,
                        .concrete => |c| if (!c.eql(b.concrete.*)) return false,
                    }
                }

                // Note: value_params contain Expr which doesn't have eql yet
                // For now, we'll skip comparing value params in eql
                // This will need to be implemented when we add expression equality
                return true;
            },
        };
    }
};

pub const ParseError = error{
    UnexpectedEndOfFile,
    UnexpectedToken,
    ExpectedExpression,
    UnknownType,
    ExpectedStatement,
    OutOfMemory,
    InvalidCharacter,
    Overflow,
};

pub const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,
    equal,
    not_equal,
    less_than,
    greater_than,
    less_equal,
    greater_equal,
    logical_and,
    logical_or,
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    shift_left,
    shift_right,

    pub fn returns_bool(self: BinaryOp) bool {
        return switch (self) {
            .equal, .not_equal, .less_than, .greater_than, .less_equal, .greater_equal, .logical_and, .logical_or => true,
            else => false,
        };
    }
};

pub const UnaryOp = enum {
    negate,
    logical_not,
    bitwise_not,

    pub fn returns_bool(self: UnaryOp) bool {
        return self == .logical_not;
    }
};

pub const StructLiteralField = struct {
    name: []const u8,
    value: *Expr,
};

pub const MatchArm = struct {
    pattern: MatchPattern,
    body: *Expr,
};

pub const MatchPattern = union(enum) {
    identifier: []const u8,
    constructor: struct {
        name: []const u8,
        bindings: [][]const u8,
    },
};

pub const Expr = union(enum) {
    integer_literal: struct {
        value: i64,
        inferred_type: Type,
    },
    bool_literal: bool,
    string_literal: []const u8, // Includes quotes, e.g. "hello"
    variable: []const u8,
    binary_op: struct {
        op: BinaryOp,
        left: *Expr,
        right: *Expr,
    },
    unary_op: struct {
        op: UnaryOp,
        operand: *Expr,
    },
    function_call: struct {
        name: []const u8,
        args: []*Expr,
    },
    if_expr: struct {
        condition: *Expr,
        then_branch: *Expr,
        else_branch: ?*Expr,
    },
    block_expr: struct {
        statements: []Stmt,
        result: ?*Expr,
    },
    unsafe_block: struct {
        statements: []Stmt,
        result: ?*Expr,
    },
    tuple_literal: []*Expr,
    tuple_index: struct {
        tuple: *Expr,
        index: usize,
    },
    struct_literal: struct {
        type_name: []const u8,
        fields: []StructLiteralField,
    },
    field_access: struct {
        object: *Expr,
        field_name: []const u8,
    },
    constructor_call: struct {
        name: []const u8,
        args: []*Expr,
    },
    match_expr: struct {
        scrutinee: *Expr,
        arms: []MatchArm,
    },
    method_call: struct {
        object: *Expr,
        method_name: []const u8,
        args: []*Expr,
    },
    intrinsic_call: struct {
        name: []const u8,
        args: []*Expr,
    },
};

pub const Param = struct {
    name: []const u8,
    param_type: Type,
};

pub const Pattern = union(enum) {
    identifier: []const u8,
    tuple: []*Pattern,

    pub fn deinit(self: *Pattern, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .identifier => {},
            .tuple => |patterns| {
                for (patterns) |pat| {
                    pat.deinit(allocator);
                    allocator.destroy(pat);
                }
                allocator.free(patterns);
            },
        }
    }
};

pub const Stmt = union(enum) {
    let_binding: struct {
        pattern: Pattern,
        type_annotation: ?Type,
        value: *Expr,
        mutable: bool,
    },
    assignment: struct {
        name: []const u8,
        value: *Expr,
    },
    while_stmt: struct {
        condition: *Expr,
        body: *Expr,
    },
    return_stmt: *Expr,
    expr: *Expr,
};

pub const ImportItem = union(enum) {
    module: []const u8, // import foo
    specific: struct { // import foo::bar or import foo::{bar, baz}
        path: []const u8, // e.g., "foo"
        items: [][]const u8, // e.g., ["bar"] or ["bar", "baz"]
    },
};

pub const ImportDecl = struct {
    item: ImportItem,

    pub fn deinit(self: *ImportDecl, allocator: std.mem.Allocator) void {
        switch (self.item) {
            .module => |path| {
                allocator.free(path);
            },
            .specific => |spec| {
                allocator.free(spec.path);
                allocator.free(spec.items);
            },
        }
    }
};

pub const TypeParameter = struct {
    name: []const u8,
    kind: ParameterKind,
};

pub const ParameterKind = union(enum) {
    type_param,
    value_param: Type,
};

pub const TypeDef = struct {
    name: []const u8,
    params: []TypeParameter,
    type_value: Type,
    is_public: bool,

    pub fn deinit(self: *TypeDef, allocator: std.mem.Allocator) void {
        for (self.params) |param| {
            if (param.kind == .value_param) {
                var mutable_type = param.kind.value_param;
                mutable_type.deinit(allocator);
            }
        }
        allocator.free(self.params);
        self.type_value.deinit(allocator);
    }
};

pub const ClassMethod = struct {
    name: []const u8,
    self_param: bool, // true if first param is Self
    param_types: []Type,
    return_type: ?Type,
};

pub const ClassDef = struct {
    name: []const u8,
    methods: []ClassMethod,
    is_public: bool,

    pub fn deinit(self: *ClassDef, allocator: std.mem.Allocator) void {
        for (self.methods) |*method| {
            for (method.param_types) |*param_type| {
                param_type.deinit(allocator);
            }
            allocator.free(method.param_types);
            if (method.return_type) |*ret_type| {
                ret_type.deinit(allocator);
            }
        }
        allocator.free(self.methods);
    }
};

pub const MethodImpl = struct {
    name: []const u8,
    params: []Param,
    return_type: Type,
    body: std.ArrayList(Stmt),
};

pub const InstanceDecl = struct {
    class_name: []const u8,
    type_name: Type,
    methods: []MethodImpl,

    pub fn deinit(self: *InstanceDecl, allocator: std.mem.Allocator) void {
        self.type_name.deinit(allocator);
        for (self.methods) |*method| {
            for (method.params) |param| {
                var param_type_copy = param.param_type;
                param_type_copy.deinit(allocator);
            }
            allocator.free(method.params);
            method.return_type.deinit(allocator);
            for (method.body.items) |*stmt| {
                FunctionDecl.deinitStmt(allocator, stmt);
            }
            method.body.deinit(allocator);
        }
        allocator.free(self.methods);
    }
};

pub const FunctionDecl = struct {
    name: []const u8,
    params: []Param,
    return_type: Type,
    body: std.ArrayList(Stmt),
    is_unsafe: bool,
    is_public: bool,

    pub fn deinit(self: *FunctionDecl, allocator: std.mem.Allocator) void {
        for (self.params) |param| {
            var param_type_copy = param.param_type;
            param_type_copy.deinit(allocator);
        }
        allocator.free(self.params);
        self.return_type.deinit(allocator);
        for (self.body.items) |*stmt| {
            deinitStmt(allocator, stmt);
        }
        self.body.deinit(allocator);
    }

    fn deinitStmt(allocator: std.mem.Allocator, stmt: *Stmt) void {
        switch (stmt.*) {
            .let_binding => |*binding| {
                binding.pattern.deinit(allocator);
                if (binding.type_annotation) |*typ| {
                    typ.deinit(allocator);
                }
                deinitExpr(allocator, binding.value);
                allocator.destroy(binding.value);
            },
            .assignment => |assign| {
                deinitExpr(allocator, assign.value);
                allocator.destroy(assign.value);
            },
            .while_stmt => |while_stmt| {
                deinitExpr(allocator, while_stmt.condition);
                allocator.destroy(while_stmt.condition);
                deinitExpr(allocator, while_stmt.body);
                allocator.destroy(while_stmt.body);
            },
            .return_stmt => |expr| {
                deinitExpr(allocator, expr);
                allocator.destroy(expr);
            },
            .expr => |expr| {
                deinitExpr(allocator, expr);
                allocator.destroy(expr);
            },
        }
    }

    fn deinitExpr(allocator: std.mem.Allocator, expr: *Expr) void {
        switch (expr.*) {
            .integer_literal => |*lit| {
                lit.inferred_type.deinit(allocator);
            },
            .bool_literal => {},
            .string_literal => {},
            .variable => {},
            .binary_op => |binop| {
                deinitExpr(allocator, binop.left);
                allocator.destroy(binop.left);
                deinitExpr(allocator, binop.right);
                allocator.destroy(binop.right);
            },
            .unary_op => |unop| {
                deinitExpr(allocator, unop.operand);
                allocator.destroy(unop.operand);
            },
            .function_call => |call| {
                for (call.args) |arg| {
                    deinitExpr(allocator, arg);
                    allocator.destroy(arg);
                }
                allocator.free(call.args);
            },
            .if_expr => |if_expr| {
                deinitExpr(allocator, if_expr.condition);
                allocator.destroy(if_expr.condition);
                deinitExpr(allocator, if_expr.then_branch);
                allocator.destroy(if_expr.then_branch);
                if (if_expr.else_branch) |else_branch| {
                    deinitExpr(allocator, else_branch);
                    allocator.destroy(else_branch);
                }
            },
            .block_expr => |block| {
                for (block.statements) |*stmt| {
                    deinitStmt(allocator, stmt);
                }
                allocator.free(block.statements);
                if (block.result) |result| {
                    deinitExpr(allocator, result);
                    allocator.destroy(result);
                }
            },
            .unsafe_block => |block| {
                for (block.statements) |*stmt| {
                    deinitStmt(allocator, stmt);
                }
                allocator.free(block.statements);
                if (block.result) |result| {
                    deinitExpr(allocator, result);
                    allocator.destroy(result);
                }
            },
            .tuple_literal => |elements| {
                for (elements) |elem| {
                    deinitExpr(allocator, elem);
                    allocator.destroy(elem);
                }
                allocator.free(elements);
            },
            .tuple_index => |idx| {
                deinitExpr(allocator, idx.tuple);
                allocator.destroy(idx.tuple);
            },
            .struct_literal => |lit| {
                for (lit.fields) |field| {
                    deinitExpr(allocator, field.value);
                    allocator.destroy(field.value);
                }
                allocator.free(lit.fields);
            },
            .field_access => |access| {
                deinitExpr(allocator, access.object);
                allocator.destroy(access.object);
            },
            .constructor_call => |call| {
                for (call.args) |arg| {
                    deinitExpr(allocator, arg);
                    allocator.destroy(arg);
                }
                allocator.free(call.args);
            },
            .match_expr => |match_expr| {
                deinitExpr(allocator, match_expr.scrutinee);
                allocator.destroy(match_expr.scrutinee);
                for (match_expr.arms) |arm| {
                    switch (arm.pattern) {
                        .identifier => {},
                        .constructor => |constructor| {
                            allocator.free(constructor.bindings);
                        },
                    }
                    deinitExpr(allocator, arm.body);
                    allocator.destroy(arm.body);
                }
                allocator.free(match_expr.arms);
            },
            .method_call => |call| {
                deinitExpr(allocator, call.object);
                allocator.destroy(call.object);
                for (call.args) |arg| {
                    deinitExpr(allocator, arg);
                    allocator.destroy(arg);
                }
                allocator.free(call.args);
            },
            .intrinsic_call => |call| {
                for (call.args) |arg| {
                    deinitExpr(allocator, arg);
                    allocator.destroy(arg);
                }
                allocator.free(call.args);
            },
        }
    }
};

pub const AST = struct {
    imports: std.ArrayList(ImportDecl),
    type_defs: std.ArrayList(TypeDef),
    class_defs: std.ArrayList(ClassDef),
    instances: std.ArrayList(InstanceDecl),
    functions: std.ArrayList(FunctionDecl),

    pub fn deinit(self: *AST, allocator: std.mem.Allocator) void {
        for (self.imports.items) |*import_decl| {
            import_decl.deinit(allocator);
        }
        self.imports.deinit(allocator);
        for (self.type_defs.items) |*typedef| {
            typedef.deinit(allocator);
        }
        self.type_defs.deinit(allocator);
        for (self.class_defs.items) |*classdef| {
            classdef.deinit(allocator);
        }
        self.class_defs.deinit(allocator);
        for (self.instances.items) |*instance| {
            instance.deinit(allocator);
        }
        self.instances.deinit(allocator);
        for (self.functions.items) |*func| {
            func.deinit(allocator);
        }
        self.functions.deinit(allocator);
    }
};

pub const Parser = struct {
    tokens: []const Token,
    pos: usize,
    allocator: std.mem.Allocator,

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Parser) !AST {
        var imports: std.ArrayList(ImportDecl) = .empty;
        errdefer imports.deinit(self.allocator);
        var type_defs: std.ArrayList(TypeDef) = .empty;
        errdefer type_defs.deinit(self.allocator);
        var class_defs: std.ArrayList(ClassDef) = .empty;
        errdefer class_defs.deinit(self.allocator);
        var instances: std.ArrayList(InstanceDecl) = .empty;
        errdefer instances.deinit(self.allocator);
        var functions: std.ArrayList(FunctionDecl) = .empty;
        errdefer functions.deinit(self.allocator);

        while (!self.isAtEnd()) {
            // Check for optional pub keyword
            const is_public = if (self.check(.pub_keyword)) blk: {
                _ = try self.expect(.pub_keyword);
                break :blk true;
            } else false;

            if (self.check(.import_keyword)) {
                const import_decl = try self.parseImport();
                try imports.append(self.allocator, import_decl);
            } else if (self.check(.type_keyword)) {
                const typedef = try self.parseTypeDef(is_public);
                try type_defs.append(self.allocator, typedef);
            } else if (self.check(.class_keyword)) {
                const classdef = try self.parseClassDef(is_public);
                try class_defs.append(self.allocator, classdef);
            } else if (self.check(.instance_keyword)) {
                const instance = try self.parseInstanceDecl();
                try instances.append(self.allocator, instance);
            } else if (self.check(.unsafe_keyword) or self.check(.fn_keyword)) {
                const func = try self.parseFunction(is_public);
                try functions.append(self.allocator, func);
            } else {
                return error.UnexpectedToken;
            }
        }

        return AST{
            .imports = imports,
            .type_defs = type_defs,
            .class_defs = class_defs,
            .instances = instances,
            .functions = functions,
        };
    }

    fn parseFunction(self: *Parser, is_public: bool) !FunctionDecl {
        // Check for optional unsafe keyword
        const is_unsafe = if (self.check(.unsafe_keyword)) blk: {
            _ = try self.expect(.unsafe_keyword);
            break :blk true;
        } else false;

        _ = try self.expect(.fn_keyword);

        const name_token = try self.expect(.identifier);
        const name = name_token.lexeme;

        _ = try self.expect(.left_paren);

        // Parse parameters: param_name: Type, param_name: Type, ...
        var params: std.ArrayList(Param) = .empty;
        errdefer params.deinit(self.allocator);

        if (!self.check(.right_paren)) {
            while (true) {
                const param_name = try self.expect(.identifier);
                _ = try self.expect(.colon);
                const param_type = try self.parseType();

                try params.append(self.allocator, Param{
                    .name = param_name.lexeme,
                    .param_type = param_type,
                });

                if (!self.check(.comma)) break;
                _ = try self.expect(.comma);
            }
        }

        _ = try self.expect(.right_paren);

        const return_type = try self.parseType();

        const body = try self.parseBlockStatements();

        return FunctionDecl{
            .name = name,
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .body = body,
            .is_unsafe = is_unsafe,
            .is_public = is_public,
        };
    }

    fn parseBlockStatements(self: *Parser) !std.ArrayList(Stmt) {
        _ = try self.expect(.left_brace);
        var body: std.ArrayList(Stmt) = .empty;
        errdefer body.deinit(self.allocator);

        while (!self.check(.right_brace)) {
            const stmt = try self.parseStatement();
            try body.append(self.allocator, stmt);
        }

        _ = try self.expect(.right_brace);
        return body;
    }

    fn parseImport(self: *Parser) !ImportDecl {
        _ = try self.expect(.import_keyword);

        // Parse module path (e.g., "foo" or "foo.bar")
        // We need to distinguish:
        // - import foo       -> module import
        // - import foo.bar   -> could be module foo.bar OR module foo with item bar
        // - import foo.{a,b} -> module foo with items a,b

        const first_part = try self.expect(.identifier);

        // Check if there's a dot after the first identifier
        if (!self.check(.dot)) {
            // Just: import foo
            const module_path = try self.allocator.dupe(u8, first_part.lexeme);
            return ImportDecl{
                .item = .{ .module = module_path },
            };
        }

        _ = try self.expect(.dot);

        // Check for brace list: import foo.{bar, baz}
        if (self.check(.left_brace)) {
            _ = try self.expect(.left_brace);
            var items: std.ArrayList([]const u8) = .empty;
            errdefer items.deinit(self.allocator);

            while (!self.check(.right_brace)) {
                const item = try self.expect(.identifier);
                try items.append(self.allocator, item.lexeme);

                if (self.check(.comma)) {
                    _ = try self.expect(.comma);
                } else {
                    break;
                }
            }

            _ = try self.expect(.right_brace);

            const path = try self.allocator.dupe(u8, first_part.lexeme);
            return ImportDecl{
                .item = .{
                    .specific = .{
                        .path = path,
                        .items = try items.toOwnedSlice(self.allocator),
                    },
                },
            };
        }

        // Parse rest of path: foo.bar.baz or foo.bar
        var path_buf: std.ArrayList(u8) = .empty;
        defer path_buf.deinit(self.allocator);

        try path_buf.appendSlice(self.allocator, first_part.lexeme);
        try path_buf.appendSlice(self.allocator, ".");

        const second_part = try self.expect(.identifier);
        try path_buf.appendSlice(self.allocator, second_part.lexeme);

        // Continue parsing module path
        while (self.check(.dot)) {
            // Peek ahead to see if it's a brace list
            if (self.peekAhead(1).kind == .left_brace) {
                // This is: import foo.bar.{items}
                _ = try self.expect(.dot);
                _ = try self.expect(.left_brace);

                var items: std.ArrayList([]const u8) = .empty;
                errdefer items.deinit(self.allocator);

                while (!self.check(.right_brace)) {
                    const item = try self.expect(.identifier);
                    try items.append(self.allocator, item.lexeme);

                    if (self.check(.comma)) {
                        _ = try self.expect(.comma);
                    } else {
                        break;
                    }
                }

                _ = try self.expect(.right_brace);

                const module_path = try self.allocator.dupe(u8, path_buf.items);
                return ImportDecl{
                    .item = .{
                        .specific = .{
                            .path = module_path,
                            .items = try items.toOwnedSlice(self.allocator),
                        },
                    },
                };
            }

            // Continue building module path
            _ = try self.expect(.dot);
            const next_part = try self.expect(.identifier);
            try path_buf.appendSlice(self.allocator, ".");
            try path_buf.appendSlice(self.allocator, next_part.lexeme);
        }

        // Module import: import foo.bar.baz
        const module_path = try self.allocator.dupe(u8, path_buf.items);
        return ImportDecl{
            .item = .{ .module = module_path },
        };
    }

    fn parseTypeDef(self: *Parser, is_public: bool) !TypeDef {
        _ = try self.expect(.type_keyword);
        const name_token = try self.expect(.identifier);

        // Parse optional type parameters: type Vec[A, n: Nat] = ...
        var params = std.ArrayList(TypeParameter).empty;
        errdefer {
            for (params.items) |param| {
                if (param.kind == .value_param) {
                    var mutable_type = param.kind.value_param;
                    mutable_type.deinit(self.allocator);
                }
            }
            params.deinit(self.allocator);
        }

        if (self.check(.left_bracket)) {
            _ = try self.expect(.left_bracket);

            // Parse first parameter
            const first_param = try self.parseTypeParameter();
            try params.append(self.allocator, first_param);

            // Parse additional parameters
            while (self.check(.comma)) {
                _ = try self.expect(.comma);
                const param = try self.parseTypeParameter();
                try params.append(self.allocator, param);
            }

            _ = try self.expect(.right_bracket);
        }

        _ = try self.expect(.equal);
        const type_value = try self.parseType();

        return TypeDef{
            .name = name_token.lexeme,
            .params = try params.toOwnedSlice(self.allocator),
            .type_value = type_value,
            .is_public = is_public,
        };
    }

    fn parseTypeParameter(self: *Parser) !TypeParameter {
        const param_name = try self.expect(.identifier);

        // Check if this is a value parameter: name: Type
        if (self.check(.colon)) {
            _ = try self.expect(.colon);
            const param_type = try self.parseType();
            return TypeParameter{
                .name = param_name.lexeme,
                .kind = .{ .value_param = param_type },
            };
        }

        // Otherwise it's a type parameter
        return TypeParameter{
            .name = param_name.lexeme,
            .kind = .type_param,
        };
    }

    const type_name_map = std.StaticStringMap(PrimitiveType).initComptime(.{
        .{ "Bool", .bool },
        .{ "I8", .i8 },
        .{ "I16", .i16 },
        .{ "I32", .i32 },
        .{ "I64", .i64 },
        .{ "ptr", .ptr },
        .{ "str", .str },
        .{ "Type", .type },
        .{ "U8", .u8 },
        .{ "U16", .u16 },
        .{ "U32", .u32 },
        .{ "U64", .u64 },
    });

    fn parseUsageAnnotation(self: *Parser) !UsageAnnotation {
        if (!self.check(.at)) {
            return .once; // Default
        }
        _ = try self.expect(.at);

        if (self.check(.star)) {
            _ = try self.expect(.star);
            return .unlimited;
        }

        if (self.check(.question)) {
            _ = try self.expect(.question);
            return .optional;
        }

        if (self.check(.integer)) {
            const num_token = try self.expect(.integer);
            const n = try std.fmt.parseInt(u32, num_token.lexeme, 10);
            return .{ .exactly = n };
        }

        return error.UnexpectedToken;
    }

    fn parseType(self: *Parser) ParseError!Type {
        if (self.check(.pipe)) {
            var variants: std.ArrayList(SumTypeVariant) = .empty;
            errdefer {
                for (variants.items) |variant| {
                    for (variant.payload_types) |payload_type| {
                        payload_type.deinit(self.allocator);
                        self.allocator.destroy(payload_type);
                    }
                    self.allocator.free(variant.payload_types);
                }
                variants.deinit(self.allocator);
            }

            while (self.check(.pipe)) {
                _ = try self.expect(.pipe);
                const variant_name = try self.expect(.identifier);

                var payload_types: std.ArrayList(*Type) = .empty;
                errdefer {
                    for (payload_types.items) |payload_type| {
                        payload_type.deinit(self.allocator);
                        self.allocator.destroy(payload_type);
                    }
                    payload_types.deinit(self.allocator);
                }

                if (self.check(.left_paren)) {
                    _ = try self.expect(.left_paren);
                    const first_type = try self.allocator.create(Type);
                    first_type.* = try self.parseType();
                    try payload_types.append(self.allocator, first_type);

                    while (self.check(.comma)) {
                        _ = try self.expect(.comma);
                        const payload_type = try self.allocator.create(Type);
                        payload_type.* = try self.parseType();
                        try payload_types.append(self.allocator, payload_type);
                    }

                    _ = try self.expect(.right_paren);
                }

                try variants.append(self.allocator, SumTypeVariant{
                    .name = variant_name.lexeme,
                    .payload_types = try payload_types.toOwnedSlice(self.allocator),
                });
            }

            const usage = try self.parseUsageAnnotation();
            return Type{ .kind = .{ .sum_type = try variants.toOwnedSlice(self.allocator) }, .usage = usage };
        }

        if (self.check(.left_paren)) {
            _ = try self.expect(.left_paren);

            var element_types: std.ArrayList(*Type) = .empty;
            errdefer {
                for (element_types.items) |elem| {
                    elem.deinit(self.allocator);
                    self.allocator.destroy(elem);
                }
                element_types.deinit(self.allocator);
            }

            if (self.check(.right_paren)) {
                _ = try self.expect(.right_paren);
                const usage = try self.parseUsageAnnotation();
                return Type{ .kind = .{ .tuple = try element_types.toOwnedSlice(self.allocator) }, .usage = usage };
            }

            const first_elem = try self.allocator.create(Type);
            first_elem.* = try self.parseType();
            try element_types.append(self.allocator, first_elem);

            while (self.check(.comma)) {
                _ = try self.expect(.comma);
                const elem = try self.allocator.create(Type);
                elem.* = try self.parseType();
                try element_types.append(self.allocator, elem);
            }

            _ = try self.expect(.right_paren);
            const usage = try self.parseUsageAnnotation();
            return Type{ .kind = .{ .tuple = try element_types.toOwnedSlice(self.allocator)  }, .usage = usage };
        }

        if (self.check(.left_brace)) {
            _ = try self.expect(.left_brace);

            var fields: std.ArrayList(StructField) = .empty;
            errdefer {
                for (fields.items) |field| {
                    field.field_type.deinit(self.allocator);
                    self.allocator.destroy(field.field_type);
                }
                fields.deinit(self.allocator);
            }

            while (!self.check(.right_brace)) {
                const field_name = try self.expect(.identifier);
                _ = try self.expect(.colon);
                const field_type_ptr = try self.allocator.create(Type);
                field_type_ptr.* = try self.parseType();

                try fields.append(self.allocator, StructField{
                    .name = field_name.lexeme,
                    .field_type = field_type_ptr,
                });

                if (self.check(.comma)) {
                    _ = try self.expect(.comma);
                }
            }

            _ = try self.expect(.right_brace);
            const usage = try self.parseUsageAnnotation();
            return Type{ .kind = .{ .struct_type = try fields.toOwnedSlice(self.allocator)  }, .usage = usage };
        }

        const type_token = try self.expect(.identifier);

        // Check for dependent type syntax: TypeName[params...]
        if (self.check(.left_bracket)) {
            _ = try self.expect(.left_bracket);

            var type_params = std.ArrayList(TypeParam).empty;
            var value_params = std.ArrayList(Expr).empty;
            errdefer {
                for (type_params.items) |type_param| {
                    if (type_param == .concrete) {
                        type_param.concrete.deinit(self.allocator);
                        self.allocator.destroy(type_param.concrete);
                    }
                }
                type_params.deinit(self.allocator);
                value_params.deinit(self.allocator);
            }

            // Parse first parameter
            try self.parseDependentTypeParameter(&type_params, &value_params);

            // Parse additional parameters
            while (self.check(.comma)) {
                _ = try self.expect(.comma);
                try self.parseDependentTypeParameter(&type_params, &value_params);
            }

            _ = try self.expect(.right_bracket);
            const usage = try self.parseUsageAnnotation();

            return Type{
                .kind = .{
                    .dependent = DependentType{
                        .base = type_token.lexeme,
                        .type_params = try type_params.toOwnedSlice(self.allocator),
                        .value_params = try value_params.toOwnedSlice(self.allocator),
                    },
                },
                .usage = usage,
            };
        }

        const usage = try self.parseUsageAnnotation();
        if (type_name_map.get(type_token.lexeme)) |prim| {
            return Type{ .kind = .{ .primitive = prim  }, .usage = usage };
        } else {
            return Type{ .kind = .{ .named = type_token.lexeme  }, .usage = usage };
        }
    }

    fn parseDependentTypeParameter(self: *Parser, type_params: *std.ArrayList(TypeParam), value_params: *std.ArrayList(Expr)) !void {
        // Heuristic: if it starts with identifier and isn't followed by operator, it's likely a type param
        // If it's a number or has operators, it's a value param
        if (self.check(.identifier)) {
            const saved_pos = self.pos;
            const ident = try self.expect(.identifier);

            // Check if this looks like a type (primitive or type variable)
            // If followed by comma, ], or is a known primitive, treat as type
            if (self.check(.comma) or self.check(.right_bracket) or type_name_map.has(ident.lexeme)) {
                // It's a type parameter
                if (type_name_map.get(ident.lexeme)) |prim| {
                    // Concrete primitive type
                    const type_ptr = try self.allocator.create(Type);
                    type_ptr.* = Type{ .kind = .{ .primitive = prim }, .usage = .once };
                    try type_params.append(self.allocator, .{ .concrete = type_ptr });
                } else {
                    // Type variable
                    try type_params.append(self.allocator, .{ .variable = ident.lexeme });
                }
                return;
            }

            // Otherwise, it's part of a value expression - backtrack and parse as expression
            self.pos = saved_pos;
        }

        // Parse as value expression
        const expr = try self.parseExpression();
        try value_params.append(self.allocator, expr);
    }

    fn skipOptionalSemicolon(self: *Parser) void {
        if (self.check(.semicolon)) {
            _ = self.expect(.semicolon) catch {};
        }
    }

    fn isStatementStart(self: *Parser) bool {
        return self.check(.let_keyword) or
               self.check(.var_keyword) or
               self.check(.return_keyword) or
               self.check(.while_keyword) or
               (self.check(.identifier) and self.peekAhead(1).kind == .equal);
    }

    fn parseStatement(self: *Parser) !Stmt {
        if (self.check(.let_keyword)) {
            return try self.parseLetBinding(false);
        }
        if (self.check(.var_keyword)) {
            return try self.parseLetBinding(true);
        }
        if (self.check(.return_keyword)) {
            return try self.parseReturnStatement();
        }
        if (self.check(.while_keyword)) {
            return try self.parseWhileStatement();
        }
        // Check for assignment: identifier = expr;
        if (self.check(.identifier) and self.peekAhead(1).kind == .equal) {
            return try self.parseAssignment();
        }
        // Expression statement (for side effects)
        const expr = try self.parseExpression();
        self.skipOptionalSemicolon();
        const expr_ptr = try self.allocator.create(Expr);
        expr_ptr.* = expr;
        return Stmt{ .expr = expr_ptr };
    }

    fn parseAssignment(self: *Parser) !Stmt {
        const name_token = try self.expect(.identifier);
        _ = try self.expect(.equal);

        const value_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(value_ptr);
        value_ptr.* = try self.parseExpression();

        self.skipOptionalSemicolon();

        return .{ .assignment = .{
            .name = name_token.lexeme,
            .value = value_ptr,
        } };
    }

    fn parseLetBinding(self: *Parser, mutable: bool) !Stmt {
        if (mutable) {
            _ = try self.expect(.var_keyword);
        } else {
            _ = try self.expect(.let_keyword);
        }

        var pattern = try self.parsePattern();
        errdefer pattern.deinit(self.allocator);

        var type_annotation: ?Type = null;
        if (self.check(.colon)) {
            _ = try self.expect(.colon);
            type_annotation = try self.parseType();
        }

        _ = try self.expect(.equal);

        const value_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(value_ptr);
        value_ptr.* = try self.parseExpression();

        self.skipOptionalSemicolon();

        return .{ .let_binding = .{
            .pattern = pattern,
            .type_annotation = type_annotation,
            .value = value_ptr,
            .mutable = mutable,
        } };
    }

    fn parsePattern(self: *Parser) !Pattern {
        if (self.check(.left_paren)) {
            _ = try self.expect(.left_paren);

            var patterns: std.ArrayList(*Pattern) = .empty;
            errdefer {
                for (patterns.items) |pat| {
                    pat.deinit(self.allocator);
                    self.allocator.destroy(pat);
                }
                patterns.deinit(self.allocator);
            }

            const first_pat = try self.allocator.create(Pattern);
            first_pat.* = try self.parsePattern();
            try patterns.append(self.allocator, first_pat);

            while (self.check(.comma)) {
                _ = try self.expect(.comma);
                const pat = try self.allocator.create(Pattern);
                pat.* = try self.parsePattern();
                try patterns.append(self.allocator, pat);
            }

            _ = try self.expect(.right_paren);
            return Pattern{ .tuple = try patterns.toOwnedSlice(self.allocator) };
        }

        const name_token = try self.expect(.identifier);
        return Pattern{ .identifier = name_token.lexeme };
    }

    fn parseReturnStatement(self: *Parser) !Stmt {
        _ = try self.expect(.return_keyword);
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpression();
        return .{ .return_stmt = expr };
    }

    fn parseWhileStatement(self: *Parser) !Stmt {
        _ = try self.expect(.while_keyword);

        // Parse condition
        const condition = try self.parseExpressionPtr(&.{});

        // Parse body (must be block expression)
        const body = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(body);
        body.* = try self.parseBlockExpression();

        self.skipOptionalSemicolon();

        return .{
            .while_stmt = .{
                .condition = condition,
                .body = body,
            },
        };
    }

    fn parseExpression(self: *Parser) ParseError!Expr {
        return try self.parseLogicalOr();
    }

    fn makeBinaryOp(self: *Parser, op: BinaryOp, left: Expr, right: Expr) ParseError!Expr {
        const left_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(left_ptr);
        left_ptr.* = left;

        const right_ptr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(right_ptr);
        right_ptr.* = right;

        return .{ .binary_op = .{ .op = op, .left = left_ptr, .right = right_ptr } };
    }

    // Precedence climbing parser
    fn parseLogicalOr(self: *Parser) ParseError!Expr {
        var left = try self.parseLogicalAnd();

        while (self.check(.pipe_pipe)) {
            _ = try self.expect(.pipe_pipe);
            const right = try self.parseLogicalAnd();
            left = try self.makeBinaryOp(.logical_or, left, right);
        }

        return left;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!Expr {
        var left = try self.parseBitwiseOr();

        while (self.check(.ampersand_ampersand)) {
            _ = try self.expect(.ampersand_ampersand);
            const right = try self.parseBitwiseOr();
            left = try self.makeBinaryOp(.logical_and, left, right);
        }

        return left;
    }

    fn parseBitwiseOr(self: *Parser) ParseError!Expr {
        var left = try self.parseBitwiseXor();

        while (self.check(.pipe)) {
            _ = try self.expect(.pipe);
            const right = try self.parseBitwiseXor();
            left = try self.makeBinaryOp(.bitwise_or, left, right);
        }

        return left;
    }

    fn parseBitwiseXor(self: *Parser) ParseError!Expr {
        var left = try self.parseBitwiseAnd();

        while (self.check(.caret)) {
            _ = try self.expect(.caret);
            const right = try self.parseBitwiseAnd();
            left = try self.makeBinaryOp(.bitwise_xor, left, right);
        }

        return left;
    }

    fn parseBitwiseAnd(self: *Parser) ParseError!Expr {
        var left = try self.parseComparison();

        while (self.check(.ampersand)) {
            _ = try self.expect(.ampersand);
            const right = try self.parseComparison();
            left = try self.makeBinaryOp(.bitwise_and, left, right);
        }

        return left;
    }

    fn parseComparison(self: *Parser) ParseError!Expr {
        var left = try self.parseShift();

        while (true) {
            const op: BinaryOp = if (self.check(.equal_equal)) blk: {
                _ = try self.expect(.equal_equal);
                break :blk .equal;
            } else if (self.check(.bang_equal)) blk: {
                _ = try self.expect(.bang_equal);
                break :blk .not_equal;
            } else if (self.check(.less)) blk: {
                _ = try self.expect(.less);
                break :blk .less_than;
            } else if (self.check(.greater)) blk: {
                _ = try self.expect(.greater);
                break :blk .greater_than;
            } else if (self.check(.less_equal)) blk: {
                _ = try self.expect(.less_equal);
                break :blk .less_equal;
            } else if (self.check(.greater_equal)) blk: {
                _ = try self.expect(.greater_equal);
                break :blk .greater_equal;
            } else break;

            const right = try self.parseShift();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseShift(self: *Parser) ParseError!Expr {
        var left = try self.parseTerm();

        while (true) {
            const op: BinaryOp = if (self.check(.less_less)) blk: {
                _ = try self.expect(.less_less);
                break :blk .shift_left;
            } else if (self.check(.greater_greater)) blk: {
                _ = try self.expect(.greater_greater);
                break :blk .shift_right;
            } else break;

            const right = try self.parseTerm();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseTerm(self: *Parser) ParseError!Expr {
        var left = try self.parseFactor();

        while (true) {
            const op: BinaryOp = if (self.check(.plus)) blk: {
                _ = try self.expect(.plus);
                break :blk .add;
            } else if (self.check(.minus)) blk: {
                _ = try self.expect(.minus);
                break :blk .subtract;
            } else break;

            const right = try self.parseFactor();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseFactor(self: *Parser) ParseError!Expr {
        var left = try self.parseUnary();

        while (true) {
            const op: BinaryOp = if (self.check(.star)) blk: {
                _ = try self.expect(.star);
                break :blk .multiply;
            } else if (self.check(.slash)) blk: {
                _ = try self.expect(.slash);
                break :blk .divide;
            } else if (self.check(.percent)) blk: {
                _ = try self.expect(.percent);
                break :blk .modulo;
            } else break;

            const right = try self.parseUnary();
            left = try self.makeBinaryOp(op, left, right);
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!Expr {
        const op: ?UnaryOp = if (self.check(.minus)) blk: {
            _ = try self.expect(.minus);
            break :blk .negate;
        } else if (self.check(.bang)) blk: {
            _ = try self.expect(.bang);
            break :blk .logical_not;
        } else if (self.check(.tilde)) blk: {
            _ = try self.expect(.tilde);
            break :blk .bitwise_not;
        } else null;

        if (op) |unary_op| {
            const operand_ptr = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(operand_ptr);
            operand_ptr.* = try self.parseUnary();
            return .{ .unary_op = .{ .op = unary_op, .operand = operand_ptr } };
        }

        return try self.parsePostfix();
    }

    fn parsePrimary(self: *Parser) ParseError!Expr {
        if (self.check(.integer)) {
            const token = try self.expect(.integer);
            const value = try std.fmt.parseInt(i64, token.lexeme, 10);
            return .{ .integer_literal = .{ .value = value, .inferred_type = .{ .kind = .{ .primitive = .i32 }, .usage = .once } } };
        }

        if (self.check(.string)) {
            const token = try self.expect(.string);
            return .{ .string_literal = token.lexeme };
        }

        if (self.check(.true_keyword)) {
            _ = try self.expect(.true_keyword);
            return .{ .bool_literal = true };
        }

        if (self.check(.false_keyword)) {
            _ = try self.expect(.false_keyword);
            return .{ .bool_literal = false };
        }

        if (self.check(.if_keyword)) {
            return try self.parseIfExpression();
        }

        if (self.check(.match_keyword)) {
            return try self.parseMatchExpression();
        }

        if (self.check(.unsafe_keyword)) {
            return try self.parseUnsafeBlock();
        }

        if (self.check(.left_brace)) {
            return try self.parseBlockExpression();
        }

        if (self.check(.intrinsic)) {
            const intrinsic_token = try self.expect(.intrinsic);
            _ = try self.expect(.left_paren);

            // Parse arguments
            var args: std.ArrayList(*Expr) = .empty;
            errdefer {
                for (args.items) |arg| {
                    FunctionDecl.deinitExpr(self.allocator, arg);
                    self.allocator.destroy(arg);
                }
                args.deinit(self.allocator);
            }

            if (!self.check(.right_paren)) {
                while (true) {
                    const arg_expr = try self.parseExpression();
                    const arg_ptr = try self.allocator.create(Expr);
                    arg_ptr.* = arg_expr;
                    try args.append(self.allocator, arg_ptr);

                    if (!self.check(.comma)) break;
                    _ = try self.expect(.comma);
                }
            }

            _ = try self.expect(.right_paren);

            return .{
                .intrinsic_call = .{
                    .name = intrinsic_token.lexeme,
                    .args = try args.toOwnedSlice(self.allocator),
                },
            };
        }

        if (self.check(.identifier)) {
            const token = try self.expect(.identifier);

            // Check if this is a function call or constructor call
            if (self.check(.left_paren)) {
                _ = try self.expect(.left_paren);

                // Parse arguments
                var args: std.ArrayList(*Expr) = .empty;
                errdefer {
                    for (args.items) |arg| {
                        FunctionDecl.deinitExpr(self.allocator, arg);
                        self.allocator.destroy(arg);
                    }
                    args.deinit(self.allocator);
                }

                if (!self.check(.right_paren)) {
                    while (true) {
                        const arg_expr = try self.parseExpression();
                        const arg_ptr = try self.allocator.create(Expr);
                        arg_ptr.* = arg_expr;
                        try args.append(self.allocator, arg_ptr);

                        if (!self.check(.comma)) break;
                        _ = try self.expect(.comma);
                    }
                }

                _ = try self.expect(.right_paren);

                // Constructor call if starts with uppercase
                if (token.lexeme.len > 0 and std.ascii.isUpper(token.lexeme[0])) {
                    return .{
                        .constructor_call = .{
                            .name = token.lexeme,
                            .args = try args.toOwnedSlice(self.allocator),
                        },
                    };
                }

                return .{
                    .function_call = .{
                        .name = token.lexeme,
                        .args = try args.toOwnedSlice(self.allocator),
                    },
                };
            }

            // Only parse as struct literal if identifier starts with uppercase (type names are capitalized)
            if (self.check(.left_brace) and token.lexeme.len > 0 and std.ascii.isUpper(token.lexeme[0])) {
                _ = try self.expect(.left_brace);

                var fields: std.ArrayList(StructLiteralField) = .empty;
                errdefer {
                    for (fields.items) |field| {
                        FunctionDecl.deinitExpr(self.allocator, field.value);
                        self.allocator.destroy(field.value);
                    }
                    fields.deinit(self.allocator);
                }

                if (!self.check(.right_brace)) {
                    while (true) {
                        const field_name = try self.expect(.identifier);
                        _ = try self.expect(.colon);
                        const field_value = try self.parseExpression();
                        const field_value_ptr = try self.allocator.create(Expr);
                        field_value_ptr.* = field_value;

                        try fields.append(self.allocator, StructLiteralField{
                            .name = field_name.lexeme,
                            .value = field_value_ptr,
                        });

                        if (!self.check(.comma)) break;
                        _ = try self.expect(.comma);
                    }
                }

                _ = try self.expect(.right_brace);

                return .{
                    .struct_literal = .{
                        .type_name = token.lexeme,
                        .fields = try fields.toOwnedSlice(self.allocator),
                    },
                };
            }

            // Nullary constructor if starts with uppercase (no args, no parens)
            if (token.lexeme.len > 0 and std.ascii.isUpper(token.lexeme[0])) {
                const empty_args: []*Expr = &[_]*Expr{};
                return .{
                    .constructor_call = .{
                        .name = token.lexeme,
                        .args = try self.allocator.dupe(*Expr, empty_args),
                    },
                };
            }

            // Just a variable
            return .{ .variable = token.lexeme };
        }

        if (self.check(.left_paren)) {
            _ = try self.expect(.left_paren);

            if (self.check(.right_paren)) {
                _ = try self.expect(.right_paren);
                const empty: []*Expr = &[_]*Expr{};
                return .{ .tuple_literal = try self.allocator.dupe(*Expr, empty) };
            }

            const first_expr = try self.parseExpression();

            if (self.check(.comma)) {
                var elements: std.ArrayList(*Expr) = .empty;
                errdefer {
                    for (elements.items) |elem| {
                        FunctionDecl.deinitExpr(self.allocator, elem);
                        self.allocator.destroy(elem);
                    }
                    elements.deinit(self.allocator);
                }

                const first_ptr = try self.allocator.create(Expr);
                first_ptr.* = first_expr;
                try elements.append(self.allocator, first_ptr);

                while (self.check(.comma)) {
                    _ = try self.expect(.comma);
                    const elem_expr = try self.parseExpression();
                    const elem_ptr = try self.allocator.create(Expr);
                    elem_ptr.* = elem_expr;
                    try elements.append(self.allocator, elem_ptr);
                }

                _ = try self.expect(.right_paren);
                return .{ .tuple_literal = try elements.toOwnedSlice(self.allocator) };
            }

            _ = try self.expect(.right_paren);
            return first_expr;
        }

        return error.ExpectedExpression;
    }

    fn parsePostfix(self: *Parser) ParseError!Expr {
        var expr = try self.parsePrimary();

        while (self.check(.dot)) {
            _ = try self.expect(.dot);

            if (self.check(.integer)) {
                // Tuple indexing: expr.0, expr.1, etc.
                const index_token = try self.expect(.integer);
                const index = try std.fmt.parseInt(usize, index_token.lexeme, 10);

                const tuple_ptr = try self.allocator.create(Expr);
                tuple_ptr.* = expr;

                expr = .{ .tuple_index = .{
                    .tuple = tuple_ptr,
                    .index = index,
                } };
            } else if (self.check(.identifier)) {
                const identifier = try self.expect(.identifier);

                // Check if it's a method call: expr.method(args)
                if (self.check(.left_paren)) {
                    _ = try self.expect(.left_paren);

                    var args: std.ArrayList(*Expr) = .empty;
                    errdefer args.deinit(self.allocator);

                    while (!self.check(.right_paren) and !self.isAtEnd()) {
                        const arg = try self.parseExpressionPtr(&.{});
                        try args.append(self.allocator, arg);

                        if (!self.check(.right_paren)) {
                            _ = try self.expect(.comma);
                        }
                    }

                    _ = try self.expect(.right_paren);

                    const object_ptr = try self.allocator.create(Expr);
                    object_ptr.* = expr;

                    expr = .{ .method_call = .{
                        .object = object_ptr,
                        .method_name = identifier.lexeme,
                        .args = try args.toOwnedSlice(self.allocator),
                    } };
                } else {
                    // Struct field access: expr.field
                    const object_ptr = try self.allocator.create(Expr);
                    object_ptr.* = expr;

                    expr = .{ .field_access = .{
                        .object = object_ptr,
                        .field_name = identifier.lexeme,
                    } };
                }
            } else {
                return error.ExpectedExpression;
            }
        }

        return expr;
    }

    fn parseExpressionPtr(self: *Parser, already_allocated: []const *Expr) !*Expr {
        const expr = try self.parseExpression();
        const ptr = try self.allocator.create(Expr);
        errdefer {
            for (already_allocated) |allocated| {
                FunctionDecl.deinitExpr(self.allocator, allocated);
                self.allocator.destroy(allocated);
            }
            self.allocator.destroy(ptr);
        }
        ptr.* = expr;
        return ptr;
    }

    fn parseIfExpression(self: *Parser) ParseError!Expr {
        _ = try self.expect(.if_keyword);
        return try self.parseIfExpressionContinuation();
    }

    fn parseIfExpressionContinuation(self: *Parser) ParseError!Expr {
        // Parse condition
        const condition = try self.parseExpressionPtr(&.{});

        // Parse then branch
        _ = try self.expect(.left_brace);
        const then_branch = try self.parseExpressionPtr(&.{condition});
        _ = try self.expect(.right_brace);

        // Parse optional elseif/else branch
        const else_branch = blk: {
            if (self.check(.elseif_keyword)) {
                // elseif becomes a nested if expression
                _ = try self.expect(.elseif_keyword);
                const nested_if = try self.parseIfExpressionContinuation();
                const else_ptr = try self.allocator.create(Expr);
                else_ptr.* = nested_if;
                break :blk else_ptr;
            } else if (self.check(.else_keyword)) {
                _ = try self.expect(.else_keyword);
                _ = try self.expect(.left_brace);
                const else_ptr = try self.parseExpressionPtr(&.{ condition, then_branch });
                _ = try self.expect(.right_brace);
                break :blk else_ptr;
            } else {
                break :blk null;
            }
        };
        errdefer {
            if (else_branch) |eb| {
                FunctionDecl.deinitExpr(self.allocator, eb);
                self.allocator.destroy(eb);
            }
        }

        return .{
            .if_expr = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        };
    }

    fn parseMatchExpression(self: *Parser) ParseError!Expr {
        _ = try self.expect(.match_keyword);
        const scrutinee = try self.parseExpressionPtr(&.{});
        _ = try self.expect(.left_brace);

        var arms: std.ArrayList(MatchArm) = .empty;
        errdefer {
            for (arms.items) |arm| {
                switch (arm.pattern) {
                    .identifier => {},
                    .constructor => |constructor| {
                        self.allocator.free(constructor.bindings);
                    },
                }
                FunctionDecl.deinitExpr(self.allocator, arm.body);
                self.allocator.destroy(arm.body);
            }
            arms.deinit(self.allocator);
        }

        while (!self.check(.right_brace)) {
            const pattern = try self.parseMatchPattern();
            _ = try self.expect(.fat_arrow);
            const body_expr = try self.parseExpression();
            const body_ptr = try self.allocator.create(Expr);
            body_ptr.* = body_expr;

            try arms.append(self.allocator, MatchArm{
                .pattern = pattern,
                .body = body_ptr,
            });

            if (self.check(.comma)) {
                _ = try self.expect(.comma);
            }
        }

        _ = try self.expect(.right_brace);

        return .{
            .match_expr = .{
                .scrutinee = scrutinee,
                .arms = try arms.toOwnedSlice(self.allocator),
            },
        };
    }

    fn parseMatchPattern(self: *Parser) ParseError!MatchPattern {
        const name_token = try self.expect(.identifier);

        if (self.check(.left_paren)) {
            _ = try self.expect(.left_paren);

            var bindings: std.ArrayList([]const u8) = .empty;
            errdefer bindings.deinit(self.allocator);

            if (!self.check(.right_paren)) {
                while (true) {
                    const binding_token = try self.expect(.identifier);
                    try bindings.append(self.allocator, binding_token.lexeme);

                    if (!self.check(.comma)) break;
                    _ = try self.expect(.comma);
                }
            }

            _ = try self.expect(.right_paren);

            return .{
                .constructor = .{
                    .name = name_token.lexeme,
                    .bindings = try bindings.toOwnedSlice(self.allocator),
                },
            };
        }

        return .{ .identifier = name_token.lexeme };
    }

    fn parseBlockExpression(self: *Parser) ParseError!Expr {
        _ = try self.expect(.left_brace);

        var stmts: std.ArrayList(Stmt) = .empty;
        errdefer {
            for (stmts.items) |*stmt| {
                FunctionDecl.deinitStmt(self.allocator, stmt);
            }
            stmts.deinit(self.allocator);
        }

        // Parse statements until we hit right_brace or final expression
        while (!self.check(.right_brace)) {
            if (self.isStatementStart()) {
                const stmt = try self.parseStatement();
                try stmts.append(self.allocator, stmt);
            } else {
                // It's a final expression (no semicolon)
                const result_ptr = try self.allocator.create(Expr);
                errdefer self.allocator.destroy(result_ptr);
                result_ptr.* = try self.parseExpression();
                _ = try self.expect(.right_brace);
                return .{
                    .block_expr = .{
                        .statements = try stmts.toOwnedSlice(self.allocator),
                        .result = result_ptr,
                    },
                };
            }
        }

        _ = try self.expect(.right_brace);
        return .{
            .block_expr = .{
                .statements = try stmts.toOwnedSlice(self.allocator),
                .result = null,
            },
        };
    }

    fn parseUnsafeBlock(self: *Parser) ParseError!Expr {
        _ = try self.expect(.unsafe_keyword);
        _ = try self.expect(.left_brace);

        var stmts: std.ArrayList(Stmt) = .empty;
        errdefer {
            for (stmts.items) |*stmt| {
                FunctionDecl.deinitStmt(self.allocator, stmt);
            }
            stmts.deinit(self.allocator);
        }

        // Parse statements until we hit right_brace or final expression
        while (!self.check(.right_brace)) {
            if (self.isStatementStart()) {
                const stmt = try self.parseStatement();
                try stmts.append(self.allocator, stmt);
            } else {
                // It's a final expression (no semicolon)
                const result_ptr = try self.allocator.create(Expr);
                errdefer self.allocator.destroy(result_ptr);
                result_ptr.* = try self.parseExpression();
                _ = try self.expect(.right_brace);
                return .{
                    .unsafe_block = .{
                        .statements = try stmts.toOwnedSlice(self.allocator),
                        .result = result_ptr,
                    },
                };
            }
        }

        _ = try self.expect(.right_brace);
        return .{
            .unsafe_block = .{
                .statements = try stmts.toOwnedSlice(self.allocator),
                .result = null,
            },
        };
    }

    fn peekAhead(self: *Parser, n: usize) Token {
        const target_pos = self.pos + n;
        if (target_pos >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1]; // Return EOF token
        }
        return self.tokens[target_pos];
    }

    fn expect(self: *Parser, kind: TokenKind) !Token {
        if (self.isAtEnd()) return error.UnexpectedEndOfFile;
        const token = self.current();
        if (token.kind != kind) {
            std.debug.print("Expected {s}, got {s} at line {d}\n", .{
                @tagName(kind),
                @tagName(token.kind),
                token.line,
            });
            return error.UnexpectedToken;
        }
        self.pos += 1;
        return token;
    }

    fn check(self: *Parser, kind: TokenKind) bool {
        if (self.isAtEnd()) return false;
        return self.current().kind == kind;
    }

    fn current(self: *Parser) Token {
        return self.tokens[self.pos];
    }

    fn isAtEnd(self: *Parser) bool {
        return self.pos >= self.tokens.len or self.current().kind == .eof;
    }

    fn parseClassDef(self: *Parser, is_public: bool) !ClassDef {
        _ = try self.expect(.class_keyword);
        const name_token = try self.expect(.identifier);
        const name = name_token.lexeme;

        _ = try self.expect(.left_brace);

        var methods: std.ArrayList(ClassMethod) = .empty;
        errdefer methods.deinit(self.allocator);

        while (!self.check(.right_brace) and !self.isAtEnd()) {
            const method_name_token = try self.expect(.identifier);
            const method_name = method_name_token.lexeme;

            _ = try self.expect(.colon);
            _ = try self.expect(.fn_keyword);
            _ = try self.expect(.left_paren);

            var param_types: std.ArrayList(Type) = .empty;
            errdefer param_types.deinit(self.allocator);

            var self_param = false;
            while (!self.check(.right_paren) and !self.isAtEnd()) {
                const param_type = try self.parseType();
                // Check if this is Self type
                if (param_type.kind == .named) {
                    if (std.mem.eql(u8, param_type.kind.named, "Self")) {
                        self_param = true;
                    }
                }
                try param_types.append(self.allocator, param_type);

                if (!self.check(.right_paren)) {
                    _ = try self.expect(.comma);
                }
            }

            _ = try self.expect(.right_paren);

            // Optional return type
            var return_type: ?Type = null;
            if (!self.check(.semicolon)) {
                return_type = try self.parseType();
            }

            const method = ClassMethod{
                .name = method_name,
                .self_param = self_param,
                .param_types = try param_types.toOwnedSlice(self.allocator),
                .return_type = return_type,
            };
            try methods.append(self.allocator, method);

            // Skip optional semicolon
            if (self.check(.semicolon)) {
                self.pos += 1;
            }
        }

        _ = try self.expect(.right_brace);

        return ClassDef{
            .name = name,
            .methods = try methods.toOwnedSlice(self.allocator),
            .is_public = is_public,
        };
    }

    fn parseInstanceDecl(self: *Parser) !InstanceDecl {
        _ = try self.expect(.instance_keyword);
        const class_name_token = try self.expect(.identifier);
        const class_name = class_name_token.lexeme;

        _ = try self.expect(.left_bracket);
        const type_name = try self.parseType();
        _ = try self.expect(.right_bracket);

        _ = try self.expect(.left_brace);

        var methods: std.ArrayList(MethodImpl) = .empty;
        errdefer methods.deinit(self.allocator);

        while (!self.check(.right_brace) and !self.isAtEnd()) {
            const method_name_token = try self.expect(.identifier);
            const method_name = method_name_token.lexeme;

            _ = try self.expect(.equal);
            _ = try self.expect(.fn_keyword);

            // Parse parameters
            _ = try self.expect(.left_paren);
            var params: std.ArrayList(Param) = .empty;
            errdefer params.deinit(self.allocator);

            while (!self.check(.right_paren) and !self.isAtEnd()) {
                const param_name_token = try self.expect(.identifier);
                const param_name = param_name_token.lexeme;

                _ = try self.expect(.colon);
                const param_type = try self.parseType();

                try params.append(self.allocator, Param{
                    .name = param_name,
                    .param_type = param_type,
                });

                if (!self.check(.right_paren)) {
                    _ = try self.expect(.comma);
                }
            }
            _ = try self.expect(.right_paren);

            // Parse return type
            const return_type = try self.parseType();

            // Parse body
            const body = try self.parseBlockStatements();

            const method = MethodImpl{
                .name = method_name,
                .params = try params.toOwnedSlice(self.allocator),
                .return_type = return_type,
                .body = body,
            };
            try methods.append(self.allocator, method);
        }

        _ = try self.expect(.right_brace);

        return InstanceDecl{
            .class_name = class_name,
            .type_name = type_name,
            .methods = try methods.toOwnedSlice(self.allocator),
        };
    }
};

test "parser: simple function" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return 42 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 1), ast.functions.items.len);

    const func = ast.functions.items[0];
    try testing.expectEqualStrings("main", func.name);
    try testing.expect(func.return_type.kind == .primitive);
    try testing.expectEqual(PrimitiveType.i32, func.return_type.kind.primitive);
    try testing.expectEqual(@as(usize, 1), func.body.items.len);

    const stmt = func.body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .integer_literal);
    try testing.expectEqual(@as(i64, 42), stmt.return_stmt.integer_literal.value);
}

test "parser: binary addition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return 1 + 2 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    const stmt = func.body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, stmt.return_stmt.binary_op.op);
    try testing.expectEqual(@as(i64, 1), stmt.return_stmt.binary_op.left.integer_literal.value);
    try testing.expectEqual(@as(i64, 2), stmt.return_stmt.binary_op.right.integer_literal.value);
}

test "parser: operator precedence" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return 1 + 2 * 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    const stmt = func.body.items[0];
    // Should parse as 1 + (2 * 3)
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, stmt.return_stmt.binary_op.op);
    try testing.expectEqual(@as(i64, 1), stmt.return_stmt.binary_op.left.integer_literal.value);

    const right = stmt.return_stmt.binary_op.right;
    try testing.expect(right.* == .binary_op);
    try testing.expectEqual(BinaryOp.multiply, right.binary_op.op);
    try testing.expectEqual(@as(i64, 2), right.binary_op.left.integer_literal.value);
    try testing.expectEqual(@as(i64, 3), right.binary_op.right.integer_literal.value);
}

test "parser: arithmetic operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() I32 { return 5 - 3 }", .op = .subtract },
        .{ .src = "fn f() I32 { return 5 * 3 }", .op = .multiply },
        .{ .src = "fn f() I32 { return 5 / 3 }", .op = .divide },
        .{ .src = "fn f() I32 { return 5 % 3 }", .op = .modulo },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: comparison operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() I32 { return 5 == 3 }", .op = .equal },
        .{ .src = "fn f() I32 { return 5 != 3 }", .op = .not_equal },
        .{ .src = "fn f() I32 { return 5 < 3 }", .op = .less_than },
        .{ .src = "fn f() I32 { return 5 > 3 }", .op = .greater_than },
        .{ .src = "fn f() I32 { return 5 <= 3 }", .op = .less_equal },
        .{ .src = "fn f() I32 { return 5 >= 3 }", .op = .greater_equal },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: logical operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() I32 { return 5 && 3 }", .op = .logical_and },
        .{ .src = "fn f() I32 { return 5 || 3 }", .op = .logical_or },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: bitwise operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() I32 { return 5 & 3 }", .op = .bitwise_and },
        .{ .src = "fn f() I32 { return 5 | 3 }", .op = .bitwise_or },
        .{ .src = "fn f() I32 { return 5 ^ 3 }", .op = .bitwise_xor },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: shift operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: BinaryOp }{
        .{ .src = "fn f() I32 { return 5 << 3 }", .op = .shift_left },
        .{ .src = "fn f() I32 { return 5 >> 3 }", .op = .shift_right },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expectEqual(case.op, stmt.return_stmt.binary_op.op);
    }
}

test "parser: parentheses" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return (1 + 2) * 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    // Should parse as (1 + 2) * 3
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.multiply, stmt.return_stmt.binary_op.op);
    try testing.expectEqual(@as(i64, 3), stmt.return_stmt.binary_op.right.integer_literal.value);

    const left = stmt.return_stmt.binary_op.left;
    try testing.expect(left.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, left.binary_op.op);
    try testing.expectEqual(@as(i64, 1), left.binary_op.left.integer_literal.value);
    try testing.expectEqual(@as(i64, 2), left.binary_op.right.integer_literal.value);
}

test "parser: unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const cases = [_]struct { src: []const u8, op: UnaryOp }{
        .{ .src = "fn f() I32 { return -5 }", .op = .negate },
        .{ .src = "fn f() I32 { return !5 }", .op = .logical_not },
        .{ .src = "fn f() I32 { return ~5 }", .op = .bitwise_not },
    };

    for (cases) |case| {
        var lex = Lexer.init(case.src);
        var tokens = try lex.tokenize(testing.allocator);
        defer tokens.deinit(testing.allocator);

        var p = Parser.init(tokens.items, testing.allocator);
        var ast = try p.parse();
        defer ast.deinit(testing.allocator);

        const stmt = ast.functions.items[0].body.items[0];
        try testing.expect(stmt.return_stmt.* == .unary_op);
        try testing.expectEqual(case.op, stmt.return_stmt.unary_op.op);
        try testing.expectEqual(@as(i64, 5), stmt.return_stmt.unary_op.operand.integer_literal.value);
    }
}

test "parser: nested unary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return --5 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    // Should parse as -(-5)
    try testing.expect(stmt.return_stmt.* == .unary_op);
    try testing.expectEqual(UnaryOp.negate, stmt.return_stmt.unary_op.op);

    const inner = stmt.return_stmt.unary_op.operand;
    try testing.expect(inner.* == .unary_op);
    try testing.expectEqual(UnaryOp.negate, inner.unary_op.op);
    try testing.expectEqual(@as(i64, 5), inner.unary_op.operand.integer_literal.value);
}

test "parser: unary with binary operators" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return -5 + 3 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    // Should parse as (-5) + 3
    try testing.expect(stmt.return_stmt.* == .binary_op);
    try testing.expectEqual(BinaryOp.add, stmt.return_stmt.binary_op.op);

    const left = stmt.return_stmt.binary_op.left;
    try testing.expect(left.* == .unary_op);
    try testing.expectEqual(UnaryOp.negate, left.unary_op.op);
    try testing.expectEqual(@as(i64, 5), left.unary_op.operand.integer_literal.value);

    try testing.expectEqual(@as(i64, 3), stmt.return_stmt.binary_op.right.integer_literal.value);
}

test "parser: bool literals" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() Bool { return true }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .bool_literal);
    try testing.expectEqual(true, stmt.return_stmt.bool_literal);
}

test "parser: simple if expression" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if true { 1 } else { 2 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const if_expr = stmt.return_stmt.if_expr;
    // Check condition
    try testing.expect(if_expr.condition.* == .bool_literal);
    try testing.expectEqual(true, if_expr.condition.bool_literal);

    // Check then branch
    try testing.expect(if_expr.then_branch.* == .integer_literal);
    try testing.expectEqual(@as(i64, 1), if_expr.then_branch.integer_literal.value);

    // Check else branch
    try testing.expect(if_expr.else_branch != null);
    try testing.expect(if_expr.else_branch.?.* == .integer_literal);
    try testing.expectEqual(@as(i64, 2), if_expr.else_branch.?.integer_literal.value);
}

test "parser: if expression with comparison condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if 10 > 5 { 1 } else { 0 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const if_expr = stmt.return_stmt.if_expr;
    // Condition should be binary_op
    try testing.expect(if_expr.condition.* == .binary_op);
    try testing.expectEqual(BinaryOp.greater_than, if_expr.condition.binary_op.op);
}

test "parser: elseif chain" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if false { 1 } elseif true { 2 } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const if_expr = stmt.return_stmt.if_expr;
    // First condition
    try testing.expect(if_expr.condition.* == .bool_literal);
    try testing.expectEqual(false, if_expr.condition.bool_literal);

    // Then branch
    try testing.expectEqual(@as(i64, 1), if_expr.then_branch.integer_literal.value);

    // Else branch should be another if_expr (the elseif)
    try testing.expect(if_expr.else_branch != null);
    try testing.expect(if_expr.else_branch.?.* == .if_expr);

    const nested_if = if_expr.else_branch.?.if_expr;
    try testing.expect(nested_if.condition.* == .bool_literal);
    try testing.expectEqual(true, nested_if.condition.bool_literal);
    try testing.expectEqual(@as(i64, 2), nested_if.then_branch.integer_literal.value);
    try testing.expectEqual(@as(i64, 3), nested_if.else_branch.?.integer_literal.value);
}

test "parser: multiple elseif chain" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if false { 1 } elseif false { 2 } elseif true { 3 } else { 4 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    // Should create deeply nested if expressions
    const if1 = stmt.return_stmt.if_expr;
    try testing.expectEqual(@as(i64, 1), if1.then_branch.integer_literal.value);

    // First elseif
    try testing.expect(if1.else_branch.?.* == .if_expr);
    const if2 = if1.else_branch.?.if_expr;
    try testing.expectEqual(@as(i64, 2), if2.then_branch.integer_literal.value);

    // Second elseif
    try testing.expect(if2.else_branch.?.* == .if_expr);
    const if3 = if2.else_branch.?.if_expr;
    try testing.expectEqual(@as(i64, 3), if3.then_branch.integer_literal.value);

    // Final else
    try testing.expectEqual(@as(i64, 4), if3.else_branch.?.integer_literal.value);
}

test "parser: nested if expressions" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { return if true { if false { 1 } else { 2 } } else { 3 } }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .if_expr);

    const outer_if = stmt.return_stmt.if_expr;
    // Then branch should be another if_expr
    try testing.expect(outer_if.then_branch.* == .if_expr);

    const inner_if = outer_if.then_branch.if_expr;
    try testing.expectEqual(@as(i64, 1), inner_if.then_branch.integer_literal.value);
    try testing.expectEqual(@as(i64, 2), inner_if.else_branch.?.integer_literal.value);

    // Outer else
    try testing.expectEqual(@as(i64, 3), outer_if.else_branch.?.integer_literal.value);
}

test "parser: simple while loop" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { while true { 42 }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .while_stmt);

    const while_loop = stmt.while_stmt;
    try testing.expect(while_loop.condition.* == .bool_literal);
    try testing.expectEqual(true, while_loop.condition.bool_literal);
    try testing.expect(while_loop.body.* == .block_expr);
}

test "parser: while loop with comparison condition" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { while 5 > 3 { 1 }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .while_stmt);

    const while_loop = stmt.while_stmt;
    try testing.expect(while_loop.condition.* == .binary_op);
    try testing.expectEqual(BinaryOp.greater_than, while_loop.condition.binary_op.op);
}

test "parser: nested while loops" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn f() I32 { while true { while false { 1 } }; return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .while_stmt);

    const outer_while = stmt.while_stmt;
    try testing.expect(outer_while.body.* == .block_expr);
    const outer_body = outer_while.body.block_expr;
    try testing.expectEqual(@as(usize, 1), outer_body.statements.len);
    try testing.expect(outer_body.statements[0] == .while_stmt);

    const inner_while = outer_body.statements[0].while_stmt;
    try testing.expect(inner_while.condition.* == .bool_literal);
    try testing.expectEqual(false, inner_while.condition.bool_literal);
}

test "parser: empty tuple literal" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return () }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .tuple_literal);
    try testing.expectEqual(@as(usize, 0), stmt.return_stmt.tuple_literal.len);
}

test "parser: simple tuple literal" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return (1, 2) }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .tuple_literal);
    try testing.expectEqual(@as(usize, 2), stmt.return_stmt.tuple_literal.len);
    try testing.expect(stmt.return_stmt.tuple_literal[0].* == .integer_literal);
    try testing.expectEqual(@as(i64, 1), stmt.return_stmt.tuple_literal[0].integer_literal.value);
    try testing.expect(stmt.return_stmt.tuple_literal[1].* == .integer_literal);
    try testing.expectEqual(@as(i64, 2), stmt.return_stmt.tuple_literal[1].integer_literal.value);
}

test "parser: nested tuple literal" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return (1, (2, 3)) }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .tuple_literal);
    try testing.expectEqual(@as(usize, 2), stmt.return_stmt.tuple_literal.len);
    try testing.expect(stmt.return_stmt.tuple_literal[0].* == .integer_literal);
    try testing.expectEqual(@as(i64, 1), stmt.return_stmt.tuple_literal[0].integer_literal.value);

    try testing.expect(stmt.return_stmt.tuple_literal[1].* == .tuple_literal);
    const inner_tuple = stmt.return_stmt.tuple_literal[1].tuple_literal;
    try testing.expectEqual(@as(usize, 2), inner_tuple.len);
    try testing.expectEqual(@as(i64, 2), inner_tuple[0].integer_literal.value);
    try testing.expectEqual(@as(i64, 3), inner_tuple[1].integer_literal.value);
}

test "parser: tuple with mixed types" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return (42, true, 100) }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.return_stmt.* == .tuple_literal);
    try testing.expectEqual(@as(usize, 3), stmt.return_stmt.tuple_literal.len);
    try testing.expect(stmt.return_stmt.tuple_literal[0].* == .integer_literal);
    try testing.expectEqual(@as(i64, 42), stmt.return_stmt.tuple_literal[0].integer_literal.value);
    try testing.expect(stmt.return_stmt.tuple_literal[1].* == .bool_literal);
    try testing.expectEqual(true, stmt.return_stmt.tuple_literal[1].bool_literal);
    try testing.expect(stmt.return_stmt.tuple_literal[2].* == .integer_literal);
    try testing.expectEqual(@as(i64, 100), stmt.return_stmt.tuple_literal[2].integer_literal.value);
}

test "parser: tuple indexing" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { let x = (1, 2); return x.0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    const ret_stmt = func.body.items[1];
    try testing.expect(ret_stmt == .return_stmt);
    try testing.expect(ret_stmt.return_stmt.* == .tuple_index);
    try testing.expect(ret_stmt.return_stmt.tuple_index.tuple.* == .variable);
    try testing.expectEqualStrings("x", ret_stmt.return_stmt.tuple_index.tuple.variable);
    try testing.expectEqual(@as(usize, 0), ret_stmt.return_stmt.tuple_index.index);
}

test "parser: nested tuple indexing" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { let x = (1, (2, 3)); return x.1.0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const ret_stmt = ast.functions.items[0].body.items[1];
    try testing.expect(ret_stmt.return_stmt.* == .tuple_index);
    try testing.expectEqual(@as(usize, 0), ret_stmt.return_stmt.tuple_index.index);

    const inner_index = ret_stmt.return_stmt.tuple_index.tuple;
    try testing.expect(inner_index.* == .tuple_index);
    try testing.expectEqual(@as(usize, 1), inner_index.tuple_index.index);
    try testing.expect(inner_index.tuple_index.tuple.* == .variable);
    try testing.expectEqualStrings("x", inner_index.tuple_index.tuple.variable);
}

test "parser: simple tuple type" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() (I32, Bool) { return (42, true) }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    try testing.expect(func.return_type.kind == .tuple);
    try testing.expectEqual(@as(usize, 2), func.return_type.kind.tuple.len);
    try testing.expect(func.return_type.kind.tuple[0].kind == .primitive);
    try testing.expectEqual(PrimitiveType.i32, func.return_type.kind.tuple[0].kind.primitive);
    try testing.expect(func.return_type.kind.tuple[1].kind == .primitive);
    try testing.expectEqual(PrimitiveType.bool, func.return_type.kind.tuple[1].kind.primitive);
}

test "parser: nested tuple type" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() (I32, (Bool, I64)) { return (1, (true, 2)) }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const func = ast.functions.items[0];
    try testing.expect(func.return_type.kind == .tuple);
    try testing.expectEqual(@as(usize, 2), func.return_type.kind.tuple.len);
    try testing.expect(func.return_type.kind.tuple[0].kind == .primitive);
    try testing.expectEqual(PrimitiveType.i32, func.return_type.kind.tuple[0].kind.primitive);

    try testing.expect(func.return_type.kind.tuple[1].kind == .tuple);
    const inner_type = func.return_type.kind.tuple[1].kind.tuple;
    try testing.expectEqual(@as(usize, 2), inner_type.len);
    try testing.expect(inner_type[0].kind == .primitive);
    try testing.expectEqual(PrimitiveType.bool, inner_type[0].kind.primitive);
    try testing.expect(inner_type[1].kind == .primitive);
    try testing.expectEqual(PrimitiveType.i64, inner_type[1].kind.primitive);
}

test "parser: simple tuple destructuring" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { let (a, b) = (1, 2); return a }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .let_binding);
    try testing.expect(stmt.let_binding.pattern == .tuple);
    try testing.expectEqual(@as(usize, 2), stmt.let_binding.pattern.tuple.len);
    try testing.expect(stmt.let_binding.pattern.tuple[0].* == .identifier);
    try testing.expectEqualStrings("a", stmt.let_binding.pattern.tuple[0].identifier);
    try testing.expect(stmt.let_binding.pattern.tuple[1].* == .identifier);
    try testing.expectEqualStrings("b", stmt.let_binding.pattern.tuple[1].identifier);

    try testing.expect(stmt.let_binding.value.* == .tuple_literal);
    try testing.expectEqual(@as(usize, 2), stmt.let_binding.value.tuple_literal.len);
}

test "parser: nested tuple destructuring" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { let (x, (y, z)) = (1, (2, 3)); return x }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt.let_binding.pattern == .tuple);
    try testing.expectEqual(@as(usize, 2), stmt.let_binding.pattern.tuple.len);
    try testing.expect(stmt.let_binding.pattern.tuple[0].* == .identifier);
    try testing.expectEqualStrings("x", stmt.let_binding.pattern.tuple[0].identifier);

    try testing.expect(stmt.let_binding.pattern.tuple[1].* == .tuple);
    const inner_pattern = stmt.let_binding.pattern.tuple[1].tuple;
    try testing.expectEqual(@as(usize, 2), inner_pattern.len);
    try testing.expect(inner_pattern[0].* == .identifier);
    try testing.expectEqualStrings("y", inner_pattern[0].identifier);
    try testing.expect(inner_pattern[1].* == .identifier);
    try testing.expectEqualStrings("z", inner_pattern[1].identifier);
}

test "parser: tuple with type annotation" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { let x: (I32, Bool) = (42, true); return 0 }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .let_binding);
    try testing.expect(stmt.let_binding.type_annotation != null);
    const type_ann = stmt.let_binding.type_annotation.?;
    try testing.expect(type_ann.kind == .tuple);
    try testing.expectEqual(@as(usize, 2), type_ann.kind.tuple.len);
    try testing.expect(type_ann.kind.tuple[0].kind == .primitive);
    try testing.expectEqual(PrimitiveType.i32, type_ann.kind.tuple[0].kind.primitive);
    try testing.expect(type_ann.kind.tuple[1].kind == .primitive);
    try testing.expectEqual(PrimitiveType.bool, type_ann.kind.tuple[1].kind.primitive);
}

test "parser: parenthesized expression vs tuple" {
    const testing = std.testing;
    const Lexer = @import("lexer.zig").Lexer;

    const source = "fn main() I32 { return (42) }";
    var lex = Lexer.init(source);
    var tokens = try lex.tokenize(testing.allocator);
    defer tokens.deinit(testing.allocator);

    var p = Parser.init(tokens.items, testing.allocator);
    var ast = try p.parse();
    defer ast.deinit(testing.allocator);

    const stmt = ast.functions.items[0].body.items[0];
    try testing.expect(stmt == .return_stmt);
    try testing.expect(stmt.return_stmt.* == .integer_literal);
    try testing.expectEqual(@as(i64, 42), stmt.return_stmt.integer_literal.value);
}

