# Generic Struct Type Parameter Substitution

## Problem

Currently, type parameters in struct definitions aren't substituted when instantiated:

```orion
type Box[T] = { value: T }
let b: Box[i32] = Box[i32] { value: 42 }  // ERROR: T not substituted
```

The typedef stores `{ value: T }` literally, and `T` remains an unresolved named type.

## Current State

**Parser** - Already handles type parameters:
```zig
pub const TypeDef = struct {
    name: []const u8,
    params: []TypeParameter,  // [T] parsed here
    type_value: Type,         // { value: T } - T stored as named type
    is_public: bool,
};
```

**Type resolution** - Looks up typedef but doesn't substitute:
```zig
fn resolveType(self: *TypeChecker, typ: Type) Type {
    switch (typ.kind) {
        .dependent => |dep| {
            if (self.type_defs.get(dep.base)) |typedef| {
                return typedef;  // Returns { value: T } as-is
            }
        },
        ...
    }
}
```

## Solution

### 1. Add Type Substitution Function

```zig
// In typechecker.zig or a shared types module
fn substituteTypeParams(
    allocator: Allocator,
    typ: Type,
    substitutions: std.StringHashMap(Type),
) !Type {
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
            var new_fields = try allocator.alloc(StructField, fields.len);
            for (fields, 0..) |field, i| {
                new_fields[i] = .{
                    .name = field.name,
                    .field_type = try substituteTypeParams(allocator, field.field_type.*, substitutions),
                };
            }
            return Type{ .kind = .{ .struct_type = new_fields }, .usage = typ.usage };
        },
        .tuple => |elems| {
            // Recursively substitute in each element
            var new_elems = try allocator.alloc(*Type, elems.len);
            for (elems, 0..) |elem, i| {
                var new_type = try allocator.create(Type);
                new_type.* = try substituteTypeParams(allocator, elem.*, substitutions);
                new_elems[i] = new_type;
            }
            return Type{ .kind = .{ .tuple = new_elems }, .usage = typ.usage };
        },
        .sum_type => |variants| {
            // Recursively substitute in variant payloads
            var new_variants = try allocator.alloc(SumTypeVariant, variants.len);
            for (variants, 0..) |variant, i| {
                var new_payloads = try allocator.alloc(*Type, variant.payload_types.len);
                for (variant.payload_types, 0..) |payload, j| {
                    var new_type = try allocator.create(Type);
                    new_type.* = try substituteTypeParams(allocator, payload.*, substitutions);
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
            var new_type_params = try allocator.alloc(TypeParam, dep.type_params.len);
            for (dep.type_params, 0..) |param, i| {
                switch (param) {
                    .concrete => |t| {
                        var new_t = try allocator.create(Type);
                        new_t.* = try substituteTypeParams(allocator, t.*, substitutions);
                        new_type_params[i] = .{ .concrete = new_t };
                    },
                    .variable => new_type_params[i] = param,
                }
            }
            return Type{
                .kind = .{ .dependent = .{
                    .base = dep.base,
                    .type_params = new_type_params,
                    .value_params = dep.value_params,
                }},
                .usage = typ.usage,
            };
        },
        // Primitives don't need substitution
        .primitive => return typ,
    }
}
```

### 2. Update resolveType to Substitute

```zig
fn resolveType(self: *TypeChecker, typ: Type) Type {
    switch (typ.kind) {
        .dependent => |dep| {
            if (self.type_defs.get(dep.base)) |typedef| {
                // Build substitution map from type params
                var substitutions = std.StringHashMap(Type).init(self.allocator);
                defer substitutions.deinit();

                for (typedef.params, 0..) |param, i| {
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
                return substituteTypeParams(self.allocator, typedef.type_value, substitutions)
                    catch typedef.type_value;
            }
            return typ;
        },
        // ... rest unchanged
    }
}
```

### 3. Store TypeDef with Parameters

The typedef storage needs to include the parameter list:

```zig
// In TypeChecker
type_defs: std.StringHashMap(TypeDefInfo),

const TypeDefInfo = struct {
    params: []TypeParameter,
    type_value: Type,
};
```

Currently it only stores `Type`, losing the parameter names needed for substitution.

## Example Flow

```orion
type Box[T] = { value: T }
let b: Box[i32] = Box[i32] { value: 42 }
```

1. **Parse**: `TypeDef{ name: "Box", params: [{name: "T", kind: .type_param}], type_value: {value: T} }`

2. **Store**: `type_defs["Box"] = { params: [T], type_value: {value: T} }`

3. **Resolve `Box[i32]`**:
   - Look up "Box" → get typedef with params [T]
   - Build substitution: `{T: i32}`
   - Apply to `{value: T}` → `{value: i32}`

4. **Type check**: Field `value` expects `i32`, literal `42` fits ✓

## Codegen Changes

Similar substitution needed in codegen when:
- Generating LLVM struct types
- Generating field access
- Generating struct literals

The `mangledTypeName` already handles dependent types for naming, but field type resolution needs the same substitution logic.

## Implementation Order

1. Add `TypeDefInfo` struct and update storage
2. Implement `substituteTypeParams` function
3. Update `resolveType` in typechecker
4. Update codegen's type resolution
5. Add tests for generic structs

## Test Cases

```orion
// Basic generic struct
type Box[T] = { value: T }
let b: Box[i32] = Box[i32] { value: 42 }
assert(b.value == 42)

// Multiple type params
type Pair[A, B] = { first: A, second: B }
let p: Pair[i32, bool] = Pair[i32, bool] { first: 1, second: true }

// Nested generics
type Wrapper[T] = { inner: Box[T] }
let w: Wrapper[i64] = Wrapper[i64] { inner: Box[i64] { value: 100 } }

// Generic with value param
type Array[T, n: u64] = { data: ptr, len: u64 }

// Generic sum type
type Option[T] = Some(T) | None
let x: Option[i32] = Some(42)
```
