# Dependent Types Implementation Plan

## Goal

Implement minimum viable dependent types sufficient for self-hosting the Orion compiler. The self-hosted compiler will then implement the full dependent type system.

## Motivation

The original Orion compiler hit a critical architectural issue: dependent type parameters existed only at the type level, not as runtime values. This made it impossible to write functions like:

```orion
fn append(v1: Vec[A, n], v2: Vec[A, m]) Vec[A, n+m] {
  malloc(n + m)  // ERROR: n and m aren't runtime values!
}
```

The bootstrap compiler will solve this through **monomorphization with call-site instantiation**, where:
- Dependent value parameters are actual runtime values inside function bodies
- Type checker does constant evaluation to extract concrete types at call sites
- Specialized versions are generated on-demand and memoized

## Forward Compatibility Constraint

Any dependent type syntax added now must be compatible with the full system. We cannot add syntax that looks like dependent types without having sufficient semantics to avoid painting ourselves into a corner.

## Minimum Viable Scope

### What We Need Now

1. **Type definitions with parameters**
   ```orion
   type Vec[A, n: Nat] = ...
   type Matrix[T, rows: Nat, cols: Nat] = ...
   ```
   - Square bracket syntax for type parameters
   - Type parameters: `A`, `T` (pure compile-time, erased after type checking)
   - Value parameters: `n: Nat` (runtime values with type constraints)
   - **Convention (not enforced)**: Type parameters should be UpperCase, value parameters should be lowerCase

2. **Dependent type syntax in expressions**
   ```orion
   let v: Vec[Int, 5]
   fn foo(m: Matrix[Float, 3, 4]) i32
   ```
   - Parser looks up type constructor to determine parameter kinds
   - No case convention needed - type definitions specify what each parameter is
   - Note: `String[n]` is not being implemented; `str` remains a primitive type

3. **Value parameters in function signatures**
   ```orion
   fn append(v1: Vec[A, n], v2: Vec[A, m]) Vec[A, n+m]
   fn make_vec(size: Nat) Vec[Int, size]
   ```
   - Value parameters like `n`, `m` appear in types
   - Same variables are accessible as runtime values in function body
   - Simple type-level arithmetic: `n+m`, `r*c`

4. **Runtime access to value parameters**
   ```orion
   fn append(v1: Vec[A, n], v2: Vec[A, m]) Vec[A, n+m] {
     let total: Nat = n + m  // n and m are runtime values here
     let result = vec_alloc(total)
     // ... copy elements ...
     return result
   }
   ```
   - Inside function body, `n` and `m` are actual runtime variables
   - Can be used in arithmetic, passed to functions, etc.

5. **Constant evaluation for @type(...)**
   ```orion
   let val = ptr_read(p, @type(u64))  // Type checker extracts u64
   ```
   - Type checker evaluates `@type(u64)` at compile time
   - Extracts concrete type for monomorphization

6. **Call-site monomorphization**
   ```orion
   fn ptr_read(ptr: Ptr, typ: Type) typ

   // Call site:
   let x = ptr_read(p, @type(u64))  // Generates ptr_read_U64: fn(Ptr) u64
   ```
   - Track all call sites during type checking
   - Infer concrete type arguments at each call
   - Generate specialized versions on-demand
   - Memoize to avoid duplicates

7. **Updated instance syntax**
   ```orion
   // Non-dependent (current)
   instance Pointer for i32 { ... }

   // Dependent (new)
   instance Pointer[T] for Vec[T, n] { ... }
   instance Eq[A] for Vec[A, n] { ... }
   ```
   - Square brackets on type class mean parameterized instance
   - Works with dependent types on the implementation side

### What We Can Defer

These features will be implemented in the self-hosted compiler:

- **Full type-level computation**: Complex arithmetic, conditionals, recursive functions
- **Where clauses**: `fn multiply(m1: Matrix[A, r1, c1], m2: Matrix[A, r2, c2]) Matrix[A, r1, c2] where c1 = r2`
- **Dependent pattern matching**: `match v { Vec[Int, 0] => ..., Vec[Int, n] => ... }`
- **Inference of value parameters**: Always require explicit value parameters (Option A from inference doc)
- **Array literal syntax**: Can be added later if needed

## Implementation Tasks

### 1. Parser Changes

**Type Definitions**:
- Parse `type Name[params...] = ...` syntax
- Distinguish type parameters (bare identifiers) from value parameters (with `: Type` annotation)
- Store parameter kinds in TypeDef AST node

**Type Expressions**:
- After parsing base type identifier, check for `[`
- Parse comma-separated parameters
- Look up type constructor to determine if each parameter should be parsed as type or expression
- Handle type-level arithmetic: `n+m`, `r*c`, etc.

**Function Signatures**:
- Return type can reference parameters: `fn foo(x: Vec[A, n]) Vec[A, n+1]`
- Simple arithmetic expressions in type positions

**Instance Declarations**:
- Parse optional `[params...]` after type class name
- `instance TypeClass[A] for Type[A, n] { ... }`

### 2. AST Changes

**TypeKind Extension**:
```zig
pub const TypeKind = union(enum) {
    primitive: PrimitiveType,
    tuple: []*Type,
    struct_type: []StructField,
    sum_type: []SumTypeVariant,
    named: []const u8,
    dependent: DependentType,  // NEW
};

pub const DependentType = struct {
    base: []const u8,              // "Vec", "String", "Matrix"
    type_params: []TypeParam,      // Type variables
    value_params: []Expr,          // Value expressions
};

pub const TypeParam = union(enum) {
    variable: []const u8,          // Type variable like "A"
    concrete: *Type,               // Concrete type like "Int"
};
```

**TypeDef Extension**:
```zig
pub const TypeDef = struct {
    name: []const u8,
    params: []TypeParameter,       // NEW: parameter specifications
    body: Type,
};

pub const TypeParameter = struct {
    name: []const u8,
    kind: ParameterKind,
};

pub const ParameterKind = union(enum) {
    type_param,                    // Pure type parameter
    value_param: Type,             // Value parameter with type constraint
};
```

**InstanceDecl Extension**:
```zig
pub const InstanceDecl = struct {
    class_name: []const u8,
    type_params: [][]const u8,     // NEW: type parameters for class
    implementing_type: Type,
    methods: []FunctionDecl,
};
```

### 3. Type Checker Changes

**Constant Evaluation**:
- Evaluate `@type(TypeName)` expressions at compile time
- Extract concrete type from Type value
- Use for monomorphization decisions

**Call-Site Tracking**:
- Track all calls to functions with dependent parameters
- Record concrete type arguments at each call site
- Build map of (function, concrete_types) -> needs_monomorphization

**Monomorphization**:
- Generate specialized function versions for each unique set of concrete types
- Mangle names: `ptr_read_U64`, `append_Int_3_2`
- Memoize to avoid duplicate generation
- Update call sites to use specialized versions

**Value Parameter Binding**:
- When calling `append(v1: Vec[Int, 3], v2: Vec[Int, 2])`, bind `A=Int`, `n=3`, `m=2`
- Make value bindings available as runtime values in function body
- Type check arithmetic expressions: `n+m` has type Nat

### 4. Codegen Changes

**Monomorphized Functions**:
- Generate LLVM IR for each specialized version
- Value parameters become actual function parameters in LLVM
- Type parameters are erased (already handled by specialization)

**Type-Level Arithmetic**:
- Evaluate `n+m` using runtime values
- Generate LLVM arithmetic instructions

**@ptr_read Type Signature**:
```orion
// Intrinsic remains compiler built-in, but now has dependent type signature:
@ptr_read: fn(ptr: Ptr, typ: Type) typ

// Usage (unchanged):
let val = @ptr_read(ptr, @type(u64))  // Type checker knows return type is u64
```

The intrinsic is still special-cased in codegen (generates LLVM load instruction), but the type checker can now properly express and validate its return type through the dependent type system.

## Example: Vec Append

```orion
// Type definition
type Vec[A, n: Nat] = ...

// Function using dependent types
fn append(v1: Vec[A, n], v2: Vec[A, m]) Vec[A, n+m] {
  // Inside here, n and m are runtime Nat values
  let total_len: Nat = n + m
  let result = vec_alloc(total_len)
  // ... copy elements from v1 and v2 ...
  return result
}

// Call site
let xs: Vec[Int, 3] = vec_from([1, 2, 3])
let ys: Vec[Int, 2] = vec_from([4, 5])
let zs = append(xs, ys)  // Type: Vec[Int, 5]
```

**What happens**:
1. Parser: `Vec[A, n]` looks up `Vec` type def, sees `A` is type param, `n: Nat` is value param
2. Type checker: At call site, infers `A=Int`, `n=3`, `m=2`, evaluates `n+m=5`
3. Monomorphization: Generates `append_Int_3_2` with `n` and `m` as i64 parameters
4. Codegen: LLVM function takes `(ptr, i64, ptr, i64) -> ptr`

## Success Criteria

The implementation is sufficient when we can:

1. ✅ Define `Vec[A, n: Nat]` in stdlib (note: `String[n]` is not being implemented; `str` remains primitive)
2. ✅ Write `append` that uses `n` and `m` as runtime values
3. ✅ Express `@ptr_read` intrinsic type signature as `fn(ptr: Ptr, typ: Type) typ`
4. ✅ Build self-hosting compiler using these features
5. ✅ All existing tests pass
6. ✅ No memory leaks

The self-hosted compiler will then implement:
- Full type-level computation
- Where clauses
- Dependent pattern matching
- Value parameter inference (if desired)
