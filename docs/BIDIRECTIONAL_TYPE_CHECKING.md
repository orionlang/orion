# Bidirectional Type Checking for Orion

## Overview

This document outlines the plan to add bidirectional type checking to Orion's compiler. This enables type information to flow both **up** (inference/synthesis) and **down** (checking) through expressions, unlocking features like polymorphic literals and cleaner type coercion.

## Motivation

### Current Limitations

The current type system is purely bottom-up:

```
inferExprType: Expr → Type
```

This causes issues:

1. **Polymorphic literals impossible** - `"hello"` always infers as `str`, can't adapt to context
2. **Match arm coercion is hacky** - We post-hoc convert arm values instead of generating correct types upfront
3. **Integer literals need explicit types** - `let x: I64 = 42` should just work, but `42` infers as `I32`
4. **No typeclass-based conversions** - Can't use `StringLike[Rope]` to convert literals automatically

### What Bidirectional Enables

```orion
let r: Rope = "hello"           // Uses StringLike[Rope].from_str
let n: I64 = 42                 // Literal adopts expected type
let result: (I64, File) = match file {
  Open(fd) => (bytes, Open(fd)),
  Closed => (0, Closed)         // 0 checks against I64, not inferred as I32
}
```

## Interaction with Existing Type Features

### Dependent Types

Bidirectional checking **helps** dependent types. When you write:

```orion
let v: Vec[I64, 8] = ...
```

The expected type `Vec[I64, 8]` flows down, so the RHS knows both the element type *and* the size parameter. Without bidirectional checking, we'd need to infer the size from the expression, which is harder.

Checking mode validates that value parameters match:

```orion
let v: Vec[I64, 8] = make_vec[I64, 4]()  // Error: 8 != 4
```

### Linear Types

Linearity and bidirectional checking are **orthogonal**:

| System | Tracks |
|--------|--------|
| Bidirectional | Type information flow direction |
| Linear | Resource usage count (once vs unlimited) |

They can be checked independently:

```
check(expr, expected_type) → type correctness
linear_check(expr) → usage correctness
```

**Implicit conversions and linearity:**

When inserting typeclass-based conversions:

```orion
let r: Rope = some_string
// Desugars to: Rope.from_str(some_string)
```

The conversion *consumes* the original value. For linear types, this is correct - ownership transfers to the conversion function. For unlimited types (like `str`), no issue.

## Design

### Two Modes

```
infer(expr) → Type                    // Synthesis: what type does this have?
check(expr, expected) → Bool/Error    // Checking: does this match expected type?
```

### Mode Selection Rules

| Expression | Mode | Rationale |
|------------|------|-----------|
| `let x: T = e` | check(e, T) | Declared type flows into initializer |
| `let x = e` | infer(e) | No expected type, must synthesize |
| `return e` | check(e, return_type) | Function return type flows down |
| `f(e1, e2)` | check(e1, param1_type), check(e2, param2_type) | Parameter types flow into args |
| `if c { a } else { b }` | infer both, unify | Or check both against expected if available |
| `match x { arms }` | check each arm against expected | Expected type flows into all arms |
| `(a, b, c)` | check elements against expected tuple element types | Tuple type flows into elements |
| Integer literal | adopt expected type if compatible | `42` becomes I64 if expected |
| String literal | check for StringLike instance | `"hi"` becomes Rope if StringLike[Rope] exists |

### Subsumption and Conversion

When checking `check(expr, expected)`:

1. First, infer the expression's natural type: `actual = infer(expr)`
2. If `actual == expected`, success
3. If `actual` can be **subsumed** by `expected` (e.g., integer widening), success with implicit conversion
4. If there's a **typeclass instance** for conversion (e.g., `StringLike[expected]`), insert conversion call
5. Otherwise, type error

### Conversion Priority

1. Exact match (no conversion)
2. Primitive widening (I32 → I64, etc.)
3. Typeclass-based conversion (StringLike, Into, etc.)
4. Error

## Implementation Plan

### Phase 1: Infrastructure

**Goal:** Add expected type threading without changing behavior.

1. **Modify `generateExpression` signature:**
   ```zig
   fn generateExpression(self: *Codegen, expr: *const Expr, expected: ?Type) ![]const u8
   ```

2. **Thread `expected` through all call sites:**
   - Pass `null` everywhere initially (preserves current behavior)
   - Update all recursive calls

3. **Add `checkExpression` helper:**
   ```zig
   fn checkExpression(self: *Codegen, expr: *const Expr, expected: Type) ![]const u8 {
       const result = try self.generateExpression(expr, expected);
       // For now, just post-hoc convert if needed
       const actual = self.inferExprType(expr);
       return try self.convertIfNeeded(result, actual, expected);
   }
   ```

### Phase 2: Let Bindings

**Goal:** Declared types flow into initializers.

```orion
let x: I64 = 42  // 42 checks against I64
```

1. In `generateStatement` for `.var_decl`:
   - If type annotation exists, call `generateExpression(init, declared_type)`
   - Integer literals adopt expected type directly

2. **Modify integer literal codegen:**
   ```zig
   .integer_literal => |lit| {
       const target_type = expected orelse lit.inferred_type;
       // Generate with target_type instead of lit.inferred_type
   }
   ```

### Phase 3: Function Calls

**Goal:** Parameter types flow into arguments.

```orion
fn foo(x: I64) { ... }
foo(42)  // 42 checks against I64
```

1. Look up function signature before generating args
2. For each argument, call `generateExpression(arg, param_type)`

### Phase 4: Return Statements

**Goal:** Return type flows into returned expression.

```orion
fn bar() I64 {
  return 42  // 42 checks against I64
}
```

1. In `.return_stmt` handling, use `current_function_return_type` as expected type
2. Already have this field, just need to use it

### Phase 5: Match Expressions

**Goal:** Expected type flows into all arms, eliminating post-hoc conversion.

```orion
let x: (I64, File) = match file {
  Open(fd) => (bytes, Open(fd)),
  Closed => (0, Closed)  // checks against (I64, File)
}
```

1. Determine expected type (from context or first non-returning arm)
2. Generate each arm with `generateExpression(arm.body, expected)`
3. Remove post-hoc tuple conversion hack from current implementation

### Phase 6: Tuple Literals

**Goal:** Expected element types flow into tuple elements.

```orion
let t: (I64, str) = (42, "hello")
```

1. If expected type is tuple, destructure it
2. Generate each element with its expected element type
3. Integer/string literals adopt correct types automatically

### Phase 7: Typeclass-Based Conversion

**Goal:** Enable `StringLike` and similar conversion typeclasses.

```orion
class StringLike {
  from_str: fn(str) Self;
}

instance StringLike[Rope] { ... }

let r: Rope = "hello"  // Desugars to Rope.from_str("hello")
```

1. **In string literal codegen with expected type:**
   ```zig
   .string_literal => |lexeme| {
       if (expected) |exp| {
           if (exp.kind != .primitive or exp.kind.primitive != .str) {
               // Check for StringLike instance
               if (self.findInstance("StringLike", exp)) |instance| {
                   const str_val = try self.generateStringLiteral(lexeme);
                   return try self.generateMethodCall(exp, "from_str", &.{str_val});
               }
           }
       }
       return try self.generateStringLiteral(lexeme);
   }
   ```

2. **Instance lookup infrastructure:**
   - `findInstance(class_name, type) → ?InstanceDecl`
   - Need to track which instances exist for which types

3. **Generalize to other conversions:**
   - `Into` typeclass for general conversions
   - Numeric literals with `FromInt` or similar

### Phase 8: Dependent Type Integration

**Goal:** Value parameters validated during checking.

```orion
let v: Vec[I64, 8] = make_vec[I64, n]()  // Check n == 8
```

1. When checking against dependent type, extract value parameters
2. Compare value parameters for equality (or unify if variables)
3. Report clear errors on mismatch: "expected Vec[I64, 8], got Vec[I64, 4]"

### Phase 9: Cleanup

1. Remove `convertIfNeeded` calls that are now unnecessary
2. Remove match arm post-hoc conversion
3. Audit all type coercion points
4. Update error messages to be more precise

## Testing Strategy

### Unit Tests

Each phase should have tests:

```orion
// Phase 2: Let bindings
let a: I64 = 42
let b: I32 = 42
let c: U8 = 255

// Phase 3: Function calls
fn takes_i64(x: I64) I64 { return x }
let r: I64 = takes_i64(42)

// Phase 4: Return
fn returns_i64() I64 { return 42 }

// Phase 5: Match
type Option = | Some(I64) | None
fn unwrap_or(opt: Option, default: I64) I64 {
  return match opt {
    Some(x) => x,
    None => default
  }
}

// Phase 6: Tuples
let t: (I64, I64) = (1, 2)

// Phase 7: StringLike
let r: Rope = "hello"

// Phase 8: Dependent types
let v: Vec[I64, 8] = Vec[I64, 8].new()
```

### Regression Tests

- Ensure all existing tests still pass after each phase
- Add tests for edge cases discovered during implementation

### Linear Type Interaction Tests

```orion
// Verify conversions respect linearity
fn consume_rope(r: Rope) { ... }
consume_rope("hello")  // String converted and consumed

// Verify linear values can't be implicitly copied via conversion
let linear_thing: LinearType = ...
let converted: OtherType = linear_thing  // Should consume linear_thing
// linear_thing is now invalid
```

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Large change surface | Phased approach, each phase is self-contained |
| Performance regression | Expected type is just a pointer, minimal overhead |
| Inference ambiguity | Clear priority rules for conversion selection |
| Breaking existing code | Phase 1 preserves behavior, gradual opt-in |
| Dependent type complexity | Reuse existing value comparison logic |
| Linear type interference | Keep linearity checking separate |

## Open Questions

1. **Should we allow ambiguous conversions?** If both `StringLike[T]` and `Into[str, T]` exist, which wins?
   - Proposal: Error on ambiguity, require explicit conversion

2. **How deep does checking propagate?** For `let x: Vec[I64, 8] = vec_of_ints`, do we check element types?
   - Proposal: Start shallow, deepen as needed

3. **Interaction with type inference for generics?** If `fn id[T](x: T) T`, does `id(42)` infer T=I32 or adopt from context?
   - Proposal: Context wins if available, otherwise infer

4. **Dependent type unification:** When checking `Vec[T, n]` against `Vec[I64, 8]`, do we unify T=I64 and n=8?
   - Proposal: Yes, this enables inference of type/value parameters from context

## References

- [Bidirectional Typing (PFPL)](https://www.cs.cmu.edu/~rwh/pfpl/) - Pierce & Turner's foundational work
- [Complete and Easy Bidirectional Typechecking](https://arxiv.org/abs/1306.6032) - Dunfield & Krishnaswami
- [How to implement dependent types](http://davidchristiansen.dk/tutorials/nbe/) - Christiansen (uses bidirectional)
