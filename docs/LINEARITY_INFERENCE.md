# Local-Scope Linearity Inference

## Overview

Multiplicity annotations in Orion track how many times a parameter is used **directly in the current function's scope**. When a parameter is passed to another function, that counts as exactly one use—the called function's internal handling of that parameter is irrelevant to the caller's multiplicity contract.

This enables automatic inference with zero user annotation burden while preserving complete memory safety.

## Current State

Today, functions require explicit multiplicity annotations on each parameter:

```orion
fn peek(lexer: Lexer@4) u8 {
  // lexer used 4 times in this function's scope
  ...
}

fn advance(lexer: Lexer@9, c: u8@2) Lexer {
  // lexer used 9 times, c used 2 times in this scope
  ...
}

fn skip_whitespace(lexer: Lexer@7) Lexer {
  // lexer used 7 times in this scope
  ...
}
```

Users must manually count each direct use and annotate accordingly. This is tedious, error-prone, and creates friction when refactoring.

## Proposed Change

Remove the annotation burden by having the compiler **automatically infer multiplicity from function bodies**. Users write functions naturally; the compiler analyzes and assigns annotations.

### What Counts as a Direct Use

A parameter use is counted in the current function's scope only:

1. **Variable reference** - `peek(lexer)` counts as 1 use (passing ownership)
2. **Field/member access** - `lexer.source` counts as 1 use
3. **Method calls** - `lexer.len()` counts as 1 use
4. **Operators** - Any operation using the parameter counts as 1 use

### What Does NOT Count as a Direct Use

- **Internal behavior of called functions** - If `peek(lexer)` internally uses lexer 4 times, that's irrelevant. It's still 1 use from skip_whitespace's perspective.
- **Transitive uses** - The compiler never performs whole-program analysis (for linearity checking). Each function is independently analyzed.

### Example

```orion
fn skip_whitespace(lexer: Lexer) Lexer {
  let c = peek(lexer)          // use 1: passed to peek()

  if c == 255 {
    return lexer               // use 2: returned directly
  }

  // In this branch:
  return skip_whitespace(advance(lexer))  // use 3: passed to advance() + skip_whitespace()
}
```

The compiler infers `skip_whitespace` needs `Lexer@3` because lexer is directly used 3 times in this function's scope:
1. Passed to `peek`
2. Returned
3. Passed to `advance` (which is then returned from `skip_whitespace`)

It doesn't matter that:
- `peek` might use lexer 4 times internally
- `advance` might use lexer 9 times internally
- The recursive call to `skip_whitespace` might use it differently

Each function is a black box from the caller's perspective.

## Inference Rules

### Control Flow Handling

**Branches (if/else):**
```orion
fn example(x: Type) {
  if cond {
    use(x)      // use 1
  } else {
    use(x)      // use 2 (different branch, same parameter)
  }
}
// Inferred: Type@2 (both branches contribute to total count)
```

**Loops (bounded):**
```orion
fn iterate(x: Type) {
  var count = 0
  while count < 10 {
    use(x)      // use 1 per iteration
    count = count + 1
  }
}
// Inferred: Type@10 (statically determined to run 10 times)
```

**Loops (unbounded):**
```orion
fn skip_whitespace(x: Type) {
  while x.is_valid() {       // use 1: x used in condition
    process(x)               // x@ within loop is @*, not counted
  }
}
// Inferred: Type@1 (only the condition use counts; loop body uses are @*)
```

When a loop's iteration count cannot be statically determined, uses within the loop body are treated as `@*` (borrowed/unlimited within that scope) and do not contribute to the parameter's multiplicity count. Only uses in the loop condition are counted as direct uses.

**Pattern Matching:**
```orion
fn classify(x: Type) {
  match x {                 // use 1
    | Pattern1 => use(x)    // use 2
    | Pattern2 => use(x)    // use 3
    | Pattern3 => use(x)    // use 4
  }
}
// Inferred: Type@4 (each match arm, and match condition contributes)
```

### Return Values

Returning a parameter counts as one use:
```orion
fn identity(x: Type) Type {
  return x     // use 1
}
// Inferred: Type@1
```

Multiple return paths:
```orion
fn maybe_return(x: Type, cond: bool) Type {
  if cond {
    return x    // use 1
  } else {
    return x    // use 2
  }
}
// Inferred: Type@2
```

## Implementation Strategy

### Phase 1: Inference Engine
- Implement AST traversal that counts direct uses per parameter
- Handle all control flow constructs (if/else, match, loops)
- Generate inferred annotations for all functions
- Add compiler flag to output inferred annotations for verification

### Phase 2: Linearity Checking
- Verify inferred annotations against actual uses (currently done manually, now automated)
- Adjust linearity tracking to respect local-scope semantics
- Update error messages to reference local scope only

### Phase 3: Gradual Rollout
- Allow both explicit annotations (for documentation/intent) and inference
- Compiler prioritizes user annotations when provided
- Suggests inferred annotation when user annotation mismatches inference
- Tool to auto-migrate codebase (remove unnecessary annotations)

## Error Messages

When linearity checking fails, errors must be precise:

**Bad:**
```
error: parameter 'lexer' expected 4 uses, got 2
```

**Good:**
```
error: function 'peek' declares lexer@4 but only 2 uses found in scope
  - use 1: line 47, peek_ahead(lexer)
  - use 2: line 49, return lexer

  missing 2 uses. Did you forget to use lexer somewhere?
```

## Backward Compatibility

Existing code with explicit annotations continues to work. The compiler can:
1. Check that the explicit annotation matches the inferred annotation
2. Warn if they diverge
3. Allow optional override with `@*` or similar marker

## Benefits

1. **Zero annotation burden** - Users write natural code, compiler infers contracts
2. **Predictable** - Each function analyzed independently, no whole-program analysis surprises
3. **Refactoring-friendly** - Change a function body, compiler auto-updates the contract
4. **Clear semantics** - "One use" means "one direct use in my scope" universally
5. **Better error locality** - Errors point to specific missing/extra uses, not propagated through call chains

## Special Cases

### Recursive Functions

Recursive calls are treated identically to non-recursive calls: each call counts as 1 use. The multiplicity is counted per iteration, not cumulatively across all recursion depth.

```orion
fn skip_whitespace(lexer: Lexer) Lexer {
  let c = peek(lexer)        // use 1
  if c == 255 {
    return lexer             // use 2
  }
  return skip_whitespace(advance(lexer))  // use 3 (the recursive call is 1 use)
}
// Inferred: Lexer@3
```

The fact that this function may call itself 100 times recursively does not multiply the use count. Each invocation sees the same `Lexer@3` contract.

### Forward Declarations

Multiplicity inference is deferred until the full function definition is available. A function declared without a body cannot have its multiplicity inferred until the body is provided.

This mirrors how integer literals work in Orion—the type is determined when the literal is bound to a parameter or variable with known type, not before.

Similarly, a function's multiplicity is determined when its definition is complete and analyzed, not at declaration time.

