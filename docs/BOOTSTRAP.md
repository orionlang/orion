# Bootstrap Orion Language Specification

This document specifies the minimal bootstrap version of Orion - a small, simple language designed to be self-hosting. The bootstrap compiler is written in Zig and compiles this minimal language to LLVM IR. Once working, the full Orion compiler will be written in Bootstrap Orion.

## Design Philosophy

Bootstrap Orion is intentionally minimal:
- Linear types (ownership tracking without borrowing)
- Minimal type class system (no generics, no polymorphism)
- Minimal dependent types (type-level value parameters for fixed-size arrays)
- No closures
- No borrowing/references
- No advanced features

The goal is a language just powerful enough to write a compiler while teaching developers the core Orion ownership model.

---

## 1. Lexical Structure

### 1.1 Keywords

```
let var fn type class instance match if else while unsafe
return import pub extern true false
```

### 1.2 Operators

```
Arithmetic: + - * / %
Comparison: == != < > <= >=
Logical: && || !
Bitwise: & | ^ << >>
Assignment: =
Access: .
```

### 1.3 Delimiters

```
( ) { } [ ] , : ; |
```

### 1.4 Comments

```orion
// Line comment

/* Block comment */
```

### 1.5 Literals

**Integer literals:**
```orion
42          // Type inferred from context, defaults to i32
let x: i8 = 42;     // Literal infers i8 from context
let y: u64 = 1000;  // Literal infers u64 from context
let z = 42;         // No context: defaults to i32
```

**Semantics:**
- Integer literals are untyped until type inference
- Type is inferred from context (variable type annotation, function parameter type, etc.)
- When no context is available, literals default to i32
- Compile-time range checking ensures literals fit in target type
- Out-of-range literals cause compile error

**Examples:**
```orion
let a: i8 = 127;    // OK: 127 fits in i8 (-128 to 127)
let b: i8 = 200;    // ERROR: 200 doesn't fit in i8
let c: u8 = 255;    // OK: 255 fits in u8 (0 to 255)
let d: u8 = 300;    // ERROR: 300 doesn't fit in u8
```

**Not yet implemented:**
```orion
42i64       // Explicit type suffix (future)
0xFF        // Hexadecimal (future)
0b1010      // Binary (future)
```

**String literals:**
```orion
"hello"     // Null-terminated C string, type Ptr[u8]
```

**Boolean literals:**
```orion
true
false
```

---

## 2. Type System

### 2.1 Primitive Types

```orion
bool        // Boolean (1 bit, represented as i1 in LLVM)
i32         // 32-bit signed integer
i64         // 64-bit signed integer
u64         // 64-bit unsigned integer
u8          // 8-bit unsigned (for byte operations)
Unit        // Empty type, written as ()
```

**LLVM Representation:**
- `bool` → `i1`
- `i32` → `i32`
- `i64` → `i64`
- `u64` → `i64` (unsigned)
- `u8` → `i8`
- `Unit` → `i32` (always value 0)

### 2.2 Pointer Types

```orion
Ptr[T]      // Mutable pointer to T
```

**LLVM Representation:**
- All pointers → opaque LLVM pointer type

**Semantics:**
- Pointers are unsafe - no bounds checking
- Pointer arithmetic via `@ptr_offset` intrinsic
- Dereferencing via `@ptr_read` and `@ptr_write`

### 2.3 Struct Types

```orion
type Point = {
    x: i32,
    y: i32,
}
```

**LLVM Representation:**
- Struct types → LLVM struct with fields in declaration order
- Field access compiles to `getelementptr` + `load/store`

**Semantics:**
- Structs are value types (passed by value, copied)
- Fields accessed with `.` operator
- Fields immutable (no field mutation, but variables can be reassigned)

### 2.4 Sum Types (ADTs)

```orion
type Option =
    | None
    | Some(i32)

type Result =
    | Ok(i32)
    | Err(i32)
```

**LLVM Representation:**
```
struct ADT {
    i64 tag;           // Discriminant (0, 1, 2, ...)
    [payload_size x i8] data;  // Max size of all variants
}
```

**Semantics:**
- Tag determines which variant is active
- Payload stored in union (max size of all variants)
- Pattern matching via tag inspection
- Nullary constructors (e.g., `None`) have no payload

### 2.5 Function Types

```orion
fn(i32, i32) i64    // Function taking two I32s, returning i64
```

**LLVM Representation:**
- Function types → LLVM function type
- Functions are first-class (can be stored in variables)

**Semantics:**
- Functions are immutable
- No closures (cannot capture environment)
- Can be passed as pointers

### 2.6 Tuple Types

```orion
(i32, i64)              // Pair of i32 and i64
()                      // Unit type (empty tuple)
(bool, i32, Ptr[u8])   // Triple
```

**LLVM Representation:**
- Tuples → LLVM struct with anonymous fields
- Unit `()` → `i32` value 0

**Semantics:**
- Tuples are anonymous structs
- Access via numeric index: `tuple.0`, `tuple.1`
- Immutable

### 2.7 Linearity & Ownership

Bootstrap Orion uses **linear types** to enforce resource safety and prevent aliasing bugs.

**Linear by Default:**
- All composite types (structs, tuples, sum types) are linear
- All pointer types (`Ptr[T]`) are linear
- Values are consumed on use (single-use semantics)
- Once a value is used, it cannot be used again

**Copy Types (Exceptions):**
- Primitive types are Copy: `bool`, `i32`, `i64`, `u64`, `u8`
- Unit type `()` is Copy
- Copy types can be used multiple times without explicit copying

**Ownership Transfer:**
- No borrowing or references in bootstrap
- Function parameters consume their arguments
- Return values transfer ownership back to caller
- To "inspect" a value, functions must return it: `fn inspect(x: T) (Result, T)`

**Explicit Copying:**
- Linear types can implement the `Copy` class
- Call `.copy()` method to explicitly copy a value
- Copy creates a new, independent value

**Example:**
```orion
// Primitives are Copy - can use multiple times
let x: i32 = 42;
let y = x + x;  // OK: x is Copy type

// Structs are linear - consumed on use
let p = Point { x: 10, y: 20 };
let p2 = p;     // p consumed, cannot use p again
// let p3 = p;  // ERROR: p already consumed

// Explicit copy for reuse
let p = Point { x: 10, y: 20 };
let p2 = p.copy();  // Explicit copy (if Point instance Copy)
let p3 = p;         // OK: p still valid after copy
```

**Linearity Rules:**
1. Each linear value must be used exactly once
2. Passing to function consumes the value
3. Returning from function transfers ownership
4. Pattern matching consumes the matched value
5. Field access on linear types consumes the parent

### 2.8 Dependent Types (Minimal)

Bootstrap Orion includes minimal dependent types sufficient for expressing fixed-size arrays and vectors.

**Type Definition with Parameters:**
```orion
type Vec[A, n: u64] = i32
type Matrix[A, rows: u64, cols: u64] = i32
```

**Type Parameters:**
- Type parameters (e.g., `A`) - compile-time erased type variables
- Value parameters (e.g., `n: u64`) - compile-time constant values with types

**Using Dependent Types:**
```orion
fn make_vec() Vec[i32, 5] {
    return 42
}

let v: Vec[i32, 5] = make_vec()
```

**Instance Methods on Dependent Types:**
```orion
class VecOps {
    get_value: fn(Vec[A, n]) i32;
}

instance VecOps[Vec[i32, 5]] {
    get_value = fn(v: Vec[i32, 5]) i32 {
        return v
    }
}

let result = v.get_value()  // Calls Vec$$i32$5$$__get_value
```

**Name Mangling:**
- Dependent type instances use `$$` delimiter mangling
- Format: `TypeName$$type_param$value_param$$__method_name`
- Example: `Vec[i32, 5]` methods → `Vec$$i32$5$$__method_name`

**Limitations:**
- No type-level computation (can't do `Vec[A, n+1]`)
- Value parameters must be compile-time literals at instantiation
- Type erasure: value parameters don't exist at runtime
- No partial instantiation of type classes
- No dependent function types (return type can't depend on parameter value)

**LLVM Representation:**
- Dependent types resolve to their underlying type definition
- `Vec[i32, 5]` compiles identically to bare `i32` in this example
- Type parameters only affect name mangling for instance methods

---

## 3. Expressions

### 3.1 Literals

```orion
42              // Integer
true            // Boolean
false           // Boolean
"hello"         // String (Ptr[u8])
()              // Unit
```

### 3.2 Variables

```orion
x               // Variable reference
module.symbol   // Qualified name
```

**Linearity Semantics:**
- Using a linear variable **consumes** it
- The variable cannot be used again in the same scope
- Copy types (primitives) can be used multiple times

### 3.3 Binary Operations

```orion
a + b           // Addition
a - b           // Subtraction
a * b           // Multiplication
a / b           // Division
a % b           // Modulo
a == b          // Equality
a != b          // Inequality
a < b           // Less than
a > b           // Greater than
a <= b          // Less than or equal
a >= b          // Greater than or equal
a && b          // Logical AND (short-circuit)
a || b          // Logical OR (short-circuit)
a & b           // Bitwise AND
a | b           // Bitwise OR
a ^ b           // Bitwise XOR
a << b          // Left shift
a >> b          // Right shift
```

**LLVM Codegen:**
- Integer ops → `add`, `sub`, `mul`, `sdiv`/`udiv`, `srem`/`urem`
- Comparisons → `icmp` with appropriate predicate
- Logical AND/OR → control flow with phi nodes (short-circuit)
- Bitwise ops → `and`, `or`, `xor`, `shl`, `ashr`/`lshr`

### 3.4 Unary Operations

```orion
-x              // Negation
!x              // Logical NOT
```

### 3.5 Function Calls

```orion
f(x, y, z)      // Function call
module.f(x)     // Qualified function call
```

**Linearity Semantics:**
- Arguments are **consumed** by the function call
- Cannot use linear arguments after passing them
- To keep using a value, function must return it

**Example:**
```orion
fn process_and_return(p: Point) (i32, Point) {
    let sum = p.x + p.y;
    (sum, p)  // Return result + original value
}

let p = Point { x: 10, y: 20 };
let (result, p) = process_and_return(p);  // Thread p through
// Can use p again here
```

**LLVM Codegen:**
- Direct calls → `call` instruction
- All arguments passed by value (pointers are values)

### 3.6 Struct Literals

```orion
Point { x: 10, y: 20 }
```

**LLVM Codegen:**
- Create undef struct value
- Use `insertvalue` for each field

### 3.7 Field Access

```orion
point.x         // Field access
tuple.0         // Tuple element access
```

**Linearity Semantics:**
- Field access on Copy types: non-consuming (can reuse parent)
- Field access on linear types: **consumes the parent value**
- Accessing a field of a linear struct consumes the entire struct

**Example:**
```orion
// Copy type fields
let x: i32 = 42;
let y = x;  // x is Copy, can still use it
let z = x;  // OK

// Linear type field access
let p = Point { x: 10, y: 20 };
let x_val = p.x;  // p consumed, cannot use p again
// let y_val = p.y;  // ERROR: p already consumed

// To access multiple fields, destructure or thread through
fn get_both(p: Point) (i32, i32, Point) {
    let x = p.x;
    let y = p.y;
    (x, y, p)
}
```

**LLVM Codegen:**
- Field access → `extractvalue` for value types
- Tuple indexing → `extractvalue` with numeric index

### 3.8 Constructor Calls

```orion
Some(42)        // Sum type constructor with payload
None            // Nullary constructor
Ok(value)       // Result constructor
```

**LLVM Codegen:**
- Create struct with tag field
- Store payload in union data field
- Nullary constructors have tag only

### 3.9 If Expressions

```orion
if condition {
    expr1
} else {
    expr2
}
```

**Semantics:**
- Both branches must have same type
- Result is value of taken branch

**LLVM Codegen:**
- Conditional branch on condition
- Phi node to merge results

### 3.10 Match Expressions

```orion
match value {
    None => 0,
    Some(x) => x,
}
```

**Semantics:**
- Pattern matching on sum types
- Must be exhaustive
- Binds variables in patterns
- **Consumes the matched value**

**Linearity Semantics:**
- Matching consumes the scrutinee
- Bound variables in patterns take ownership of payload
- To keep original value, match must return it

**Example:**
```orion
// Simple match - consumes value
let opt = Some(42);
let result = match opt {
    None => 0,
    Some(x) => x,
};
// opt is consumed, cannot use it here

// Thread value through match
fn inspect_option(opt: Option) (bool, Option) {
    let has_value = match opt {
        None => false,
        Some(_) => true,
    };
    (has_value, opt)  // Return original value
}
```

**LLVM Codegen:**
- Extract tag field
- Switch on tag value
- Each case extracts payload and binds variables
- Phi node for result

### 3.11 Block Expressions

```orion
{
    let x = 10;
    let y = 20;
    x + y
}
```

**Semantics:**
- Sequence of statements
- Value of last expression is block value
- Creates new scope

### 3.12 While Loops

```orion
while condition {
    body
}
```

**Semantics:**
- Returns unit `()`
- No break/continue in bootstrap

**LLVM Codegen:**
- Loop header with condition check
- Loop body basic block
- Exit basic block

### 3.13 Return Expressions

```orion
return expr
return          // Returns ()
```

**Semantics:**
- Early return from function
- Type must match function return type

### 3.14 Unsafe Blocks

```orion
unsafe {
    @ptr_write(ptr, 42)
}
```

**Semantics:**
- Required for pointer operations
- Signals to reader that invariants may be violated

### 3.15 Intrinsic Calls

```orion
@ptr_of(value)              // Get pointer to value
@ptr_read(ptr)              // Read from pointer
@ptr_write(ptr, value)      // Write to pointer
@ptr_offset(ptr, offset)    // Pointer arithmetic
```

**LLVM Codegen:**
- `@ptr_of` → `alloca` + `store`
- `@ptr_read` → `load`
- `@ptr_write` → `store`
- `@ptr_offset` → `getelementptr`

---

## 4. Statements

### 4.1 Let Bindings

```orion
let x = expr;               // Immutable binding
let x: i32 = expr;         // With type annotation
let (a, b) = tuple;        // Tuple destructuring
```

**Semantics:**
- `let` bindings are immutable (cannot be reassigned)
- `var` bindings are mutable and can be reassigned with `=`
- Variables are stack-allocated
- Type inference if annotation omitted
- **Each binding must be used exactly once** (if linear)
- **No shadowing** - each name can only be bound once per scope

**Linearity Semantics:**
```orion
// Copy types can be used multiple times
let x: i32 = 42;
let y = x;
let z = x;  // OK: i32 is Copy

// Linear types consumed on use
let p = Point { x: 10, y: 20 };
let p2 = p;  // p consumed
// let p3 = p;  // ERROR: p already consumed

// Single use - must use the binding
let p = Point { x: 10, y: 20 };
// ERROR if p is never used

// No shadowing
let x = 10;
// let x = 20;  // ERROR: x already bound in this scope
```

**LLVM Codegen:**
- Value stored in `alloca` stack slot
- Subsequent uses `load` from slot

### 4.2 Assignment Statements

```orion
x = expr;           // Reassign existing mutable variable
counter = counter + 1;
```

**Semantics:**
- Variable must already exist and be declared with `var` (not `let`)
- `let` bindings are immutable and cannot be reassigned
- Type of expression must match variable's declared type
- Linear values are consumed by assignment (old value dropped)

**Example:**
```orion
fn main() i32 {
    var x: i32 = 5;   // Mutable variable
    x = 10;           // OK: reassign to new value
    x = x * 2;        // OK: use and reassign

    let y: i32 = 5;   // Immutable binding
    // y = 10;        // ERROR: cannot assign to immutable variable

    return x
}
```

### 4.3 Expression Statements

```orion
f(x);
some_function();
```

---

## 5. Declarations

### 5.1 Type Definitions

**Struct types:**
```orion
type Point = {
    x: i32,
    y: i32,
}

pub type Rect = {
    top_left: Point,
    bottom_right: Point,
}
```

**Sum types:**
```orion
type Option =
    | None
    | Some(i32)

type Result =
    | Ok(i32)
    | Err(i32)
```

**Type aliases:**
```orion
type Size = u64
```

**Semantics:**
- `pub` makes type visible to other modules
- Type names are `PascalCase` by convention
- Constructors are functions

### 5.2 Function Definitions

```orion
fn add(x: i32, y: i32) i32 {
    x + y
}

pub fn multiply(x: i32, y: i32) i32 {
    x * y
}

unsafe fn read_ptr(ptr: Ptr[i32]) i32 {
    @ptr_read(ptr)
}
```

**Semantics:**
- `pub` makes function visible to other modules
- `unsafe` allows intrinsic calls
- Function names are `snake_case` by convention
- Parameters are immutable
- **Parameters consume their arguments** (ownership transfer)
- **Return values transfer ownership** to caller

**Linearity & Ownership:**
```orion
// Simple ownership transfer
fn consume(p: Point) i32 {
    p.x + p.y  // p consumed here
}

let point = Point { x: 10, y: 20 };
let sum = consume(point);  // point consumed, can't use it after
// let x = point.x;  // ERROR: point already consumed

// Threading pattern - inspect and return
fn inspect_point(p: Point) (i32, Point) {
    let sum = p.x + p.y;
    (sum, p)  // Return result and original value
}

let point = Point { x: 10, y: 20 };
let (sum, point) = inspect_point(point);  // Thread through
// Can use point again here

// Multiple linear parameters
fn combine(p1: Point, p2: Point) Point {
    Point { x: p1.x + p2.x, y: p1.y + p2.y }
}

let p1 = Point { x: 1, y: 2 };
let p2 = Point { x: 3, y: 4 };
let p3 = combine(p1, p2);  // Both p1 and p2 consumed
```

**LLVM Codegen:**
- Each function → LLVM function definition
- Mangled name: `module__function`
- Parameters passed by value

### 5.3 Extern Declarations

```orion
extern "C" {
    fn malloc(size: u64) Ptr[u8]
    fn free(ptr: Ptr[u8])
    fn printf(fmt: Ptr[u8], ...) i32
}

pub extern "C" {
    fn exported_func(x: i32) i32
}
```

**Semantics:**
- Declares foreign functions (typically C)
- No body provided
- ABI specified as string literal

**LLVM Codegen:**
- External function declaration
- Uses specified ABI (C calling convention)

### 5.4 Type Class Definitions

Bootstrap Orion has a **minimal type class system** for basic polymorphism.

**Class Declaration:**
```orion
class Copy {
    copy: fn(Self) Self
}

class Eq {
    eq: fn(Self, Self) bool
}

class Display {
    display: fn(Self) Ptr[u8]
}
```

**Class Semantics:**
- Declares a set of methods a type must implement
- `Self` refers to the implementing type
- No default methods in bootstrap
- No associated types
- No class constraints (no `class Ord where Eq`)

**Instance Blocks:**
```orion
type Point = {
    x: i32,
    y: i32,
}

instance Copy[Point] {
    copy = fn(self: Point) Point {
        Point { x: self.x, y: self.y }
    }
}

instance Eq[Point] {
    eq = fn(self: Point, other: Point) bool {
        self.x == other.x && self.y == other.y
    }
}
```

**Instance Semantics:**
- Implements all methods declared in the class
- Methods become available on the type
- Called with method syntax: `point.copy()`, `point.eq(other)`
- **Self parameter consumed** (follows linearity rules)

**Built-in Classes:**
- `Copy`: Enables explicit copying via `.copy()`
- `Eq`: Equality comparison
- `Display`: String representation (for debugging)

**Linearity & Classes:**
```orion
// Using Copy class
let p1 = Point { x: 10, y: 20 };
let p2 = p1.copy();  // Explicit copy
let p3 = p1;         // p1 moved (consumed)

// Class methods consume self
let p1 = Point { x: 10, y: 20 };
let p2 = Point { x: 10, y: 20 };
let is_equal = p1.eq(p2);  // Both p1 and p2 consumed
```

**LLVM Codegen:**
- Class methods → Regular functions with mangled names
- Method calls → Direct function calls
- No dynamic dispatch (classes are compile-time only)
- Monomorphization style (like Haskell type classes without polymorphism)

---

## 6. Modules

### 6.1 Module Structure

Each `.or` file is a module. Module name is derived from file path:
- `src/main.or` → module `main`
- `src/compiler/lexer.or` → module `compiler.lexer`

### 6.2 Imports

```orion
import std.mem              // Import module
import compiler.lexer       // Import submodule
```

**Semantics:**
- Imports make module symbols available
- Qualified access: `mem.malloc()`
- No wildcard imports in bootstrap

### 6.3 Exports

```orion
pub type Node = { ... }     // Exported type
pub fn parse() Node { }  // Exported function
```

**Semantics:**
- Only `pub` items visible to other modules
- By default, items are module-private

---

## 7. Intrinsics

Bootstrap Orion has exactly 4 intrinsics:

### 7.1 @ptr_of

```orion
@ptr_of(value) Ptr[T]
```

**Semantics:**
- Allocates `value` on stack
- Returns pointer to stack allocation

**LLVM Codegen:**
```llvm
%ptr = alloca T
store T %value, ptr %ptr
; return %ptr
```

**Example:**
```orion
unsafe {
    let x = 42;
    let ptr = @ptr_of(x);
}
```

### 7.2 @ptr_read

```orion
@ptr_read(ptr: Ptr[T]) T
```

**Semantics:**
- Dereferences pointer
- Returns value at pointer location
- Unsafe: no bounds checking

**LLVM Codegen:**
```llvm
%value = load T, ptr %ptr
; return %value
```

**Example:**
```orion
unsafe {
    let value = @ptr_read(ptr);
}
```

### 7.3 @ptr_write

```orion
@ptr_write(ptr: Ptr[T], value: T) ()
```

**Semantics:**
- Writes `value` to `ptr` location
- Returns unit
- Unsafe: no bounds checking

**LLVM Codegen:**
```llvm
store T %value, ptr %ptr
; return i32 0  (unit)
```

**Example:**
```orion
unsafe {
    @ptr_write(ptr, 42);
}
```

### 7.4 @ptr_offset

```orion
@ptr_offset(ptr: Ptr[T], offset: i64) Ptr[T]
```

**Semantics:**
- Pointer arithmetic: `ptr + offset * sizeof(T)`
- Returns new pointer
- Unsafe: no bounds checking

**LLVM Codegen:**
```llvm
%new_ptr = getelementptr T, ptr %ptr, i64 %offset
; return %new_ptr
```

**Example:**
```orion
unsafe {
    let elem_ptr = @ptr_offset(array_ptr, 5);
}
```

---

## 8. Memory Model

### 8.1 Stack Allocation

- Let bindings allocate on stack
- Function parameters on stack
- Automatically freed at scope exit

**LLVM:**
- Uses `alloca` in entry block
- LLVM handles stack frame management

### 8.2 Heap Allocation

- Only via FFI (e.g., `malloc`/`free`)
- No automatic memory management
- Programmer responsible for deallocation

### 8.3 Ownership Semantics

Bootstrap Orion has no ownership system:
- All values copied on assignment
- No move semantics
- No borrow checking
- Pointers are just addresses (like C)

---

## 9. Type Checking

### 9.1 Type Inference

**Variable bindings:**
- Let/var bindings infer type from RHS expression
- If type annotation present, RHS must match (or be contextually typed)

**Integer literals:**
- Integer literals are untyped until inference
- Type inferred from context (variable annotation, parameter type, etc.)
- Defaults to i32 when no context available
- Compile-time range checking validates literal fits in target type

**Function return types:**
- Must be explicitly annotated (no inference from body in bootstrap)

**Limitations:**
- No cross-function inference
- No generic type inference
- No bidirectional type inference (beyond literals)

### 9.2 Type Compatibility

- No implicit conversions between different types
- No subtyping
- Exact type match required (exception: contextual typing of literals)
- bool can be used in integer contexts (converts to 0 or 1)

### 9.3 Linearity Checking

The type checker must enforce linear type discipline:

**Single-Use Enforcement:**
- Each linear variable used exactly once per scope
- Error if used multiple times
- Error if never used (unused value)

**Copy Type Detection:**
- Primitives (`bool`, `i32`, `i64`, `u64`, `u8`) are Copy
- Unit `()` is Copy
- Types that `instance Copy` are Copy
- All other types are linear

**Consumption Tracking:**
```orion
// Type checker tracks usage
let p = Point { x: 10, y: 20 };  // p: Point (linear)
let x = p->x;                      // p consumed here
let y = p->y;                      // ERROR: p already consumed

// OK with Copy type
let n: i32 = 42;  // n: i32 (Copy)
let a = n;        // n still available
let b = n;        // OK: i32 is Copy
```

**Function Call Checking:**
- Arguments must be available (not consumed)
- After call, arguments marked as consumed (if linear)
- Return value introduces new binding

**Pattern Match Exhaustiveness:**
- Must cover all variants
- Bound variables take ownership of payloads

**Class Constraint Checking:**
- Method calls require class instance
- `.copy()` requires `Copy` instance
- `.eq()` requires `Eq` instance
- Compile error if class not instanced

### 9.4 Type Errors

Common type errors:
- Type mismatch in assignment
- Wrong number/type of function arguments
- Non-exhaustive pattern match
- Field doesn't exist
- Unsafe operations outside unsafe block
- **Linear value used multiple times**
- **Linear value never used**
- **Variable shadowing (not allowed)**
- **Class method called on type without instance**
- **Class instance missing required methods**

---

## 10. Compilation Model

### 10.1 Compilation Phases

1. **Lexing**: Source → Tokens
2. **Parsing**: Tokens → AST
3. **Module Resolution**: Resolve imports, build dependency graph
4. **Type Checking**: Check types, resolve symbols
5. **LLVM IR Generation**: AST → LLVM IR
6. **LLVM Optimization**: Run LLVM optimization passes
7. **Native Code Generation**: LLVM IR → Object file
8. **Linking**: Object files → Executable

### 10.2 Name Mangling

Functions mangled as: `module__function_name`

Example:
- `compiler.lexer.tokenize` → `compiler.lexer__tokenize`

### 10.3 Entry Point

Bootstrap programs must have:
```orion
fn main() i32 {
    // Program entry point
}
```

Returns i32 exit code (0 = success).

---

## 11. Standard Library (Minimal)

Bootstrap standard library provides only:

### 11.1 Memory Management

```orion
// In module: std.mem
extern "C" {
    fn malloc(size: u64) Ptr[u8]
    fn free(ptr: Ptr[u8])
    fn memcpy(dest: Ptr[u8], src: Ptr[u8], n: u64) Ptr[u8]
}
```

### 11.2 I/O

```orion
// In module: std.io
extern "C" {
    fn putchar(c: i32) i32
    fn puts(s: Ptr[u8]) i32
    fn printf(fmt: Ptr[u8], ...) i32
}
```

### 11.3 Core Types

```orion
// In module: std.core

type Option =
    | None
    | Some(i32)

type Result =
    | Ok(i32)
    | Err(i32)
```

That's it. Everything else (strings, vectors, hash maps, etc.) must be implemented by the bootstrap compiler as needed.

---

## 12. Limitations & Non-Features

Bootstrap Orion intentionally **does not have**:

### Not Included:
- ❌ Variable mutation (no `var`, no assignment `=`)
- ❌ Generics/polymorphism
- ❌ Full dependent types (type-level computation, runtime dependencies)
- ❌ Borrowing/references (`&T`, `&mut T`)
- ❌ Closures (cannot capture environment)
- ❌ Trait objects / dynamic dispatch
- ❌ Automatic memory management
- ❌ String type (use `Ptr[u8]`)
- ❌ For loops (use `while`)
- ❌ Break/continue
- ❌ Defer statements
- ❌ Pattern guards
- ❌ Operator overloading
- ❌ Implicit coercions
- ❌ Macros
- ❌ Variable shadowing

### Included (Minimal Features):
- ✅ **Linear types** (values consumed on use)
- ✅ **Minimal type class system** (Copy, Eq, Display)
- ✅ **Method syntax** (for class methods only)
- ✅ **Ownership transfer** (no borrowing, thread values through functions)
- ✅ **Minimal dependent types** (compile-time value parameters for fixed-size arrays)

---

## 13. Example Programs

### 13.1 Hello World

```orion
import std.io

fn main() i32 {
    unsafe {
        io.puts("Hello, World!")
    }
}
```

### 13.2 Factorial

```orion
fn factorial(n: i32) i32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() i32 {
    factorial(5)
}
```

### 13.3 Option Type

```orion
type Option =
    | None
    | Some(i32)

fn divide(a: i32, b: i32) Option {
    if b == 0 {
        None
    } else {
        Some(a / b)
    }
}

fn main() i32 {
    let result = divide(10, 2);
    match result {
        None => 0,
        Some(x) => x,
    }
    // result consumed by match, cannot use it again
}
```

### 13.4 Linear Types & Threading

```orion
type Point = {
    x: i32,
    y: i32,
}

// Implement Copy class to allow explicit copying
instance Copy[Point] {
    copy = fn(self: Point) Point {
        Point { x: self.x, y: self.y }
    }
}

// Function consumes Point, returns result and Point
fn distance_squared(p: Point) (i32, Point) {
    let dx = p.x;
    let dy = p.y;
    let dist = dx * dx + dy * dy;
    (dist, p)
}

fn main() i32 {
    let p1 = Point { x: 3, y: 4 };

    // Thread p1 through to keep using it
    let (dist, p1) = distance_squared(p1);

    // Or make a copy if needed multiple times
    let p2 = p1.copy();

    // Now have both p1 and p2
    let (d1, p1) = distance_squared(p1);
    let (d2, p2) = distance_squared(p2);

    d1 + d2
}
```

### 13.5 Linked List

```orion
import std.mem

type List =
    | Nil
    | Cons(i32, Ptr[List])

fn cons(value: i32, rest: Ptr[List]) Ptr[List] {
    unsafe {
        let node = mem.malloc(16);  // size of List struct
        let list_ptr = node;
        // Manually construct Cons variant
        @ptr_write(list_ptr, 1);  // tag = 1 for Cons
        let payload_ptr = @ptr_offset(list_ptr, 1);
        @ptr_write(payload_ptr, value);
        let next_ptr = @ptr_offset(payload_ptr, 1);
        @ptr_write(next_ptr, rest);
        list_ptr
    }
}

fn sum_list(list: Ptr[List]) i32 {
    unsafe {
        let tag = @ptr_read(list);
        if tag == 0 {
            // Nil case
            0
        } else {
            // Cons case
            let payload_ptr = @ptr_offset(list, 1);
            let value = @ptr_read(payload_ptr);
            let next_ptr = @ptr_offset(payload_ptr, 1);
            let rest = @ptr_read(next_ptr);
            value + sum_list(rest)
        }
    }
}
```

---

## 14. Grammar (EBNF)

```ebnf
program = { import | declaration } ;

import = "import" qualified_name ";" ;

declaration = type_def | fn_def | extern_block | class_def | class_instance ;

type_def = [ "pub" ] "type" IDENT [ type_params ] "=" type_body ;
type_params = "[" type_param { "," type_param } "]" ;
type_param = IDENT [ ":" type ] ;  (* IDENT alone = type param, IDENT : type = value param *)
type_body = struct_type | sum_type | type ;

struct_type = "{" [ field { "," field } ] "}" ;
field = IDENT ":" type ;

sum_type = "|" variant { "|" variant } ;
variant = IDENT [ "(" type { "," type } ")" ] ;

fn_def = [ "pub" ] [ "unsafe" ] "fn" IDENT "(" params ")" type block ;
params = [ param { "," param } ] ;
param = IDENT ":" type ;

extern_block = [ "pub" ] "extern" STRING "{" { extern_fn } "}" ;
extern_fn = "fn" IDENT "(" params ")" [ type ] ;

class_def = "class" IDENT "{" { class_method } "}" ;
class_method = IDENT ":" "fn" "(" "Self" { "," type } ")" [ type ] ";" ;

class_instance = "instance" IDENT "[" type "]" "{" { method_impl } "}" ;
method_impl = IDENT "=" fn_literal ;

type = IDENT [ "[" type_args "]" ]   (* Named or dependent type *)
     | "Ptr" "[" type "]"            (* Pointer type *)
     | "(" [ type { "," type } ] ")" (* Tuple type *)
     | "fn" "(" [ type { "," type } ] ")" type (* Function type *)
     | "Self" ;                      (* Self type in trait *)

type_args = type_arg { "," type_arg } ;
type_arg = type | expr ;             (* Heuristically determined: types or value expressions *)

stmt = let_binding
     | expr ";" ;

let_binding = "let" pattern [ ":" type ] "=" expr ";" ;

pattern = IDENT
        | "(" [ pattern { "," pattern } ] ")" ;

block = "{" { stmt } [ expr ] "}" ;

expr = literal
     | IDENT
     | qualified_name
     | tuple
     | struct_literal
     | constructor_call
     | fn_call
     | field_access
     | tuple_index
     | binary_op
     | unary_op
     | if_expr
     | match_expr
     | while_expr
     | return_expr
     | unsafe_block
     | intrinsic_call
     | "(" expr ")"
     | block ;

literal = INTEGER | STRING | "true" | "false" | "()" ;

tuple = "(" expr "," expr { "," expr } ")" ;

struct_literal = IDENT "{" [ field_init { "," field_init } ] "}" ;
field_init = IDENT ":" expr ;

constructor_call = IDENT "(" [ expr { "," expr } ] ")" ;

fn_call = expr "(" [ expr { "," expr } ] ")" ;

method_call = expr "." IDENT "(" [ expr { "," expr } ] ")" ;

field_access = expr "->" IDENT ;

tuple_index = expr "." INTEGER ;

binary_op = expr BINOP expr ;
BINOP = "+" | "-" | "*" | "/" | "%"
      | "==" | "!=" | "<" | ">" | "<=" | ">="
      | "&&" | "||"
      | "&" | "|" | "^" | "<<" | ">>" ;

unary_op = UNOP expr ;
UNOP = "-" | "!" ;

if_expr = "if" expr block [ "else" block ] ;

match_expr = "match" expr "{" { match_arm } "}" ;
match_arm = pattern "=>" expr "," ;

while_expr = "while" expr block ;

return_expr = "return" [ expr ] ;

unsafe_block = "unsafe" block ;

intrinsic_call = "@" IDENT "(" [ expr { "," expr } ] ")" ;

qualified_name = IDENT { "." IDENT } ;
```

---

## Appendix A: LLVM IR Examples

### A.1 Simple Function

**Orion:**
```orion
fn add(x: i32, y: i32) i32 {
    x + y
}
```

**LLVM IR:**
```llvm
define i32 @main__add(i32 %x, i32 %y) {
entry:
    %result = add i32 %x, %y
    ret i32 %result
}
```

### A.2 Struct

**Orion:**
```orion
type Point = { x: i32, y: i32 }

fn make_point(x: i32, y: i32) Point {
    Point { x: x, y: y }
}
```

**LLVM IR:**
```llvm
%Point = type { i32, i32 }

define %Point @main__make_point(i32 %x, i32 %y) {
entry:
    %0 = insertvalue %Point undef, i32 %x, 0
    %1 = insertvalue %Point %0, i32 %y, 1
    ret %Point %1
}
```

### A.3 Sum Type

**Orion:**
```orion
type Option = None | Some(i32)
```

**LLVM IR:**
```llvm
%Option = type { i64, [4 x i8] }  ; tag + max(payload sizes)

; None constructor
define %Option @main__None() {
entry:
    %0 = insertvalue %Option undef, i64 0, 0
    ret %Option %0
}

; Some constructor
define %Option @main__Some(i32 %value) {
entry:
    %0 = insertvalue %Option undef, i64 1, 0
    %1 = bitcast [4 x i8]* to i32*
    store i32 %value, i32* %1
    %2 = load [4 x i8], [4 x i8]* ...
    %3 = insertvalue %Option %0, [4 x i8] %2, 1
    ret %Option %3
}
```

---

**End of Bootstrap Orion Specification**
