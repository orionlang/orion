# Orion Bootstrap Compiler: Compilation Architecture

This document describes how the Orion bootstrap compiler (written in Zig) transforms Orion source code into executable binaries. The compiler implements a sophisticated two-pass multi-module compilation strategy with incremental caching, type checking, linearity inference, and LLVM-based code generation.

## Table of Contents

1. [Overview](#overview)
2. [Compilation Pipeline](#compilation-pipeline)
3. [Lexical Analysis](#lexical-analysis)
4. [Parsing & AST](#parsing--ast)
5. [Type System](#type-system)
6. [Linearity Inference](#linearity-inference)
7. [Code Generation](#code-generation)
8. [Optimization & Linking](#optimization--linking)
9. [Multi-Module Compilation](#multi-module-compilation)
10. [Stdlib Compilation & Caching](#stdlib-compilation--caching)
11. [Module Resolution](#module-resolution)
12. [Platform Support](#platform-support)
13. [Project Configuration](#project-configuration)
14. [Data Flow](#data-flow)

## Overview

The Orion bootstrap compiler is a multi-phase compiler that transforms Orion source code into machine code through the following major stages:

- **Lexical Analysis**: Source text → Tokens
- **Parsing**: Tokens → Abstract Syntax Tree (AST)
- **Module Discovery**: Resolve imports and build dependency graph
- **Linearity Inference**: Infer parameter usage annotations
- **Type Checking**: Validate types and resolve overloads
- **Code Generation**: AST → LLVM Intermediate Representation (IR)
- **Optimization**: Apply LLVM coroutine and backend passes
- **Object Code Generation**: LLVM IR → Native object files
- **Linking**: Combine object files into executable

The compiler implements a **two-pass strategy** for multi-module projects:
- **Pass 1**: Parse all modules to build complete AST graph
- **Pass 2**: Compile each module independently with full type information available

## Compilation Pipeline

### Entry Points

The compiler supports three invocation modes:

#### 1. Single File Compilation
```bash
orion <input.or> [--target triple] [--build-mode debug|release] [-c] [-S]
```

Direct compilation of an Orion source file. The compiler:
1. Discovers the project root by searching upward for `orion.toml`
2. Discovers all transitively imported modules via dependency graph analysis
3. Parses all modules (Pass 1)
4. Compiles each module independently (Pass 2)
5. Links all object files with stdlib and runtime
6. Generates executable

#### 2. Project Build
```bash
orion build [--release] [--lib]
```

Reads `orion.toml` manifest in the current directory or parent directories to identify the entrypoint, then invokes single-file compilation.

#### 3. Help
```bash
orion -h | --help
```

### Main Compilation Steps

**Location**: `bootstrap/src/main.zig`

```
1. Load and parse prelude (stdlib/prelude.or)
2. Discover entrypoint module dependencies via DependencyGraph
3. Load and parse all user modules (Pass 1)
4. For each module in topological order (Pass 2):
   a. Build combined AST (prelude + stdlib type defs + current module + extern declarations)
   b. Run linearity inference (Phase 1)
   c. Type checking (Phase 2)
   d. Code generation (Phase 3) → LLVM IR
   e. Run coroutine optimization passes → optimized IR
   f. Generate object file via llc backend
5. Optionally compile stdlib to stdlib.a (with hash-based caching)
6. Link all .o files + stdlib.a + runtime → executable
```

## Lexical Analysis

**Location**: `bootstrap/src/lexer.zig`

The lexer transforms source text into a stream of tokens. It handles:

### Token Categories

- **Keywords**: `fn`, `return`, `let`, `var`, `if`, `else`, `while`, `type`, `class`, `instance`, `import`, `async`, `spawn`, `select`, `recv`, `send`, `match`, `case`, etc.
- **Literals**: Integer constants, string literals
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`, `%`), comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`), logical (`&&`, `||`, `!`), bitwise (`&`, `|`, `^`, `<<`, `>>`), assignment (`=`), arrow (`->`)
- **Intrinsics**: `@` prefix (e.g., `@malloc`, `@ptr_read`)
- **Punctuation**: Parentheses, braces, brackets, semicolons, commas, colons, dots, pipes (`|`)

### Features

- **Position tracking**: Line and column numbers for error diagnostics
- **Comment handling**:
  - Line comments: `// ...`
  - Nested block comments: `/* ... */ /* nested */`
- **String literal escape sequences**: Standard C-like escaping

### Output

Produces `ArrayList(Token)` consumed by the parser.

## Parsing & AST

**Location**: `bootstrap/src/parser.zig`

The parser is the largest component of the compiler, responsible for transforming tokens into a typed abstract syntax tree.

### AST Structure

```zig
pub const AST = struct {
    imports: ArrayList(ImportDecl),
    extern_functions: ArrayList(ExternFunctionDecl),
    type_defs: ArrayList(TypeDef),
    class_defs: ArrayList(ClassDef),
    instances: ArrayList(InstanceDecl),
    functions: ArrayList(FunctionDecl),
};
```

### Key Node Types

#### Function Declaration
```zig
pub const FunctionDecl = struct {
    name: []const u8,
    params: []Param,
    return_type: Type,
    body: []Stmt,
    is_public: bool,
};
```

#### Type System
```zig
pub const Type = struct {
    kind: TypeKind,           // primitive | tuple | struct_type | sum_type | named | dependent
    usage: UsageAnnotation,   // once | exactly:N | optional | unlimited
    annotation_source: AnnotationSource,  // explicit | inferred
};
```

**Type kinds**:
- **Primitive**: Built-in types (`i64`, `i32`, `i8`, `ptr`, `()`)
- **Tuple**: `(T1, T2, ...)`
- **Struct**: Compound data types with named fields
- **Sum Type**: Variant types with constructors
- **Named**: Custom type references
- **Dependent**: Generic types with parameters (e.g., `Task[T]`)

**Usage annotations** track parameter multiplicity:
- `once`: Parameter used exactly once
- `exactly:N`: Parameter used exactly N times
- `optional`: Parameter may not be used
- `unlimited`: Parameter used multiple times

#### Statement
```zig
pub const Stmt = union(enum) {
    binding: Binding,          // let/var x: Type = expr
    if_stmt: IfStmt,          // if condition { ... } else { ... }
    while_loop: WhileLoop,    // while condition { ... }
    return_stmt: Expr,        // return expr
    match_stmt: MatchExpr,    // match expr { case variant => ... }
    unsafe_block: UnsafeBlock,  // unsafe { ... }
    expr_stmt: Expr,          // expression as statement
};
```

#### Expression
```zig
pub const Expr = union(enum) {
    integer_literal: i64,
    string_literal: []const u8,
    variable: []const u8,
    binary_op: BinaryOp,
    unary_op: UnaryOp,
    assignment: Assignment,
    function_call: FunctionCall,
    method_call: MethodCall,
    struct_literal: StructLiteral,
    match_expr: MatchExpr,
    if_expr: IfExpr,
    spawn: SpawnExpr,
    async_block: AsyncBlock,
    // ... additional expression forms
};
```

### Parsing Strategy

The parser uses **recursive descent** with **operator precedence climbing** for expressions:

1. **Primary expressions**: Literals, variables, parenthesized expressions
2. **Postfix operators**: Function calls, method calls, array indexing
3. **Unary operators**: Negation, dereference, pointer-of
4. **Binary operators** (in precedence order):
   - Multiplicative: `*`, `/`, `%`
   - Additive: `+`, `-`
   - Shift: `<<`, `>>`
   - Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
   - Logical AND: `&&`
   - Logical OR: `||`
   - Assignment: `=` (right-associative)

### Generic Types

The parser supports generic type parameters:
```orion
type Result[T] =
  | Ok(T)
  | Err(str)

fn map[T, U](result: Result[T], f: fn(T) U) Result[U] {
  match result {
    case Ok(x) => Ok(f(x))
    case Err(e) => Err(e)
  }
}
```

### Async and Concurrency

Supports Orion's async/concurrency model:
```orion
async {
  // Async block - can use await and spawn
  let task = spawn my_function(x)
  let result = await task
}
```

## Type System

**Location**: `bootstrap/src/typechecker.zig` (1932 lines)

The type checker validates that all expressions and statements are well-typed, consistent with declared types, and satisfy linearity constraints.

### TypeChecker State

```zig
pub const TypeChecker = struct {
    variables: StringHashMap(VariableInfo),          // In-scope variables
    functions: StringHashMap(FunctionSignature),     // Available functions
    type_defs: StringHashMap(TypeDefInfo),          // Type definitions
    instance_methods: StringHashMap(FunctionSignature),  // Method signatures
    in_unsafe_block: bool,                           // Unsafe context
    async_scope_depth: u32,                          // Async nesting
};
```

### Type Checking Responsibilities

1. **Expression type inference**: Determine type of all expressions
2. **Function signature collection**: Build table of available functions
3. **Type definition resolution**: Resolve custom type references
4. **Generic type instantiation**: Handle parametric types
5. **Instance method resolution**: Look up methods on types (typeclass-like)
6. **Linearity validation**: Ensure usage patterns match annotations
7. **Async scope validation**: Prevent blocking operations in async contexts
8. **Unsafe block context**: Track when unsafe operations are allowed

### External Signatures

For two-pass compilation, type information is available via `ExternalSignature`:

```zig
pub const ExternalSignature = struct {
    functions: StringHashMap(FunctionSignature),
    type_defs: StringHashMap(TypeDefInfo),
    instance_methods: StringHashMap(FunctionSignature),
};
```

This allows modules to reference functions from other modules before those modules are checked.

## Linearity Inference

**Location**: `bootstrap/src/linearity_inference.zig` (289 lines)

Linearity inference is **Phase 1** of the compilation pipeline. It analyzes each function independently to infer parameter usage annotations.

### Algorithm

For each parameter:

1. Count how many times it's used in the function body
2. Determine async context (in loops, async blocks)
3. Infer annotation:
   - 0 uses → `optional` (parameter not needed)
   - 1 use → `once` (used exactly once, moves value)
   - 2+ uses or in unbounded loop → `unlimited` (can be used multiple times)
   - Exactly N uses → `exactly:N` (used N times)

### Validation

- Respects explicit annotations (validates they match inferred)
- Suggests migrations via `orion-migrate` tool for annotation changes
- Tracks loop depth to detect unbounded loops (→ `unlimited`)

### Example

```orion
fn process(x: i64, y: i64) i64 {  // Inferred: x: once, y: optional
  return x + 5  // y not used
}

fn use_both(x: i64, y: i64) i64 {  // Inferred: x: once, y: once
  let sum = x + y
  return sum
}

fn repeat(x: i64) i64 {  // Inferred: x: unlimited
  while true {
    let val = x + 1  // Uses x in unbounded loop
  }
}
```

## Code Generation

**Location**: `bootstrap/src/codegen.zig`

The code generator is the second-largest compiler component, transforming type-checked AST into LLVM Intermediate Representation (IR).

### Code Generation State

```zig
pub const Codegen = struct {
    output: ArrayList(u8),                       // Accumulated LLVM IR text
    variables: StringHashMap(VarInfo),           // Local variable stack
    functions: StringHashMap(FunctionInfo),      // Function signatures
    string_literals: ArrayList(u8),              // Pooled string data
    defined_strings: StringHashMap([]const u8),  // String deduplication
    current_block: ?[]const u8,                  // Current basic block label
    block_terminated: bool,                      // Block needs more code?
    needs_coro_intrinsics: bool,                 // Use async features?
    needs_scheduler: bool,                       // Link runtime scheduler?
    generate_start: bool,                        // Generate _start entry?
    module_functions: ?[]const []const u8,       // For per-module filtering
    module_name: ?[]const u8,                    // For function name mangling
};
```

### IR Generation

The code generator produces **LLVM textual IR** targeting different platforms:

#### Linux (x86_64)
- Target triple: `x86_64-pc-linux-gnu`
- Entry point: Custom `_start` function that calls `__libc_start_main`
- Calling convention: System V AMD64 ABI

#### macOS (aarch64)
- Target triple: `aarch64-apple-darwin`
- Entry point: `_main` function
- Calling convention: ARM64 ABI

#### Windows (x86_64)
- Target triple: `x86_64-pc-windows-msvc` or `x86_64-pc-windows-gnu`
- Entry point: `main` function
- Calling convention: Microsoft x64 or System V AMD64

**Note**: Main development happens on `x86_64-pc-linux-gnu`.

### Key IR Generation Patterns

#### Local Variables
```llvm
%var_name = alloca i64          ; Stack allocation
store i64 42, ptr %var_name     ; Store value
%loaded = load i64, ptr %var_name  ; Load value
```

#### Function Calls
```llvm
%result = call i64 @function_name(i64 %arg1, i64 %arg2)
```

#### Binary Operations
```llvm
%sum = add i64 %a, %b
%product = mul i64 %a, %b
%cmp = icmp slt i64 %a, %b
```

#### Branches and Merging
```llvm
br i1 %condition, label %then_block, label %else_block

then_block:
  %then_val = add i64 %a, 1
  br label %merge_block

else_block:
  %else_val = add i64 %a, 2
  br label %merge_block

merge_block:
  %result = phi i64 [ %then_val, %then_block ], [ %else_val, %else_block ]
```

### Async & Coroutines

For async blocks, the code generator:
1. Generates LLVM coroutine intrinsics (`@llvm.coro.*`)
2. Marks blocks that can suspend
3. Tracks which runtime features are needed
4. Links `liborion_runtime.a` if scheduler is needed

```llvm
declare token @llvm.coro.id(i32, ptr, ptr, ptr)
declare i64 @llvm.coro.size.i64()
declare ptr @llvm.coro.begin(token, ptr)
declare token @llvm.coro.save(ptr)
declare i8 @llvm.coro.suspend(token, i1)
```

### Per-Module Code Generation

For multi-module compilation, the code generator:
1. Filters which functions to emit (only those in current module)
2. Mangles function names: `module_name__function_name`
3. Generates extern declarations for called functions from other modules
4. Never mangles the `main` function (entry point)

```zig
// Function name mangling
const func_name = if (self.module_name) |mod_name|
    if (std.mem.eql(u8, func.name, "main"))
        func.name  // main never mangled
    else
        std.fmt.allocPrint(..., "{s}__{s}", .{ mod_name, func.name })
else
    func.name;
```

### String Literal Handling

String literals are pooled into a single global section:

```llvm
@.str0 = private constant [5 x i8] c"hello\00"
@.str1 = private constant [6 x i8] c"world!\00"

; In function:
%ptr = getelementptr [5 x i8], ptr @.str0, i64 0, i64 0
```

## Optimization & Linking

### LLVM Optimization Passes

**Location**: `bootstrap/src/main.zig`

After LLVM IR generation, the compiler runs coroutine-specific optimization:

```bash
opt -S -passes=coro-early,coro-split,coro-elide,coro-cleanup \
    input.ll -o output.opt.ll
```

**Passes**:
- `coro-early`: Early coroutine lowering and analysis
- `coro-split`: Split coroutine into state machine
- `coro-elide`: Eliminate unnecessary heap allocations
- `coro-cleanup`: Final coroutine cleanup and verification

### Object File Generation

Uses LLVM's `llc` backend to compile optimized IR to object code:

```bash
llc -filetype=obj -mtriple=x86_64-pc-linux-gnu \
    input.opt.ll -o output.o
```

### Linking

**Location**: `bootstrap/src/main.zig`

The compiler links object files and libraries using platform-specific tools:

#### Linux
```bash
ld.lld -o executable \
  build/debug/main.o \
  build/debug/utils.o \
  ~/.orion/stdlib/x86_64-pc-linux-gnu/stdlib.a \
  /path/to/liborion_runtime.a \
  -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
  -lc -lpthread
```

#### macOS
```bash
clang -fuse-ld=lld -o executable \
  build/debug/main.o \
  build/debug/utils.o \
  ~/.orion/stdlib/aarch64-apple-darwin/stdlib.a \
  /path/to/liborion_runtime.a \
  -lpthread -framework CoreFoundation
```

#### Windows
```bash
clang-cl /Fe:executable.exe \
  build/debug/main.obj \
  build/debug/utils.obj \
  C:\orion\stdlib\x86_64-pc-windows-msvc\stdlib.lib \
  C:\orion\runtime\liborion_runtime.lib
```

### Entry Point Generation

For each platform, the compiler generates appropriate entry code:

**Linux (via `_start` function in LLVM IR)**:
```llvm
define void @_start() {
entry:
  %argc_ptr = call ptr asm "mov %rsp, $0", "=r"()
  %argc = load i32, ptr %argc_ptr
  %argv = getelementptr i8, ptr %argc_ptr, i64 8
  %ret = call i32 @__libc_start_main(
    ptr @main, i32 %argc, ptr %argv,
    ptr null, ptr null, ptr null, ptr null)
  unreachable
}
```

This replaces the C runtime startup code (`crt1.o`).

## Multi-Module Compilation

**Location**: `bootstrap/src/main.zig`

The compiler implements a sophisticated two-pass strategy for multi-module projects:

### Pass 1: Parsing All Modules

```
1. Discover entrypoint module
2. Build dependency graph (transitive closure of imports)
3. Perform topological sort
4. Parse all modules in dependency order
5. Store ASTs in StringHashMap<AST>
```

**Output**: All module ASTs available for Pass 2

### Pass 2: Per-Module Compilation

For each module in compilation order:

```
1. Build combined AST:
   a. Prelude declarations (types, classes, intrinsics)
   b. Stdlib type definitions (not implementations)
   c. Prelude's stdlib dependencies
   d. Current module's AST
   e. Type definitions from all other modules
   f. Extern function declarations for other modules

2. Run linearity inference

3. Type checking (with external signatures)

4. Code generation:
   - Filter: Only generate functions from current module
   - Mangle: Non-main functions get module prefix
   - Declare: Generate extern declarations for called functions

5. Optimization passes

6. Object file generation

7. Collect object file path for linking
```

### Module Function Name Mangling

Functions are mangled with their module name to avoid conflicts:

```
math.or:    add() → @math__add()
utils.or:   format() → @utils__format()
main.or:    main() → @main()  // Never mangled
```

The entry point function (`main`) is never mangled, as it must be discoverable at link time.

### Cross-Module Function Calls

When module A calls a function from module B:

1. **In module A's codegen**: Generate call to mangled name
   ```llvm
   %result = call i64 @math__add(i64 %a, i64 %b)
   ```

2. **In module B's codegen**: Generate function definition with mangled name
   ```llvm
   define i64 @math__add(i64 %a, i64 %b) {
     ...
   }
   ```

3. **At link time**: Linker resolves the call to the definition

### Type Information Sharing

Each module can reference types and instances defined in other modules:

```orion
// lib.or
import utils

type Config = { name: str, value: i64 }

instance Display[Config] {
  show = fn(c: Config) str { ... }
}

// main.or
import lib

fn main() i64 {
  let cfg = lib.Config { name: "test", value: 42 }
  // Can use Display instance even though it's defined in lib
  return cfg.value
}
```

## Stdlib Compilation & Caching

**Location**: `bootstrap/src/main.zig`

The standard library is compiled once and cached to accelerate builds.

### Cache Structure

```
~/.orion/stdlib/{target_triple}/
├── stdlib.a             # Static library archive
├── stdlib.o             # Object file
├── stdlib.ll            # Generated LLVM IR
├── stdlib.opt.ll        # Optimized IR
├── stdlib.hash          # Cache validity token
└── src/                 # Source files (optional for reference)
```

Example paths:
```
~/.orion/stdlib/x86_64-pc-linux-gnu/stdlib.a
~/.orion/stdlib/aarch64-apple-darwin/stdlib.a
~/.orion/stdlib/x86_64-pc-windows-msvc/stdlib.a
```

### Hash-Based Validation

The cache is validated using SHA256 hashing:

```zig
fn computeStdlibHash(allocator, stdlib_modules) ![]const u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    // Hash each module path to detect changes
    for (stdlib_modules) |module_path| {
        hasher.update(module_path);
        hasher.update("|");
    }

    var digest: [32]u8 = undefined;
    hasher.final(&digest);

    // Convert to hex string
    const hex_str = try allocator.alloc(u8, 64);
    for (digest, 0..) |byte, i| {
        _ = std.fmt.bufPrint(hex_str[i*2..i*2+2], "{x:0>2}", .{byte});
    }
    return hex_str;
}
```

### Compilation Logic

```
1. Compute hash of current stdlib module list
2. Check if stdlib.a exists and hash matches stored hash:
   a. If match: Use cached archive
   b. If no match or archive missing: Recompile
3. After compilation:
   a. Run coroutine optimization passes
   b. Generate object file
   c. Create archive: ar rcs stdlib.a stdlib.o
   d. Write hash to stdlib.hash for next build
```

### Benefits

- **Fast rebuilds**: No need to recompile stdlib on every build
- **Automatic invalidation**: Any change to stdlib forces recompilation
- **Per-target caching**: Different binaries for different architectures
- **No manual cache management**: Automatic validation

## Module Resolution

**Location**: `bootstrap/src/module.zig`

The module resolver translates import statements into file system paths.

### ModuleResolver

```zig
pub const ModuleResolver = struct {
    src_dir: []const u8,              // Project source directory
    stdlib_dir: []const u8,           // Standard library location
    include_dirs: []const []const u8, // External library search paths
};
```

### Resolution Algorithm

For import statement `import module.path.name`:

1. **Check if stdlib module** (starts with `std.`):
   - Look in `{stdlib_dir}/std/{path}.or`
   - E.g., `std.string` → `stdlib/std/string.or`

2. **User module** (relative import):
   - First try `{src_dir}/{path}.or`
   - If not found, try each `include_dir` in order
   - Path transformation: `"compiler.codegen"` → `"compiler/codegen.or"`

3. **Return ModuleId** if found, or error if not found

### Examples

```
Import "lexer"
  → {src_dir}/lexer.or

Import "std.string"
  → {stdlib_dir}/std/string.or

Import "compiler.codegen"
  → {src_dir}/compiler/codegen.or

Import "extern.mylib"
  → {include_dirs[0]}/extern/mylib.or (or subsequent dirs)
```

## Platform Support

**Location**: `bootstrap/src/target.zig`

The compiler supports cross-platform compilation via target triples.

### Target Triple Format

```
{architecture}-{vendor}-{os}-{environment}
```

Examples:
- `x86_64-pc-linux-gnu` - 64-bit x86 on Linux with GNU ABI
- `aarch64-apple-darwin` - ARM64 on macOS
- `x86_64-pc-windows-msvc` - 64-bit x86 on Windows with MSVC ABI
- `x86_64-pc-windows-gnu` - 64-bit x86 on Windows with MinGW
- `aarch64-linux-gnu` - ARM64 on Linux

### Platform-Specific Code Generation

#### Calling Conventions
- **Linux**: System V AMD64 ABI
- **macOS**: ARM64 ABI (aarch64) or x64 ABI (Intel)
- **Windows**: Microsoft x64 convention (MSVC) or System V (MinGW)

#### Entry Points
- **Linux**: `_start` (custom, replaces crt1.o)
- **macOS**: `_main`
- **Windows**: `main`

#### Dynamic Linking
- **Linux**: `/lib64/ld-linux-x86-64.so.2`
- **macOS**: System dyld
- **Windows**: No custom dynamic linker needed

#### Runtime Libraries
- **All platforms**: `-lpthread` for threading
- **macOS**: `-framework CoreFoundation` for system integration
- **Windows**: Standard Windows libraries (kernel32, etc.)

## Project Configuration

**Location**: `bootstrap/src/manifest.zig`

Projects are configured via `orion.toml` manifest files using a simple TOML subset.

### Manifest Format

```toml
[package]
name = "my_project"
version = "0.1.0"
description = "My Orion project"

[bin]
entrypoint = "src/main.or"

[lib]
entrypoint = "src/lib.or"

[dependencies]
mylib = { path = "./vendor/mylib" }
other = { git = "https://github.com/user/other.git" }
```

### Build Command Behavior

```bash
$ orion build             # Builds [bin] target
$ orion build --lib       # Builds [lib] target
$ orion build --release   # Debug symbols removed, optimizations enabled
```

## Data Flow

### Complete Compilation Flow

```
┌─ Input: .or source file
│
├─ Lexer: Text → Tokens
├─ Parser: Tokens → AST
│
├─ Module Discovery:
│  ├─ Read orion.toml (find project root)
│  ├─ Build dependency graph (transitive imports)
│  ├─ Topological sort (dependency order)
│  └─ Parse all modules (Pass 1)
│
├─ Stdlib Compilation (if needed):
│  ├─ Combine stdlib ASTs
│  ├─ Linearity inference
│  ├─ Type checking
│  ├─ Code generation → LLVM IR
│  ├─ Optimization passes
│  ├─ Object file generation → stdlib.o
│  ├─ Archive creation → stdlib.a
│  └─ Hash validation for next build
│
├─ Per-Module User Compilation (Pass 2):
│  ├─ For each module:
│  │  ├─ Combine AST (prelude + stdlib types + current + externs)
│  │  ├─ Linearity inference → Inferred annotations
│  │  ├─ Type checking → Type-checked AST
│  │  ├─ Code generation → LLVM IR
│  │  ├─ Optimization passes → Optimized IR
│  │  ├─ Object file generation → module.o
│  │  └─ Collect path for linking
│  └─ Repeat for next module
│
├─ Linking:
│  ├─ Collect all user .o files
│  ├─ Link with stdlib.a
│  ├─ Link with liborion_runtime.a (if async needed)
│  ├─ Link with system libraries (libc, pthreads, etc.)
│  └─ Generate executable
│
└─ Output: Executable binary
```

### Build Artifact Structure

```
project/
├── orion.toml              # Project manifest
├── src/
│  ├── main.or             # Entry point
│  ├── lib.or              # Module
│  └── utils.or            # Module
│
├── build/                 # Compiler output
│  ├── debug/              # Debug mode artifacts
│  │  ├── src/
│  │  │  ├── main.or.ll         (LLVM IR)
│  │  │  ├── main.or.opt.ll     (Optimized)
│  │  │  ├── main.or.o          (Object)
│  │  │  └── main               (Executable)
│  │  ├── lib.or.o
│  │  └── utils.or.o
│  │
│  └── release/            # Release mode artifacts
│     └── (same structure)
│
└── ~/.orion/stdlib/       # Global stdlib cache
   └── x86_64-pc-linux-gnu/
      ├── stdlib.a         (Cached archive)
      ├── stdlib.hash      (Validity check)
      ├── stdlib.o
      ├── stdlib.ll
      └── stdlib.opt.ll
```

## Summary

The Orion bootstrap compiler is a modern compiler that demonstrates:

1. **Proper separation of concerns**: Distinct phases for lexing, parsing, type checking, inference, and code generation
2. **Incremental compilation**: Two-pass strategy allows fast multi-module builds with per-module artifacts
3. **Intelligent caching**: Hash-based stdlib cache avoids unnecessary recompilation
4. **Cross-platform support**: Single codebase generates correct code for Linux, macOS, and Windows
5. **Modern IR**: Targets LLVM for maximum compatibility and optimization potential
6. **Type safety**: Sophisticated type system with generic types, typeclasses, and linearity constraints
7. **Async-first design**: Built-in support for structured concurrency via coroutines
8. **User-friendly configuration**: TOML-based project manifests for easy project setup

The compiler successfully transforms high-level Orion source code into efficient, type-safe executables through a well-designed pipeline of transformations.
