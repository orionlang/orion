# Orion Bootstrap Compiler

A minimal bootstrap compiler for the Orion programming language, written in Zig.
This compiler implements a small subset of Orion (Bootstrap Orion) that is just
powerful enough to be self-hosting.

## What is Orion?

Orion is a systems programming language with linear types and ownership
tracking. The bootstrap version implements the core ownership model without
advanced features like borrowing, generics, or closures.

See [docs/BOOTSTRAP.md](docs/BOOTSTRAP.md) for the complete bootstrap language specification.

## Project Status

Currently implemented:
- Lexer and parser for basic syntax
- Type checker with function signatures and type inference
- LLVM IR code generation
- Comments: line (`//`) and nested block (`/* */`)
- Control flow: `if`/`else`/`elseif`, `while` loops
- Let bindings (immutable) and var bindings (mutable)
- Assignment statements
- Block expressions
- Expression statements (expressions as statements for side effects)
- Integer types: `I8`, `I16`, `I32`, `I64`, `U8`, `U16`, `U32`, `U64`
- Contextual integer typing (literals infer type from context)
- Integer literal range validation
- Implicit integer widening conversions
- Boolean type: `Bool`
- Binary and unary operators
- Function calls with parameters
- Tuples: literals, indexing, destructuring
- Structs: type definitions, literals, field access
- Sum types (ADTs): type definitions with variants, pattern matching with `match`
- Type classes: class definitions, instance declarations, method calls
- Linear types: ownership tracking with usage annotations (`@*`, `@?`, `@2`, etc.)
- Intrinsics: compiler built-in functions for low-level operations
  - Pointer operations: `@ptr_of`, `@ptr_read`, `@ptr_write`, `@ptr_offset`
- Standard library system
  - `stdlib/prelude.or` - automatically imported by compiler
  - Pointer type class with methods for `ptr` type: `read()`, `write()`, `offset()`

## Building

Requires Zig 0.15.1 or later.

```bash
# Build the compiler
zig build

# Run tests
zig build test

# Build and run
zig build run
```

## Usage

```bash
# Compile an Orion source file to executable
./zig-out/bin/orion source.or
./source

# Stop after generating LLVM IR
./zig-out/bin/orion source.or -S

# Stop after generating object file
./zig-out/bin/orion source.or -c

# Cross-compile for different target (defaults to host)
./zig-out/bin/orion source.or --target x86_64-pc-linux-gnu
./zig-out/bin/orion source.or --target aarch64-apple-darwin
./zig-out/bin/orion source.or --target x86_64-pc-windows-msvc
```

## Cross-Compilation

The compiler supports cross-compilation to multiple platforms and architectures. Target triples follow LLVM's format: `<arch>-<vendor>-<os>`.

Supported targets:
- **Linux**: `x86_64-pc-linux-gnu`, `i686-pc-linux-gnu`, `aarch64-unknown-linux-gnu`, `arm-unknown-linux-gnu`
- **macOS**: `x86_64-apple-darwin`, `aarch64-apple-darwin`
- **Windows**: `x86_64-pc-windows-msvc`, `i686-pc-windows-msvc`

The compiler automatically handles:
- Pointer size differences (32-bit vs 64-bit)
- Platform-specific ABIs and calling conventions
- System C library linking (glibc on Linux, libSystem on macOS, UCRT/msvcrt on Windows)

Requirements:
- **Linux targets**: `gcc` (system linker driver)
- **macOS targets**: `clang` (system linker driver)
- **Windows targets**: `clang-cl` (MSVC toolchain) or `x86_64-w64-mingw32-gcc` (MinGW-w64)

## Examples

### Factorial

```orion
fn factorial(n: I32) I32 {
    var result: I32 = 1
    var counter: I32 = n
    while counter > 1 {
        result = result * counter
        counter = counter - 1
    }
    return result
}

fn main() I32 {
    return factorial(5)
}
```

### Sum Types (ADTs)

```orion
type Option = | None | Some(I32)

fn divide(a: I32, b: I32) Option {
    if b == 0 {
        return None
    } else {
        return Some(a / b)
    }
}

fn main() I32 {
    let result = divide(10, 2)
    return match result {
        None => 0,
        Some(x) => x,
    }
}
```

### Type Classes and Standard Library

```orion
fn main() I32 {
    let x: I32@* = 100
    let p: ptr@* = @ptr_of(x)
    p.write(42)
    return p.read()
}
```

The `Pointer` type class is defined in the standard library prelude and provides
methods for working with the primitive `ptr` type: `read()`, `write()`, `offset()`.

See [examples/](examples/) for more examples.

## Development

```bash
# Run specific test
zig test src/parser.zig
zig test src/codegen.zig

# Build with debug info
zig build -Doptimize=Debug
```

## Architecture

- `src/lexer.zig` - Tokenizes source code
- `src/parser.zig` - Builds AST from tokens
- `src/typechecker.zig` - Type checking and inference
- `src/codegen.zig` - LLVM IR generation
- `src/main.zig` - Compiler entry point
- `stdlib/prelude.or` - Standard library prelude (automatically imported)

## License

Copyright 2025 Jeremy Tregunna

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[https://www.apache.org/licenses/LICENSE-2.0](https://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
