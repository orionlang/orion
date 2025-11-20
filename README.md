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
- Control flow: `if`/`else`/`elseif`, `while` loops
- Let bindings and mutable assignment
- Block expressions
- Basic types: `I32`, `Bool`
- Binary and unary operators
- Function calls with parameters

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
```

## Example

```orion
fn factorial(n: I32) I32 {
    var result: I32 = 1;
    var counter: I32 = n;
    while counter > 1 {
        result = result * counter;
        counter = counter - 1;
    };
    return result
}

fn main() I32 {
    return factorial(5)
}
```

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
