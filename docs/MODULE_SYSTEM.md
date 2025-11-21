# Module System Design

## Overview

The Orion bootstrap compiler uses a module system inspired by Rust but simplified for bootstrap purposes. Each file is a module, and the compiler automatically discovers and compiles dependencies in optimal parallel order.

## Goals

- **Ergonomic**: File = module, clear visibility, automatic discovery
- **Simple**: No re-exports, globs, or multiple visibility levels
- **Fast**: Parallel compilation where dependencies allow
- **Libraries**: Support both binary and library compilation

---

## File Structure

### Project Layout

```
myproject/
  orion.toml              # Project manifest
  src/
    main.or              # Binary entry point (has main())
    lexer.or             # Module: lexer
    parser.or            # Module: parser
    compiler/
      codegen.or         # Module: compiler.codegen
      types.or           # Module: compiler.types
```

### Module Naming

- Each `.or` file is a module
- Module name derived from file path relative to `src/`
- Directory separator `.` in import paths

**Examples:**
```
src/main.or           → module: main
src/lexer.or          → module: lexer
src/compiler/codegen.or → module: compiler.codegen
```

---

## Project Manifest (orion.toml)

### Binary Project

```toml
[package]
name = "mycompiler"
version = "0.1.0"

[bin]
entrypoint = "src/main.or"
```

### Library Project

```toml
[package]
name = "mylib"
version = "0.1.0"

[lib]
entrypoint = "src/lib.or"
```

### Binary + Library Project

```toml
[package]
name = "mycompiler"
version = "0.1.0"

[bin]
entrypoint = "src/main.or"

[lib]
entrypoint = "src/lib.or"
```

### With Dependencies

```toml
[package]
name = "mycompiler"
version = "0.1.0"

[bin]
entrypoint = "src/main.or"

[dependencies]
mylib = { path = "../mylib" }
```

---

## Visibility

### Public vs Private

```orion
// Private by default (module-local)
fn helper() I32 { 42 }

type InternalState = {
    counter: I32,
}

// Public (exported from module)
pub fn tokenize(source: Ptr[U8]) Ptr[Token] {
    helper()  // Can call private functions internally
}

pub type Token = {
    kind: TokenKind,
    value: U64,
}
```

**Rules:**
- Everything is private by default
- Use `pub` to export from module
- No `pub(crate)` or `pub(super)` in bootstrap
- All struct fields are public if struct is `pub`

---

## Import Syntax

### Basic Imports

```orion
// Import entire module (use qualified names)
import lexer
lexer.tokenize(source)

// Import submodule
import compiler.codegen
codegen.generate(ast)

// Import specific items
import lexer.tokenize
tokenize(source)  // Use unqualified

// Import multiple items from one module
import compiler.codegen.{generate, optimize}
generate(ast)
optimize(ast)
```

### Import Resolution

**Rule:** Imports are resolved relative to `src/` directory

```orion
import lexer              → src/lexer.or
import compiler.codegen   → src/compiler/codegen.or
import compiler.types     → src/compiler/types.or
```

**Standard library:**
```orion
import std.mem            → stdlib/std/mem.or (bundled with compiler)
import std.io             → stdlib/std/io.or
```

---

## Compilation Model

### Command Line Usage

```bash
# Build binary (uses orion.toml)
orion build

# Or specify manifest explicitly
orion build --manifest myproject/orion.toml

# Build library
orion build-lib

# Specify target
orion build --target x86_64-pc-linux-gnu
```

### Compilation Phases

**Phase 1: Module Discovery**
1. Read `orion.toml` to find entrypoint
2. Parse entrypoint, extract imports
3. Recursively discover all imported modules
4. Build module dependency graph

**Phase 2: Cycle Detection**
```
if circular dependency detected:
  error: "Circular dependency: main → lexer → parser → lexer"
  exit
```

**Phase 3: Topological Sort**
- Sort modules by dependencies
- Group into "levels" where each level can compile in parallel
- Modules in level N only depend on modules in levels < N

**Phase 4: Parallel Compilation**
```
For each level in topological order:
  Compile all modules in this level in parallel:
    1. Type check (can see exports from already-compiled dependencies)
    2. Generate LLVM IR
    3. Compile to object file (.o)
```

**Phase 5: Linking**
```
If binary:
  Link all .o files into executable

If library:
  Bundle .o files
  Generate .or.lib metadata (exports, types)
```

### Dependency Graph Example

```
Source files:
  main.or: import lexer, parser, codegen
  lexer.or: (no imports)
  parser.or: import lexer
  codegen.or: import types, parser
  types.or: (no imports)

Dependency graph:
    main.or
     ├─> lexer.or
     ├─> parser.or ──> lexer.or
     └─> codegen.or ──> types.or
                   └──> parser.or ──> lexer.or

Topological sort (compilation order):
  Level 0: [lexer.or, types.or]        ← Compile in parallel
  Level 1: [parser.or]                 ← Depends on lexer
  Level 2: [codegen.or]                ← Depends on types, parser
  Level 3: [main.or]                   ← Depends on everything
```

### Parallel Compilation Strategy

- Within each level, spawn thread per module
- Type check needs type info from previous levels only
- LLVM IR generation is independent per module
- Object file generation is independent per module

---

## Library Support

### Compiling a Library

```bash
# Create library
orion build-lib

# Outputs:
#   target/libmylib.a       (static library with all .o files)
#   target/libmylib.or.lib  (Orion metadata: exports, type info)
```

### Library Metadata (.or.lib)

Contains:
- Module names and exports
- Public type definitions
- Function signatures
- Type class instances

**Format (binary serialized AST subset):**
```
Export {
  module: "lexer",
  name: "tokenize",
  signature: "fn(Ptr[U8]) Ptr[Token]",
  mangled_name: "lexer__tokenize",
}

Export {
  module: "lexer",
  name: "Token",
  type_def: struct { kind: TokenKind, value: U64 },
}
```

### Using a Library

**In orion.toml:**
```toml
[dependencies]
mylib = { path = "../mylib" }
```

**In source:**
```orion
import mylib.lexer.tokenize

fn main() I32 {
    let tokens = tokenize(source)
    0
}
```

**Compiler behavior:**
1. Read `mylib.or.lib` for type info and exports
2. Type check against library types
3. Link against `libmylib.a` at link time

---

## Error Messages

### Module Not Found

```
Error: Could not find module 'nonexistent'
  --> src/main.or:1:8
   |
 1 | import nonexistent
   |        ^^^^^^^^^^^ module not found
   |
   = note: searched: src/nonexistent.or
```

### Circular Dependency

```
Error: Circular module dependency detected
  --> src/main.or:1:8
   |
   = note: dependency cycle:
           main → lexer → parser → lexer
```

### Private Item Access

```
Error: Function 'helper' is private
  --> src/main.or:5:10
   |
 5 |     lexer.helper()
   |           ^^^^^^ private function
   |
   = note: defined in module 'lexer' at src/lexer.or:10:1
   = help: add 'pub' to make this function public
```

### Missing Export

```
Error: Module 'lexer' has no export named 'tokenise'
  --> src/main.or:1:14
   |
 1 | import lexer.tokenise
   |              ^^^^^^^^ not found in module 'lexer'
   |
   = note: available exports: tokenize, Token, TokenKind
   = help: did you mean 'tokenize'?
```

---

## Comparison to Rust

| Feature | Rust | Orion Bootstrap |
|---------|------|-----------------|
| File = Module | ✓ | ✓ |
| Automatic discovery | ✓ | ✓ |
| Visibility levels | `pub`, `pub(crate)`, `pub(super)`, etc | `pub` or private only |
| Import syntax | `use` | `import` |
| Qualified imports | `use foo::bar` | `import foo.bar` |
| Multiple imports | `use foo::{a, b}` | `import foo.{a, b}` |
| Glob imports | `use foo::*` | ❌ Not in bootstrap |
| Renaming | `use foo as bar` | ❌ Not in bootstrap |
| Re-exports | `pub use` | ❌ Not in bootstrap |
| Inline modules | `mod foo { }` | ❌ Only file modules |
| Conditional compilation | `#[cfg]` | ❌ Not in bootstrap |
| Macros | `macro_rules!` | ❌ Not in bootstrap |
| Workspaces | ✓ | ❌ Not in bootstrap |
| Parallel compilation | ✓ | ✓ |

---

## Implementation Checklist

### Phase 1: Basic Module Support
- [ ] Parse `orion.toml` manifest
- [ ] Add `import` statement to parser
- [ ] Add `pub` keyword to lexer/parser
- [ ] Implement module resolution (find file from import path)
- [ ] Build module dependency graph
- [ ] Detect circular dependencies

### Phase 2: Compilation Pipeline
- [ ] Topological sort of modules
- [ ] Multi-pass compilation (parse all, then type check all)
- [ ] Track exports per module
- [ ] Enforce visibility (private vs pub)
- [ ] Qualified name resolution (`lexer::tokenize`)

### Phase 3: Parallel Compilation
- [ ] Identify independent modules (levels in topo sort)
- [ ] Spawn threads for parallel compilation
- [ ] Synchronize between levels
- [ ] Collect all .o files for linking

### Phase 4: Library Support
- [ ] `build-lib` command
- [ ] Generate .or.lib metadata file
- [ ] Parse .or.lib when using libraries
- [ ] Link against library .a files
- [ ] Dependency resolution in orion.toml

### Phase 5: Standard Library
- [ ] Bundle stdlib with compiler
- [ ] Implement `std::mem`, `std::io` modules
- [ ] Automatic stdlib linking

---

## Future Enhancements (Post-Bootstrap)

Not needed for bootstrap compiler, but design allows:

1. **Incremental compilation**
   - Track file mtimes
   - Skip recompiling unchanged modules
   - Reuse cached .o files

2. **Better error messages**
   - "Did you mean X?" suggestions
   - Show full import chain on errors

3. **Package registry**
   - Central package repository
   - `orion add <package>`
   - Version resolution

4. **Advanced imports**
   - Glob imports: `import foo.*`
   - Renaming: `import foo as bar`
   - Re-exports: `pub import foo.bar`

5. **Conditional compilation**
   - `#[cfg(target_os = "linux")]`
   - Feature flags

6. **Workspaces**
   - Multi-package projects
   - Shared dependencies
