# Option B: Per-Module Compilation with Explicit Function Exports
## Revised: Stdlib-as-Library Architecture Foundation

## Problem Statement

When compiling multiple modules into separate object files (.o), the current approach includes all stdlib functions in every user module's compilation. This causes:
1. Duplicate symbol errors at link time (e.g., `str__len` defined in multiple .o files)
2. Inefficient builds: stdlib recompiles with every user module change
3. Instance method duplication: stdlib instance methods appear in every module's IR

The root issue: **stdlib functions shouldn't be embedded in user module compilation at all**.

## Solution Architecture: Two Layers

### Layer 1: Pre-Compiled Stdlib Library (Phase 0)
Build stdlib once as a static archive (.a) or object files, link user modules against it.

**Core Principle:**
- Stdlib compiles independently → stdlib.a or stdlib.o
- User modules only compile their own functions
- Linker combines user code with stdlib
- No function duplication

### Layer 2: Per-Module User Compilation (Phases 1-3)
After stdlib is stable, user modules compile separately against stdlib.

## Implementation Steps

### Phase 0: Stdlib Compilation (Foundation - Do First)

**Goal**: Build stdlib once, cache it, use for all user compilations

**Changes to main.zig:**
1. Add `compileStdlib()` function:
   - Check if stdlib artifact exists and is valid
   - If not, compile all stdlib modules together
   - Output single `stdlib.a` (or stdlib.o files) to cache dir

2. Modify module discovery:
   - Separate stdlib modules from user modules
   - stdlib_modules = [std.string, std.alloc, etc]
   - user_modules = [src/main.or, src/lib.or, etc]

3. Build workflow:
   ```
   if (stdlib artifact missing or invalid) {
       compileStdlib() -> stdlib.a
   }
   for each user_module {
       compile(user_module, link_against=[stdlib.a])
   }
   ```

**Changes to codegen.zig:**
- No per-module filtering needed (Phase 0 compiles stdlib fully)
- Generate all functions in stdlib → single artifact

**Artifacts:**
- `~/.orion/stdlib/[target_triple]/stdlib.a` (cache directory)
- Hash-based validation (rebuild if stdlib source changes)

**Expected Result**: Clean separation - stdlib is a library dependency, not embedded code.

### Phase 1: Per-Module User Compilation (Second)

**Goal**: After stdlib is pre-compiled, compile user modules separately

**Changes to codegen.zig:**
1. Add `module_functions: ?[]const []const u8` field to Codegen struct
   - `null` = compile all functions (backward compatible)
   - List of names = only compile these function names

2. Add `isModuleFunction()` helper method

3. Modify function generation loop:
   - Skip functions not in `module_functions` list
   - Only user-defined functions emit LLVM code

4. Auto-generate extern declarations for external functions:
   - For functions NOT in module_functions, emit extern declarations
   - These declarations allow cross-module function calls

**Changes to main.zig:**
1. When compiling each user module:
   - Create list of function names from current module's AST
   - DO NOT include stdlib in combined_ast for function generation
   - Pass module function names to codegen

2. Link all user .o files + stdlib.a:
   - Each user .o has only its own functions
   - Linker resolves against stdlib.a for stdlib calls
   - No duplicate symbols

**Expected Result**: Each user module compiles to .o with only its functions; linker resolves all symbols.

### Phase 2: Main Compilation Loop Integration (Third)

**Goal**: Wire up two-pass user module compilation with stdlib linking

**Workflow:**
```
Pass 1: Parse all modules
  - Discover stdlib vs user modules
  - Parse all for type checking

Pass 2: Compile with caching
  if (stdlib needs rebuild) {
      compile stdlib modules together -> stdlib.a
  }
  for each user_module {
      type_check(user_module, with stdlib types)
      compile(user_module, module_functions=[current module only])
      generate object file: user_module.o
  }

Link phase:
  - Collect all user .o files
  - Link with stdlib.a
  - Produce final executable
```

**Expected Result**: Clean per-module user compilation, linked against stable stdlib.

### Phase 3: Testing (Final)

**Goal**: Verify end-to-end multi-module compilation with stdlib

**Test Cases:**
1. Single module (src/main.or) - works as before
2. Two user modules (src/main.or + src/lib.or) - generates main.o, lib.o, links with stdlib.a
3. Multi-module with cross-module calls - functions resolved via extern + linker
4. Stdlib caching - second build uses cached stdlib.a
5. Verify no duplicate symbol errors
6. Verify stdlib only compiled once per target

## Detailed Component Breakdown

### Phase 0: Stdlib Architecture

```
Discovery Phase:
  stdlib_paths = [std.string, std.alloc, std.char, ...]
  user_paths = [src/main.or, src/lib.or, ...]

Stdlib Compilation (if needed):
  combined_ast = all stdlib modules (types, classes, instances, functions)
  codegen.module_functions = null (compile everything)
  output: stdlib.ll -> stdlib.o files
  link: stdlib.o files -> stdlib.a (archive)
  cache: ~/.orion/stdlib/[target_triple]/stdlib.a

Cache Validation:
  if (stdlib source hash == cached hash) {
      reuse stdlib.a
  } else {
      recompile stdlib
  }
```

### Phase 1: Codegen Module Filtering

```zig
pub const Codegen = struct {
    // ... existing fields ...
    module_functions: ?[]const []const u8, // Names from current module only

    fn isModuleFunction(self: *Codegen, func_name: []const u8) bool {
        if (self.module_functions == null) return true;
        for (self.module_functions.?) |name| {
            if (std.mem.eql(u8, name, func_name)) return true;
        }
        return false;
    }
};

// In generate():
for (ast.functions.items) |func| {
    if (!self.isModuleFunction(func.name)) continue;
    try self.generateFunction(&func);
}

// Generate extern declarations for external functions
if (self.module_functions != null) {
    for (ast.functions.items) |func| {
        if (self.isModuleFunction(func.name)) continue;
        try self.output.writer(self.allocator).print(
            "declare {s} @{s}(...)\n",
            .{ return_type_str, func.name }
        );
    }
}
```

### Phase 2: Main.zig Changes

```zig
// Separate stdlib from user modules
var stdlib_modules = std.ArrayList([]const u8).empty;
var user_modules = std.ArrayList([]const u8).empty;

for (module_order.items) |module_path| {
    if (isStdlibModule(module_path)) {
        try stdlib_modules.append(allocator, module_path);
    } else {
        try user_modules.append(allocator, module_path);
    }
}

// Compile stdlib (if needed)
const stdlib_archive = try compileStdlib(allocator, stdlib_modules, target_triple);
defer allocator.free(stdlib_archive);

// Compile user modules
for (user_modules.items) |user_module_path| {
    var module_func_names = std.ArrayList([]const u8).empty;
    const mod_ast = module_asts.get(user_module_path).?;

    for (mod_ast.functions.items) |func| {
        try module_func_names.append(allocator, try allocator.dupe(u8, func.name));
    }

    var codegen = Codegen.init(allocator, target_triple);
    codegen.module_functions = module_func_names.items;
    const llvm_ir = try codegen.generate(&user_module_combined_ast);

    try object_files.append(allocator, obj_path);
}

// Link all user .o files with stdlib.a
linkExecutable(object_files, stdlib_archive);
```

## Data Flow Diagram

```
Parse Phase:
  std.string, std.alloc, std.char -> stdlib_modules
  src/main.or, src/lib.or -> user_modules

Stdlib Compilation (Phase 0):
  combined_ast = all stdlib types/classes/functions
  codegen.module_functions = null (all functions)
  Output: stdlib.ll -> opt -> stdlib.a (cached)

User Compilation (Phase 1):
  For src/main.or:
    module_functions = ["main", "helper_func"]
    combined_ast = user types + current module functions
    Output: src/main.ll -> src/main.o

    Generates:
      define i64 @main() { ... }
      define i64 @helper_func() { ... }
      declare i64 @other_function()  // extern for src/lib.or functions

  For src/lib.or:
    module_functions = ["lib_function"]
    combined_ast = user types + current module functions
    Output: src/lib.ll -> src/lib.o

    Generates:
      define i64 @lib_function() { ... }
      declare i64 @main()  // extern for src/main.or functions

Link Phase:
  src/main.o + src/lib.o + stdlib.a -> final executable
  Linker resolves:
    - main.o's calls to lib_function with src/lib.o definition
    - main.o's calls to str__len with stdlib.a definition
    - lib.o's calls to main with src/main.o definition
```

## Key Design Decisions

1. **Stdlib as a Pre-Compiled Artifact**
   - Solution: Build stdlib once, cache it
   - Solves instance method duplication entirely
   - Enables true incremental builds
   - Mirrors real compiler architecture (clang, rustc)

2. **Cache Invalidation**
   - Hash stdlib source files
   - Cache key: target_triple + source_hash
   - Rebuild if source changes or target changes

3. **Per-Module User Compilation**
   - Only user modules compile separately
   - stdlib compiles as one unit (no module fragmentation)
   - Type information still available to all

4. **Type Information for User Modules**
   - Parse stdlib type/class/instance defs
   - Make available to user type checker without including stdlib functions
   - User codegen only outputs user functions

5. **Cross-Module Linking**
   - Extern declarations within user modules
   - stdlib symbols resolved at link time
   - No need for complex dependency tracking

## Success Criteria

### Phase 0: Stdlib Library
- [ ] Identify all stdlib modules (std.*)
- [ ] Create stdlib compilation path (compiles as single unit)
- [ ] Generate stdlib.a artifact
- [ ] Implement cache directory with hash validation
- [ ] Verify stdlib.a contains all stdlib functions

### Phase 1: Per-Module User Code
- [ ] Add module_functions field to Codegen
- [ ] isModuleFunction() filtering works
- [ ] User functions only in their module's .o file
- [ ] Extern declarations auto-generated for external functions

### Phase 2: Integration
- [ ] Separate stdlib from user modules in main.zig
- [ ] User modules compile with module_functions filtering
- [ ] Linker combines user .o + stdlib.a successfully

### Phase 3: Testing
- [ ] Single user module links with stdlib.a
- [ ] Two user modules link with each other + stdlib.a
- [ ] No duplicate symbol errors
- [ ] Cross-module function calls work
- [ ] Stdlib caching prevents recompilation

## Potential Issues & Solutions

**Issue 1: Type Information from Stdlib in User Modules**
- User type checker needs stdlib type definitions
- Solution: Load stdlib AST for types only, don't generate functions

**Issue 2: Instance Methods from Stdlib**
- Instance methods (like str__len) needed for type checking
- Solution: Include in combined_ast for type checking, but stdlib.a contains actual implementation
- User module just needs extern declaration

**Issue 3: Cache Invalidation Across Targets**
- Building for multiple targets (x86_64, aarch64)
- Solution: Cache key includes target_triple, separate stdlib per target

**Issue 4: First-Time Build (No Cached Stdlib)**
- Slow first build (compiling stdlib)
- Solution: This is expected and correct; subsequent builds are fast

## Timeline & Dependencies

1. **Phase 0** (Do First): Stdlib library infrastructure
   - Highest priority: solves duplicate symbol problem at root
   - Enables all user-module features to work cleanly

2. **Phase 1** (Second): Codegen per-module filtering
   - Depends on Phase 0 (stdlib is external)
   - Modular, testable in isolation

3. **Phase 2** (Third): Integration in build loop
   - Depends on Phases 0 & 1

4. **Phase 3** (Final): Testing
   - Depends on Phases 0, 1, 2

## Backward Compatibility

- Single-file projects: Still work (stdlib automatically compiled and cached)
- Existing APIs: No changes to public compilation interface
- Fallback: If stdlib cache invalid, automatically recompiles
- Development: Developers can force stdlib rebuild if needed

