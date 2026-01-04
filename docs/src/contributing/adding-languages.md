# Adding Languages

This guide covers adding new language support to UAST-Grep.

## Overview

UAST-Grep supports languages through tree-sitter grammars. There are three ways to add a language:

1. **Built-in** - Compile grammar into the binary
2. **WASM** - Provide as downloadable WebAssembly module
3. **Dynamic** - Load grammar at runtime from shared library

## Finding Tree-Sitter Grammars

Most languages have existing tree-sitter grammars:

1. **Official tree-sitter org:** [github.com/tree-sitter](https://github.com/tree-sitter)
2. **Community grammars:** Search for `tree-sitter-{language}`
3. **Grammar list:** [tree-sitter.github.io/tree-sitter/#parsers](https://tree-sitter.github.io/tree-sitter/#parsers)

## Adding a Built-in Language

### Step 1: Add Grammar Dependency

Edit `native/uast_core/Cargo.toml`:

```toml
[dependencies]
# Add the tree-sitter grammar crate
tree-sitter-kotlin = { version = "0.3", optional = true }

[features]
# Add to builtin-grammars feature
builtin-grammars = [
    # ... existing grammars ...
    "dep:tree-sitter-kotlin",
]
```

### Step 2: Register the Language

Edit `native/uast_core/src/language_registry.rs`:

```rust
// Add import
#[cfg(feature = "builtin-grammars")]
use tree_sitter_kotlin;

// Add to language registration
pub fn register_builtin_languages() {
    #[cfg(feature = "builtin-grammars")]
    {
        // ... existing registrations ...

        register_language(
            "kotlin",
            &["kt", "kts"],
            tree_sitter_kotlin::language,
        );
    }
}
```

### Step 3: Add UAST Mappings

Edit `native/uast_core/src/uast/mappings.rs`:

```rust
// Add Kotlin mappings
fn kotlin_mappings() -> HashMap<&'static str, Vec<&'static str>> {
    let mut m = HashMap::new();
    m.insert("FunctionDeclaration", vec!["function_declaration"]);
    m.insert("ClassDeclaration", vec!["class_declaration"]);
    m.insert("IfStatement", vec!["if_expression"]);
    m.insert("ForStatement", vec!["for_statement"]);
    m.insert("WhileStatement", vec!["while_statement"]);
    m.insert("CallExpression", vec!["call_expression"]);
    m.insert("VariableDeclaration", vec!["property_declaration"]);
    m.insert("ImportDeclaration", vec!["import_directive"]);
    m
}

// Register in get_mappings_for_language()
pub fn get_mappings_for_language(lang: &str) -> HashMap<&'static str, Vec<&'static str>> {
    match lang {
        // ... existing languages ...
        "kotlin" | "kt" => kotlin_mappings(),
        _ => HashMap::new(),
    }
}
```

### Step 4: Test

```bash
# Build with new grammar
cargo build --release --features builtin-grammars

# Test parsing
echo 'fun hello() { println("Hello") }' | ./target/release/uast-grep parse -l kotlin --stdin

# Test pattern matching
./target/release/uast-grep run -p FunctionDeclaration -l kotlin ./test-files/
```

## Adding a WASM Language

For languages too large for built-in or less commonly used:

### Step 1: Build WASM Module

```bash
# Clone the grammar repository
git clone https://github.com/example/tree-sitter-kotlin
cd tree-sitter-kotlin

# Install emscripten
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk && ./emsdk install latest && ./emsdk activate latest
source ./emsdk_env.sh
cd ..

# Build WASM
tree-sitter build-wasm

# Output: tree-sitter-kotlin.wasm
```

### Step 2: Add to WASM Loader

Edit `native/uast_core/src/wasm_loader.rs`:

```rust
const WASM_LANGUAGES: &[(&str, &[&str])] = &[
    // ... existing languages ...
    ("kotlin", &["kt", "kts"]),
];
```

### Step 3: Upload WASM to Releases

Add the `.wasm` file to the `grammars-wasm.zip` release artifact.

### Step 4: Add UAST Mappings

Same as built-in (Step 3 above).

## Adding Dynamic Language Support

For user-provided grammars at runtime:

### Step 1: Build Grammar as Shared Library

```bash
cd tree-sitter-kotlin

# Build as dynamic library
cargo build --release

# Output:
# - Windows: target/release/tree_sitter_kotlin.dll
# - Linux: target/release/libtree_sitter_kotlin.so
# - macOS: target/release/libtree_sitter_kotlin.dylib
```

### Step 2: Load at Runtime

```rust
use crate::dynamic_loader::load_grammar;

// Load from file
let grammar = load_grammar("/path/to/tree_sitter_kotlin.dll", "kotlin")?;
register_language("kotlin", &["kt", "kts"], grammar);
```

Or from .NET:

```csharp
// Load grammar DLL
using var library = NativeLibrary.Load("tree_sitter_kotlin.dll");
var languageFunc = NativeLibrary.GetExport(library, "tree_sitter_kotlin");

// Get language pointer
var languagePtr = ((delegate* unmanaged[Cdecl]<IntPtr>)languageFunc)();

// Register with UAST
UastNative.RegisterLanguage("kotlin", languagePtr);
```

## Grammar Quality Checklist

Before adding a grammar, verify:

- [ ] Grammar compiles without errors
- [ ] Basic syntax is parsed correctly
- [ ] Named nodes match expected patterns
- [ ] Error recovery works reasonably
- [ ] Performance is acceptable
- [ ] License is compatible (MIT/Apache/BSD)

### Testing Grammar Quality

```bash
# Parse test files
uast-grep parse test.kt -l kotlin

# Check for parse errors
uast-grep parse test.kt -l kotlin 2>&1 | grep -i error

# Verify node types
uast-grep parse test.kt -l kotlin | grep function
```

## Creating UAST Mappings

### Step 1: Explore Node Types

```bash
# Parse sample code and examine nodes
echo 'fun hello() {}' | uast-grep parse -l kotlin --stdin
```

Output:
```
source_file [0:0-0:15]
  function_declaration [0:0-0:15]
    simple_identifier [0:4-0:9] "hello"
    function_value_parameters [0:9-0:11]
    function_body [0:12-0:15]
```

### Step 2: Map to UAST Types

| UAST Type | Kotlin Native |
|-----------|---------------|
| `FunctionDeclaration` | `function_declaration` |
| `ClassDeclaration` | `class_declaration` |
| `IfStatement` | `if_expression` |
| `Identifier` | `simple_identifier` |

### Step 3: Handle Variations

Some languages have multiple node types for one concept:

```rust
// Kotlin has several function types
m.insert("FunctionDeclaration", vec![
    "function_declaration",
    "anonymous_function",
    "lambda_literal",
]);
```

## Submitting Your Addition

1. **Fork** the repository
2. **Create a branch** for your language
3. **Add the grammar** following steps above
4. **Add tests** in `tests/languages/{lang}/`
5. **Update documentation**
6. **Submit a pull request**

### Pull Request Template

```markdown
## Language Addition: Kotlin

### Changes
- Added tree-sitter-kotlin dependency
- Registered kotlin language with extensions .kt, .kts
- Added UAST mappings for 15 node types
- Added test files in tests/languages/kotlin/

### Testing
- [ ] Cargo test passes
- [ ] Parse command works
- [ ] Pattern matching works
- [ ] UAST patterns map correctly

### Documentation
- [ ] Updated languages/overview.md
- [ ] Added languages/kotlin.md (if warranted)
```

## Updating Existing Grammars

When a tree-sitter grammar is updated:

```bash
# Update Cargo.toml version
tree-sitter-kotlin = { version = "0.4", optional = true }

# Rebuild
cargo build --release

# Test for regressions
cargo test

# Verify parsing still works
uast-grep parse tests/languages/kotlin/sample.kt -l kotlin
```
