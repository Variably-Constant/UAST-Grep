# Language Support Overview

UAST-Grep supports **71 programming languages** through a three-tier grammar system.

## Three-Tier Architecture

### Built-in Languages (37) - Tier 1+2

Built-in grammars are compiled directly into the binary:
- **Zero loading time** - Ready instantly
- **No network required** - Works offline
- **Small footprint** - All 37 grammars in ~29MB

### WASM Languages (31) - Tier 3

WASM grammars are downloaded on first use:
- **On-demand loading** - Only downloads what you need
- **Cached locally** - Fast subsequent use
- **Sandboxed execution** - Runs in WebAssembly sandbox
- **Larger grammar support** - Some grammars are too large for built-in

### Native Languages (3) - Tier 3b

Some grammars cannot compile to WASM due to C library dependencies:
- **Platform-specific** - Different files for Windows/Linux/macOS
- **Native performance** - Direct execution, no sandbox overhead
- **Auto-download** - Downloaded from GitHub releases when needed
- **Languages:** COBOL, Doxygen, Vim (see [Native Languages](native.md))

## Complete Language List

### Built-in Languages

| Category | Languages |
|----------|-----------|
| **Core** | PowerShell, C# |
| **Web** | HTML, CSS, JavaScript, TypeScript, TSX, Vue, Angular |
| **Backend** | Python, Go, Java, Rust, C, C++ |
| **Data** | JSON, YAML, XML, TOML, CSV |
| **DevOps** | Dockerfile, HCL/Terraform, Bicep, Nix, Bash |
| **Scripting** | Ruby, PHP, Lua |
| **Docs** | Markdown |
| **Build** | CMake, Make, Protocol Buffers, GraphQL |
| **Functional** | Elixir, Erlang, Clojure, Elm |

### WASM Languages

| Category | Languages |
|----------|-----------|
| **Large Grammars** | Verilog, LaTeX, SQL, Fortran, F#, Kotlin, Scala |
| **System** | Objective-C, Julia, D, Crystal, CUDA, Haskell, Swift, Perl |
| **Specialized** | Arduino, Agda, OCaml, Apex, Dart, Groovy, Common Lisp |
| **Modern** | Zig, AWK, R, Bitbake, Ada, Cairo, Dhall, CUE |
| **Meta** | Comment |

### Native Languages (WASM-incompatible)

| Language | Reason | More Info |
|----------|--------|-----------|
| **COBOL** | Uses C99 Variable Length Arrays | [Native Languages](native.md) |
| **Doxygen** | Uses C stdio functions | [Native Languages](native.md) |
| **Vim** | Uses wide character functions | [Native Languages](native.md) |

## Checking Language Support

### List All Languages

```bash
uast-grep languages
```

Output:
```
Built-in Languages (37):
  bash, bicep, c, cmake, cpp, csharp, css, csv, dockerfile,
  elixir, elm, erlang, go, graphql, hcl, html, java,
  javascript, json, lua, make, markdown, nix, php, powershell,
  proto, python, ruby, rust, toml, tsx, typescript, vue,
  xml, yaml

WASM Languages (31):
  ada, agda, apex, arduino, awk, cairo, comment, commonlisp,
  crystal, cue, cuda, d, dart, dhall, fortran, fsharp,
  groovy, haskell, julia, kotlin, latex, objc, ocaml, perl,
  r, scala, sql, swift, verilog, zig

Native Languages (3):
  cobol, doxygen, vim
```

### Check Specific Language

```bash
# Check if language is available
uast-grep languages | grep -i kotlin

# Or use directly - UAST-Grep will tell you if unsupported
uast-grep run -p FunctionDeclaration -l kotlin ./src
```

## Language Detection

UAST-Grep can detect languages from file extensions:

| Extensions | Language |
|------------|----------|
| `.py`, `.pyw`, `.pyi` | Python |
| `.js`, `.mjs`, `.cjs` | JavaScript |
| `.ts` | TypeScript |
| `.tsx` | TSX |
| `.rs` | Rust |
| `.go` | Go |
| `.java` | Java |
| `.cs` | C# |
| `.ps1`, `.psm1`, `.psd1` | PowerShell |
| `.rb`, `.rake` | Ruby |
| `.php` | PHP |
| `.cpp`, `.cc`, `.cxx`, `.hpp` | C++ |
| `.c`, `.h` | C |
| `.swift` | Swift |
| `.kt`, `.kts` | Kotlin |
| `.scala`, `.sc` | Scala |
| `.dart` | Dart |
| `.lua` | Lua |
| `.sh`, `.bash`, `.zsh` | Bash |
| `.sql` | SQL |
| `.html`, `.htm` | HTML |
| `.css` | CSS |
| `.json` | JSON |
| `.yaml`, `.yml` | YAML |
| `.xml` | XML |
| `.toml` | TOML |
| `.md`, `.markdown` | Markdown |
| `Dockerfile` | Dockerfile |
| `Makefile`, `.mk` | Make |
| `.tf`, `.hcl` | HCL/Terraform |
| `.nix` | Nix |
| `.ex`, `.exs` | Elixir |
| `.erl`, `.hrl` | Erlang |
| `.clj`, `.cljs` | Clojure |
| `.elm` | Elm |
| `.vue` | Vue |
| `.graphql`, `.gql` | GraphQL |
| `.proto` | Protocol Buffers |

## WASM Grammar Management

### Cache Location

WASM grammars are cached in:
- **Linux/macOS:** `~/.uast/grammars/`
- **Windows:** `%USERPROFILE%\.uast\grammars\`

### Manual Download

For offline environments:

```bash
# Download all WASM grammars
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/grammars-wasm.zip
unzip grammars-wasm.zip -d ~/.uast/grammars/
```

### Custom Path

```bash
export UAST_WASM_PATH=/custom/path/to/grammars
```

### Disable Auto-Download

```bash
export UAST_OFFLINE=1
```

### Custom Download URL

```bash
export UAST_WASM_URL=https://internal-server/grammars/
```

## Adding Custom Languages

UAST-Grep can load custom tree-sitter grammars:

1. **Compile the grammar** as a dynamic library (`.dll`/`.so`/`.dylib`)
2. **Place in grammars directory** or specify path
3. **Register with UAST-Grep**

See [Adding Languages](../contributing/adding-languages.md) for details.

## Language-Specific Guides

For detailed information about specific languages:
- [PowerShell](powershell.md) - Full PowerShell AST reference
- [C#](csharp.md) - C# patterns and best practices
- [Built-in Languages](built-in.md) - Complete built-in reference
- [WASM Languages](wasm.md) - WASM grammar details
- [Native Languages](native.md) - Native DLL grammars (COBOL, Doxygen, Vim)
