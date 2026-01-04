# UAST-Grep Build Scripts

This folder contains PowerShell scripts for building and maintaining tree-sitter grammar files.

## Grammar Architecture

UAST-Grep uses a two-tier grammar system:

| Tier | Count | Storage | Loading |
|------|-------|---------|---------|
| **Built-in** | 37 | Compiled into binary | Instant (static linking) |
| **WASM** | 34 | GitHub Releases | Downloaded on first use, cached locally |

**Why two tiers?**
- Built-in grammars provide instant startup for common languages
- WASM grammars keep the binary size manageable (~30MB vs ~100MB+)
- WASM grammars are auto-downloaded and cached to `~/.uast/grammars/`

## Scripts

### Build-NativeGrammars.ps1

Regenerates tree-sitter grammar C source files from upstream repositories.

**When to run:**
- After upgrading tree-sitter version (ABI compatibility)
- When updating grammar versions for bug fixes
- When adding new language support
- After fresh clone (grammars are checked into git, so usually not needed)

**Prerequisites:**
```powershell
# Required
cargo install tree-sitter-cli   # tree-sitter 0.26.3+
# npm (for grammars with package.json dependencies)
# git
```

**Usage:**
```powershell
# Regenerate ALL 71 grammars (takes 10-20 minutes)
.\Build-NativeGrammars.ps1

# Regenerate specific languages only
.\Build-NativeGrammars.ps1 -Languages python,javascript,rust

# Custom output directory
.\Build-NativeGrammars.ps1 -GrammarsDir "C:\custom\path\grammars"
```

**What it does:**
1. Clones each grammar repository from GitHub (shallow clone)
2. Runs `npm install` if package.json exists
3. Runs `tree-sitter generate` to create parser.c/scanner.c
4. Copies generated files to `native/uast_core/grammars/{lang}/src/`
5. Handles special cases (monorepo grammars, extra headers, C++ scanners)

**After running:**
```bash
cd native/uast_core
cargo build --release   # Recompile Rust binary with updated grammars
```

---

### Build-WasmGrammars.ps1

Compiles Tier 3 (WASM) grammars to WebAssembly for GitHub Releases deployment.

**When to run:**
- Before creating a new GitHub Release
- When updating WASM grammar versions
- For offline distribution packages

**Prerequisites:**
```powershell
# Required
cargo install tree-sitter-cli   # tree-sitter 0.26.3+

# Required for WASM compilation
# Emscripten SDK - https://emscripten.org/docs/getting_started/downloads.html
# After install, run: emsdk activate latest
```

**Usage:**
```powershell
# Compile ALL 34 WASM grammars
.\Build-WasmGrammars.ps1

# Compile specific languages only
.\Build-WasmGrammars.ps1 -Languages sql,kotlin,scala

# Custom output directory
.\Build-WasmGrammars.ps1 -OutputDir "C:\release\wasm-grammars"
```

**What it does:**
1. Reads grammar sources from `native/uast_core/grammars/{lang}/`
2. Runs `tree-sitter build --wasm` for each Tier 3 language
3. Outputs `.wasm` files to `native/uast_core/grammars-wasm/`

**After running:**
1. Upload `.wasm` files to GitHub Release (matching version in `wasm_loader.rs`)
2. Users will auto-download these on first use of each WASM language

---

## Language Tiers

### Built-in Languages (37)
Compiled directly into the Rust binary via `build.rs`:

| Category | Languages |
|----------|-----------|
| Core | powershell, c-sharp |
| Data | json, yaml, xml, toml, csv |
| DevOps | dockerfile, hcl, bicep, nix, bash |
| Web | html, css, javascript, typescript, tsx, vue, angular |
| Docs | markdown |
| Backend | python, go, java, c, cpp, rust |
| Scripting | ruby, php, lua |
| Build | cmake, make, proto, graphql |
| Functional | elixir, erlang, clojure, elm |

### WASM Languages (34)
Loaded on-demand from GitHub Releases:

| Category | Languages |
|----------|-----------|
| Large (>15MB) | verilog, latex, sql, fortran, fsharp, kotlin, cobol, scala, objc, julia, d, crystal, cuda, haskell, swift, perl, arduino, agda, ocaml |
| Niche | apex, dart, groovy, commonlisp, zig, awk, vim, r, bitbake, ada, cairo, dhall, cue, doxygen, comment |

---

## Troubleshooting

### "tree-sitter not found"
```powershell
cargo install tree-sitter-cli
# Ensure ~/.cargo/bin is in PATH
```

### "emcc not found" (WASM only)
```bash
# Download Emscripten SDK
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh   # Or emsdk_env.bat on Windows
```

### Grammar generation fails
- Check if the grammar repo URL is still valid
- Some grammars require specific npm/node versions
- Try running `tree-sitter generate` manually in the cloned repo

### Build fails after grammar update
- Ensure `tree-sitter-cli` version matches the expected ABI (0.26.x)
- Check `native/uast_core/include/tree_sitter/parser.h` exists
- Run `cargo clean` and rebuild

---

## Release Checklist

When preparing a new release:

1. **Update grammar sources** (if needed):
   ```powershell
   .\Build-NativeGrammars.ps1
   ```

2. **Build and test Rust binary**:
   ```bash
   cd native/uast_core
   cargo build --release
   cargo test
   ```

3. **Build WASM grammars**:
   ```powershell
   .\Build-WasmGrammars.ps1
   ```

4. **Update version** in `wasm_loader.rs`:
   ```rust
   const GRAMMAR_VERSION: &str = "v1.1.0";  // Match release tag
   ```

5. **Create GitHub Release**:
   - Tag: `v1.1.0`
   - Attach: `uast-grep.exe`, all `.wasm` files from `grammars-wasm/`

6. **Verify auto-download**:
   ```bash
   # Clear cache and test
   rm -rf ~/.uast/grammars/
   uast-grep parse --language sql test.sql  # Should auto-download
   ```
