# UAST-Grep Performance Benchmarks

## Overview

This document provides performance benchmarks for UAST-Grep, a high-performance cross-language AST search tool. With the migration to a pure Rust CLI, performance has improved dramatically:

- **90x faster startup**: 6ms vs 543ms
- **61x faster parsing**: 16ms vs 978ms
- **31x faster pattern matching**: 34ms vs 1,056ms

These benchmarks measure:

- **Startup time**: Time to load and execute minimal commands
- **Parsing performance**: Time to parse source files into AST structures
- **Pattern matching performance**: Time to locate matches using UAST and native patterns
- **Rule scanning performance**: Time to evaluate YAML rules against source files
- **Two-tier grammar loading**: Built-in (static) vs WASM (on-demand) grammar performance

---

## Architecture Overview

UAST-Grep now uses a **pure Rust architecture**:

| Component | Technology | Startup Cost | Notes |
|-----------|------------|--------------|-------|
| **CLI** | Rust (uast-grep binary) | ~5 ms | Native code, instant startup |
| **Parsing Engine** | tree-sitter 0.26 | 0 ms | Statically linked |
| **Built-in Grammars** | 37 languages | 0 ms | Compiled into binary |
| **WASM Grammars** | 34 languages | 50-200 ms | Downloaded on first use, cached |
| **.NET Wrapper** | UAST.Net (optional) | ~350 ms | For .NET integration only |

The pure Rust CLI eliminates all JIT compilation overhead while maintaining the same feature set.

---

## Test Environment

| Component | Specification |
|-----------|---------------|
| **Operating System** | Windows 11 |
| **CPU** | 13th Gen Intel Core i7-13800H (14 cores, 20 threads) |
| **RAM** | 21 GB DDR5 |
| **Storage** | NVMe SSD |
| **UAST-Grep Version** | 0.1.0 (Rust CLI) |

*Benchmarks run on 2026-01-03 using hyperfine 1.20.0 with 3 warmup runs and 10 measured runs.*

---

## Rust CLI Benchmark Results

The following benchmarks were measured using `hyperfine --warmup 3 --runs 10 -N` on the test environment above.

### Rust CLI Performance

| Command | Mean Time | Std Dev | Min | Max |
|---------|-----------|---------|-----|-----|
| `uast-grep languages` | 6.0 ms | 0.2 ms | 5.8 ms | 6.4 ms |
| `uast-grep parse lib.rs` (1000 lines) | 16.3 ms | 0.5 ms | 15.8 ms | 17.3 ms |
| `uast-grep run -p FunctionDeclaration -l rust ./src/` | 34.1 ms | 1.0 ms | 32.1 ms | 35.6 ms |
| `uast-grep scan -r rules.yaml ./src/` | 462.5 ms | 33.4 ms | 421.6 ms | 518.8 ms |

### Time Breakdown

| Phase | Time | Notes |
|-------|------|-------|
| Binary load + init | ~5 ms | One-time per invocation |
| Grammar lookup | < 1 ms | O(1) hash lookup |
| tree-sitter parsing | 5-15 ms | Depends on file size |
| UAST conversion | 1-5 ms | Node mapping |
| Pattern matching | 1-10 ms | Per file |
| Output formatting | 1-5 ms | Console or JSON |

**Key insight**: The Rust CLI is bottlenecked only by I/O and actual parsing work - no runtime overhead.

---

## Performance Comparison: Old (.NET) vs New (Rust)

### Migration Impact

| Operation | Old (.NET CLI) | New (Rust CLI) | Improvement |
|-----------|----------------|----------------|-------------|
| Startup (languages) | 543 ms | 6 ms | **90x faster** |
| Parse single file | 978 ms | 16 ms | **61x faster** |
| Pattern search | 1,056 ms | 34 ms | **31x faster** |
| Binary size | 35 MB + runtime | 29 MB | **No dependencies** |

### Old .NET CLI Results (for reference)

These were the benchmarks for the previous C#/.NET hybrid architecture:

| Command | Language | Mean Time | Notes |
|---------|----------|-----------|-------|
| `parse` | Rust (236 lines) | 978 ms | Includes ~400ms JIT overhead |
| `parse` | C (305 lines) | 1,178 ms | |
| `parse` | PowerShell (272 lines) | 1,420 ms | |
| `parse` | C# (265 lines) | 2,348 ms | Roslyn overhead |
| `run -p FunctionDeclaration` | Rust | 1,056 ms | |
| `languages` | N/A | 543 ms | |

### Why the Rust CLI is Faster

1. **No JIT compilation**: Native code executes immediately
2. **No runtime loading**: No .NET runtime initialization
3. **Unified binary**: No P/Invoke FFI overhead between C# and Rust
4. **Optimized release build**: LTO and single codegen unit
5. **Direct tree-sitter access**: No marshalling between languages

---

## Two-Tier Grammar System

UAST-Grep uses a two-tier grammar system to balance binary size with language coverage:

### Tier 1: Built-in Languages (37)

Grammars compiled directly into `uast_core.dll` at build time.

| Metric | Value |
|--------|-------|
| **Languages** | 37 |
| **Binary size** | ~27 MB |
| **Load time** | 0 ms (instant) |
| **Availability** | Always available offline |

**Categories**: PowerShell, C#, Python, JavaScript, TypeScript, Rust, Go, Java, C, C++, Ruby, PHP, JSON, YAML, XML, HTML, CSS, Bash, Dockerfile, HCL/Terraform, and more.

### Tier 2: WASM Languages (34)

Grammars downloaded on-demand from GitHub Releases as WebAssembly modules.

| Metric | Value |
|--------|-------|
| **Languages** | 34 |
| **Per-grammar size** | 100 KB - 2 MB |
| **First-use load time** | 50-200 ms (download) + 20-50 ms (WASM compile) |
| **Subsequent load time** | 20-50 ms (from cache) |
| **Cache location** | `~/.uast-grep/grammars/` |

**Categories**: SQL, Kotlin, Swift, Scala, Haskell, F#, LaTeX, Verilog, COBOL, Fortran, and more.

### Performance Comparison

| Grammar Type | First Parse | Subsequent Parses | Offline Support |
|--------------|-------------|-------------------|-----------------|
| **Built-in** | Instant (0 ms overhead) | Instant | ✓ Yes |
| **WASM** | 50-250 ms (download + compile) | 20-50 ms (compile only) | After first download |

### When to Use Each Tier

| Scenario | Recommendation |
|----------|----------------|
| CI/CD pipelines | Built-in languages (predictable, no network) |
| Air-gapped environments | Built-in only, or pre-download WASM |
| Development workstations | Either (WASM auto-downloads) |
| Scanning exotic languages | WASM (SQL, Kotlin, Swift, etc.) |

---

## Pattern Matching Performance

Once the AST is parsed, pattern matching is fast:

| Query Type | Time per Match | Description |
|------------|----------------|-------------|
| **Native tree-sitter queries** | ~0.05-0.2 ms | Direct S-expression queries |
| **UAST pattern matching** | ~0.1-0.5 ms | Cross-language unified patterns |

### UAST vs Native Queries

| Use Case | Recommended | Why |
|----------|-------------|-----|
| Maximum performance | Native queries | No UAST mapping overhead |
| Cross-language rules | UAST patterns | Write once, run anywhere |
| Security scanning | UAST patterns | Portable rule libraries |
| Language-specific edge cases | Native queries | Full tree-sitter access |

---

## Memory Usage

| Component | Memory Usage |
|-----------|--------------|
| **Rust core (uast_core.dll)** | ~20-30 MB (shared) |
| **Per-file AST (small)** | 500 KB - 1 MB |
| **Per-file AST (medium)** | 1 - 3 MB |
| **Per-file AST (large)** | 3 - 5 MB |
| **WASM grammar (loaded)** | 1 - 5 MB each |
| **Pattern matcher** | < 1 MB |

---

## Parallel Scanning Performance

| CPU Cores | Estimated Speedup | Notes |
|-----------|-------------------|-------|
| 1 | 1x (baseline) | Sequential |
| 4 | ~3.7x | Near-linear |
| 8 | ~7.2x | Near-linear |
| 16+ | ~12-14x | I/O bound |

For a 1,000-file codebase (~100K lines), expect **2-3 seconds** on 8 cores with SSD storage.

---

## Performance Tips

1. **Use built-in languages**: 37 languages with zero grammar loading overhead
2. **Pre-download WASM**: For CI/CD, download grammars ahead of time with `UAST_OFFLINE=0`
3. **Use native queries**: For single-language, performance-critical scans
4. **SSD storage**: I/O is often the bottleneck for large codebases
5. **File filters**: Use `--include "*.rs"` to skip irrelevant files
6. **Parallel scanning**: The Rust CLI uses rayon for parallel file processing

---

## Running Your Own Benchmarks

```bash
# Install hyperfine
winget install sharkdp.hyperfine

# Benchmark startup time
hyperfine --warmup 3 -N './uast-grep languages'

# Benchmark parse command
hyperfine --warmup 3 -N './uast-grep parse ./src/file.rs'

# Benchmark pattern matching
hyperfine --warmup 3 -N './uast-grep run -p FunctionDeclaration -l rust ./src'

# Benchmark rule scanning
hyperfine --warmup 3 -N './uast-grep scan -r rules/security.yaml ./src'

# Export results to markdown
hyperfine --warmup 3 --export-markdown results.md './uast-grep parse ./file.rs'
```

### Caveats

- Windows Defender real-time scanning can add 50-200 ms
- WASM languages have first-use download overhead
- For .NET wrapper (UAST.Net), expect ~350ms additional startup for JIT compilation

---

*Document updated 2026-01-03. Performance validated on Windows 11, Intel i7-13800H, Rust 1.75.*
