# DEPRECATED: UAST.Parsers

This project has been deprecated and replaced by the Rust implementation.

## Replacement

The functionality of UAST.Parsers has been moved to:
- **Rust**: `native/uast_core/src/uast/mapper.rs` - Tree-to-UAST conversion
- **Rust**: `native/uast_core/src/uast/mappings.rs` - Native-to-UAST type mappings
- **Rust**: `native/uast_core/grammars/` - 37 built-in grammars
- **Rust**: `native/uast_core/src/wasm_loader.rs` - 34 WASM on-demand grammars

## Key Changes

### Before (C#)
- C# and PowerShell used native parsers (Roslyn and PowerShell AST)
- 70+ tree-sitter grammars via FFI

### After (Rust)
- **All** languages use tree-sitter (including C# and PowerShell)
- Faster and more consistent parsing
- Syntax-only analysis (no semantic analysis)

## Migration

For .NET developers, use the new **UAST.Net** wrapper library:

```csharp
using UAST.Net;

var parser = new UastParser("powershell");
using var tree = parser.Parse(sourceCode);
```

See [Migration Guide](../../docs/MIGRATION.md) for details.

## Why Deprecated?

- **Performance**: 61x faster parsing (16ms vs 978ms)
- **Consistency**: Same parsing approach for all 71 languages
- **Simplicity**: No separate mappers per language

## Timeline

- **2026-01-03**: Marked as deprecated
- **Future release**: Will be removed from the solution

---

*This project is kept for reference only. Do not add new functionality here.*
