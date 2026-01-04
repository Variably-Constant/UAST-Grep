# DEPRECATED: UAST.Core

This project has been deprecated and replaced by the Rust implementation.

## Replacement

The functionality of UAST.Core has been moved to:
- **Rust**: `native/uast_core/src/uast/` - UAST schema and mappings
- **Rust**: `native/uast_core/src/matching/` - Pattern matching engine
- **Rust**: `native/uast_core/src/rules/` - YAML rules engine

## Migration

For .NET developers, use the new **UAST.Net** wrapper library:

```csharp
using UAST.Net;

var parser = new UastParser("rust");
using var tree = parser.Parse(sourceCode);
```

See [Migration Guide](../../docs/MIGRATION.md) for details.

## Why Deprecated?

- **Performance**: The Rust CLI is 90x faster at startup
- **Simplicity**: Single binary with no runtime dependencies
- **Consistency**: All languages use the same tree-sitter parsing

## Timeline

- **2026-01-03**: Marked as deprecated
- **Future release**: Will be removed from the solution

---

*This project is kept for reference only. Do not add new functionality here.*
