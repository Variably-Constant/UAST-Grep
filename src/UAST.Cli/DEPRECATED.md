# DEPRECATED: UAST.Cli

This project has been deprecated and replaced by the Rust CLI.

## Replacement

Use the new Rust CLI binary:
- **Rust CLI**: `native/uast_core/target/release/uast-grep` (or `uast-grep.exe` on Windows)

## Command Mapping

| Old Command | New Command |
|-------------|-------------|
| `UAST-Grep.exe run -p <pattern> -l <lang> <paths>` | `uast-grep run -p <pattern> -l <lang> <paths>` |
| `UAST-Grep.exe scan -r <rules> <paths>` | `uast-grep scan -r <rules> <paths>` |
| `UAST-Grep.exe parse <file>` | `uast-grep parse <file>` |
| `UAST-Grep.exe languages` | `uast-grep languages` |
| `UAST-Grep.exe ts-query <query> -l <lang> <paths>` | `uast-grep ts-query <query> -l <lang> <paths>` |

## Why Deprecated?

- **Performance**: The Rust CLI is 90x faster at startup (6ms vs 543ms)
- **No Runtime**: Single binary with no .NET runtime required
- **Smaller**: 29MB standalone vs 35MB + runtime

## Migration

Download the Rust binary from [GitHub Releases](https://github.com/MarkusMcNugen/UAST-Grep/releases) and update your scripts to use `uast-grep` instead of `UAST-Grep.exe`.

See [Migration Guide](../../docs/MIGRATION.md) for details.

## Timeline

- **2026-01-03**: Marked as deprecated
- **Future release**: Will be removed from the solution

---

*This project is kept for reference only. Do not add new functionality here.*
