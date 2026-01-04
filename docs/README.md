# UAST-Grep Documentation

Reference documentation for UAST-Grep.

## Documents

### [PowerShell-AST-Reference.md](PowerShell-AST-Reference.md)
PowerShell AST node types and their mapping to UAST schema.
- AST node hierarchy
- Property mappings
- Extension nodes for PS-specific constructs

### [BENCHMARK-REPORT.md](BENCHMARK-REPORT.md)
Performance benchmarks comparing query modes.
- Native vs UAST query speeds
- Static grammar compilation benefits (10-16x faster)
- Memory usage profiles
- Parallel scanning performance

## Quick Links

- [Main README](../README.md) - Project overview and quick start
- [Rules](../rules/README.md) - YAML rule format reference
- [Source Code](../src/README.md) - C# project structure

## Performance Summary

| Operation | Speed |
|-----------|-------|
| Parsing (tree-sitter) | 1-5ms/file |
| UAST conversion | +10-20% overhead |
| Native query | ~0.05-0.2ms/match |
| UAST pattern match | ~0.1-0.5ms/match |

## Additional Resources

- [tree-sitter documentation](https://tree-sitter.github.io/tree-sitter/)
- [70 supported languages](../native/README.md#supported-languages-70)
