# Changelog

All notable changes to UAST-Grep will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial documentation site with mdBook
- Comprehensive pattern cookbook
- Security rules collection

### Changed
- Improved error messages for unknown languages

### Fixed
- WASM grammar caching on Windows

## [0.1.0] - 2024-01-01

### Added
- **Core Features**
  - Cross-language AST search with UAST patterns
  - Native tree-sitter pattern support
  - YAML rule engine with complex matching
  - SARIF 2.1.0 output for CI/CD integration

- **Language Support**
  - 37 built-in languages (compiled into binary)
  - 34 WASM languages (downloaded on demand)
  - Total: 71 programming languages

- **CLI Commands**
  - `run` - Pattern search across files
  - `scan` - YAML rule scanning
  - `parse` - AST exploration
  - `ts-query` - Native tree-sitter queries
  - `languages` - List supported languages

- **Pattern Syntax**
  - Three metavariable prefixes: `§`, `∀`, `$`
  - Quantifiers: single, one-or-more, zero-or-more
  - Anonymous captures with `§_`
  - Backreferences for duplicate detection

- **Bindings**
  - Python API via PyO3
  - .NET API via P/Invoke

- **Output Formats**
  - Text (grep-like)
  - JSON
  - SARIF 2.1.0

### Performance
- 6ms cold start
- 16ms file parse
- 34ms pattern search
- 29MB binary size (all grammars included)

---

## Version History

| Version | Date | Highlights |
|---------|------|------------|
| 0.1.0 | 2024-01-01 | Initial release |

---

## Upgrading

### From Pre-release to 0.1.0

No breaking changes from pre-release versions.

---

## Future Plans

### 0.2.0 (Planned)
- [ ] Incremental parsing for large files
- [ ] Watch mode for continuous scanning
- [ ] Rule inheritance and composition
- [ ] Performance improvements for WASM loading

### 0.3.0 (Planned)
- [ ] Language Server Protocol (LSP) support
- [ ] IDE extensions (VS Code, JetBrains)
- [ ] Rule auto-fix application
- [ ] Semantic diffing

---

[Unreleased]: https://github.com/Variably-Constant/UAST-Grep/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/Variably-Constant/UAST-Grep/releases/tag/v0.1.0
