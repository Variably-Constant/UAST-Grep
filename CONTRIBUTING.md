# Contributing to UAST-Grep

Thank you for your interest in contributing to UAST-Grep! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please be respectful and constructive in all interactions. We welcome contributors of all skill levels.

## Ways to Contribute

### Reporting Bugs

1. Check if the issue already exists in [GitHub Issues](https://github.com/Variably-Constant/UAST-Grep/issues)
2. If not, create a new issue with:
   - Clear title and description
   - Steps to reproduce
   - Expected vs actual behavior
   - UAST-Grep version (`uast-grep --version`)
   - OS and platform

### Suggesting Features

Open an issue with the `enhancement` label describing:
- The problem you're trying to solve
- Your proposed solution
- Alternative approaches considered

### Adding Rules

New security, performance, or quality rules are always welcome!

1. Rules go in the appropriate YAML file:
   - `src/UAST.Core/Rules/BuiltIn/universal-security.yaml`
   - `src/UAST.Core/Rules/BuiltIn/universal-performance.yaml`
   - `src/UAST.Core/Rules/BuiltIn/universal-quality.yaml`

2. Follow the rule format:
   ```yaml
   ---
   id: unique-rule-id
   language: universal  # or specific language
   severity: error|warning|info|hint
   message: "Clear description of the issue"
   tags: [category, cwe-XXX, owasp-aXX]  # as applicable

   rule:
     pattern: "$PATTERN"

   fix: "Suggested fix or replacement pattern"
   url: "https://reference-link.com"
   ```

3. Test your rule locally before submitting

### Adding Language Support

For new tree-sitter grammars:

1. Built-in grammars (compiled into binary):
   - Add grammar source to `native/uast_core/grammars/`
   - Update `build.rs` to compile the grammar
   - Add UAST mappings in `src/uast/mappings/`

2. WASM grammars (downloaded on demand):
   - Build the grammar to WASM
   - Add to the WASM release package
   - Update grammar metadata

### Code Contributions

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Make your changes
4. Run tests (see below)
5. Commit with clear messages
6. Push and open a Pull Request

## Development Setup

### Prerequisites

- **Rust** (stable) - Core library and CLI
- **Python 3.8+** + maturin - Python bindings
- **.NET 8.0 SDK** - .NET bindings and rule engine
- **C compiler** - Grammar compilation

### Building

```bash
# Rust CLI (primary)
cd native/uast_core
cargo build --release

# .NET rule engine
dotnet build src/UAST.Core -c Release

# Python bindings
cd python
pip install maturin
maturin develop --features python
```

### Testing

```bash
# Rust tests
cd native/uast_core
cargo test

# .NET tests
dotnet test tests/UAST.Core.Tests

# Python tests
cd python
pytest
```

### Code Style

- **Rust**: Follow `rustfmt` defaults, run `cargo fmt`
- **C#**: Follow .NET conventions
- **Python**: Follow PEP 8, run `black` and `isort`
- **YAML rules**: Follow existing formatting in rule files

## Pull Request Guidelines

1. **One PR per feature/fix** - Keep changes focused
2. **Update documentation** - If your change affects behavior
3. **Add tests** - For new functionality
4. **Update CHANGELOG.md** - For user-facing changes
5. **Follow commit conventions**:
   - `feat:` - New features
   - `fix:` - Bug fixes
   - `docs:` - Documentation
   - `refactor:` - Code refactoring
   - `test:` - Test additions/changes
   - `chore:` - Maintenance tasks

## Project Structure

```
UAST-Grep/
├── native/uast_core/     # Rust crate (PRIMARY)
│   ├── src/               # CLI + library
│   │   ├── cli/            # CLI commands
│   │   ├── uast/           # UAST schema & mappings
│   │   ├── matching/       # Pattern engine
│   │   ├── rules/          # YAML rules engine
│   │   └── sarif/          # SARIF output
│   └── grammars/          # Built-in grammars
├── src/
│   ├── UAST.Core/         # .NET rule engine
│   │   └── Rules/BuiltIn/  # Embedded YAML rules
│   ├── UAST.Cli/          # .NET CLI alternative
│   └── UAST.Native/       # P/Invoke bindings
├── python/               # Python wrapper
├── rules/                # Sample/custom rules
└── docs/                 # mdBook documentation
```

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

## Questions?

- Open an issue for technical questions
- Check [documentation](https://variably-constant.github.io/UAST-Grep/) for usage questions

Thank you for contributing!
