# Frequently Asked Questions

## General

### What is UAST-Grep?

UAST-Grep is a high-performance code search tool that works on Abstract Syntax Trees (ASTs) rather than text. It supports 71 programming languages and provides cross-language search patterns.

### How is it different from grep/ripgrep?

Traditional grep tools search text patterns with regex. UAST-Grep understands code structure:

| Feature | grep/ripgrep | UAST-Grep |
|---------|--------------|-----------|
| Search type | Text/regex | AST structure |
| Understands code | No | Yes |
| Cross-language | No | Yes |
| Find functions | Regex hacks | `FunctionDeclaration` |
| Find in comments only | Difficult | Easy with node types |
| Semantic search | No | Yes |

### What languages are supported?

71 languages total:
- **37 built-in** - Compiled into binary, zero load time
- **34 WASM** - Downloaded on first use, cached locally

See [Language Support](languages/overview.md) for the complete list.

### Is it free?

Yes, UAST-Grep is MIT licensed and completely free for any use.

## Installation

### Which download should I choose?

| Platform | File |
|----------|------|
| Windows x64 | `uast-grep-windows-x64.zip` |
| Linux x64 | `uast-grep-linux-x64.tar.gz` |
| Linux ARM64 | `uast-grep-linux-arm64.tar.gz` |
| macOS Intel | `uast-grep-macos-x64.tar.gz` |
| macOS Apple Silicon | `uast-grep-macos-arm64.tar.gz` |

### Does it require any dependencies?

No. The binary is self-contained with all 37 built-in grammars. For WASM languages, an internet connection is needed on first use (then cached).

### Can I use it offline?

Yes! Pre-download WASM grammars:
```bash
curl -LO https://github.com/MarkusMcNugen/UAST-Grep/releases/latest/download/grammars-wasm.zip
unzip grammars-wasm.zip -d ~/.uast/grammars/
export UAST_OFFLINE=1
```

## Patterns

### What's the difference between UAST and native patterns?

- **UAST patterns** (PascalCase) work across languages: `FunctionDeclaration`
- **Native patterns** (snake_case) are language-specific: `function_definition`

UAST is simpler and portable. Native is precise and complete.

### Why use § instead of $ for metavariables?

Three options are supported:

| Prefix | Notes |
|--------|-------|
| `§` | **Recommended** - No shell escaping needed |
| `∀` | Mathematical meaning, no escaping |
| `$` | Traditional, but needs single quotes in PowerShell |

### How do I find what patterns to use?

```bash
# Parse a file to see node types
uast-grep parse example.py -l python

# Use verbose mode to see mappings
uast-grep run -p FunctionDeclaration -l python ./src --verbose
```

### Can I match multi-line code?

Yes, use YAML rules with multi-line patterns:
```yaml
rule:
  pattern: |
    if §COND:
        pass
```

## Performance

### How fast is it?

| Operation | Time |
|-----------|------|
| Startup | 6ms |
| Parse file | 16ms |
| Pattern search | 34ms |

These are 30-90x faster than traditional tools.

### Why is the first run slow for some languages?

WASM languages are downloaded on first use (1-5 seconds). After that, they're cached and load instantly.

### How do I speed up large scans?

```bash
# Use more threads
uast-grep run -p FunctionDeclaration -l python ./src -j 8

# Limit depth
uast-grep run -p FunctionDeclaration -l python ./src --max-depth 3

# Exclude directories (respects .gitignore by default)
```

## Rules

### How do I write custom rules?

Create a YAML file:
```yaml
id: my-rule
language: python
severity: warning
message: "Description"
rule:
  pattern: "code_pattern"
```

Run with:
```bash
uast-grep scan -r my-rule.yaml ./src
```

### Can rules have fixes?

Yes:
```yaml
rule:
  pattern: "eval(§§§ARGS)"
fix: "ast.literal_eval(§§§ARGS)"
```

### How do I test rules before deploying?

```bash
# Test against sample code
echo "eval(user_input)" | uast-grep scan -r rule.yaml -l python --stdin

# Use examples in the rule
examples:
  match:
    - "eval(user_input)"
  no-match:
    - "literal_eval(data)"
```

## Integration

### Does it work with CI/CD?

Yes! Use SARIF output:
```bash
uast-grep scan -r rules/ -f sarif ./src > results.sarif
```

Upload to GitHub Code Scanning, GitLab, Azure DevOps, etc.

### Can I use it from Python/.NET?

Yes:

**Python:**
```python
from uast_grep import PatternMatcher
matcher = PatternMatcher("FunctionDeclaration")
matches = matcher.matches(source, "python")
```

**.NET:**
```csharp
UastNative.Init();
var parser = UastNative.ParserNew("python");
var tree = UastNative.Parse(parser, sourcePtr, len);
```

### Does it support pre-commit hooks?

Yes:
```bash
#!/bin/bash
# .git/hooks/pre-commit
if ! uast-grep scan -r rules/security/ ./src --quiet; then
    echo "Security issues found!"
    exit 1
fi
```

## Troubleshooting

### "Unknown language" error

Check the language name:
```bash
uast-grep languages | grep -i kotlin
```

For WASM languages, ensure network access or pre-download grammars.

### Pattern doesn't match

1. Check the AST structure:
   ```bash
   uast-grep parse file.py -l python
   ```

2. Use verbose mode:
   ```bash
   uast-grep run -p Pattern -l python --verbose ./src
   ```

3. Try native pattern instead of UAST

### WASM grammar download fails

```bash
# Check network
curl -I https://github.com

# Use offline mode with pre-downloaded grammars
export UAST_OFFLINE=1

# Or specify custom URL
export UAST_WASM_URL=https://internal-mirror/grammars/
```

### "Parse error" on valid code

Some edge cases may not be handled by tree-sitter grammars. Try:
1. Update to latest UAST-Grep version
2. Report issue with minimal reproduction
3. Use a different pattern approach

## Contributing

### How do I add a new language?

See [Adding Languages](contributing/adding-languages.md).

### How do I report bugs?

Open an issue at [github.com/MarkusMcNugen/UAST-Grep/issues](https://github.com/MarkusMcNugen/UAST-Grep/issues) with:
- UAST-Grep version
- Command that failed
- Expected vs actual behavior
- Minimal code sample

### How do I contribute code?

1. Fork the repository
2. Create a feature branch
3. Make changes with tests
4. Submit a pull request

See [Building from Source](contributing/building.md) for development setup.
