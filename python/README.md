# uast-grep Python Bindings

> High-performance cross-language AST search and analysis for Python.
>
> *Created by Mark Newton*

## Features

- **Parse 71+ languages** - Python, Rust, JavaScript, Go, C/C++, Java, PowerShell, and more
- **Cross-language patterns** - Write patterns once, match across all languages
- **Native tree-sitter queries** - Maximum performance when you need it
- **YAML rule engine** - Define security and code quality rules
- **SARIF 2.1.0 output** - First-class CI/CD integration

## Installation

```bash
pip install uast-grep
```

## Quick Start

### Parse Source Code

```python
from uast_grep import UastParser

# Create a parser for Python
parser = UastParser("python")
tree = parser.parse("""
def hello():
    print("Hello, World!")
""")

# Explore the AST
print(tree.root().kind())  # 'module'
print(tree.node_count())   # Number of nodes
print(tree.has_error())    # False if parsing succeeded
```

### Pattern Matching

```python
from uast_grep import PatternMatcher

# UAST patterns work across languages
matcher = PatternMatcher("FunctionDeclaration")

# Match in Python
matches = matcher.matches("def hello(): pass", "python")
print(len(matches))  # 1

# Same pattern works in Rust!
matches = matcher.matches("fn hello() {}", "rust")
print(len(matches))  # 1
```

### Rule Scanning

```python
from uast_grep import RuleScanner

# Create a scanner and load rules
scanner = RuleScanner()
scanner.load_rules_from_string("""
id: no-write-host
language: powershell
severity: warning
message: "Avoid using Write-Host for output"
rule:
  pattern: "Write-Host §§§ARGS"
fix: "Write-Output §§§ARGS"
""")

# Scan code
results = scanner.scan("Write-Host 'Hello'", "powershell")
for r in results:
    print(f"{r.severity}: {r.message}")
```

### SARIF Output

```python
# Get SARIF 2.1.0 JSON for CI/CD integration
sarif = scanner.scan_to_sarif(source, "powershell", file_path="script.ps1")
print(sarif)  # Valid SARIF JSON
```

## Supported Languages

### Built-in (37 languages, instant loading)

Core, DevOps, Web, Backend, Scripting, Build/Config, Functional, Data formats

### WASM On-Demand (34 languages)

SQL, Kotlin, Swift, Scala, F#, Haskell, Julia, and more exotic languages

Use `supported_languages()` to get the full list.

## Pattern Syntax

### Metavariable Prefixes

Three prefixes are supported (use whichever you prefer):

| Prefix | Example | Notes |
|--------|---------|-------|
| `§` | `§NAME` | **Recommended** |
| `∀` | `∀NAME` | Mathematical |
| `$` | `$NAME` | Standard |

### UAST Patterns (Cross-language)

```
FunctionDeclaration   - Match function declarations
ClassDeclaration      - Match class declarations
§NAME                 - Capture single node
§§ARGS                - Capture one or more nodes
§§§ITEMS              - Capture zero or more nodes
*                     - Wildcard (any node)
```

### Native Patterns (Language-specific)

```
function_definition   - Python function
function_item         - Rust function
(identifier) @name    - S-expression query
```

## API Reference

### UastParser

- `UastParser(language)` - Create parser for a language
- `.parse(source, path=None)` - Parse source code
- `.parse_to_dict(source)` - Parse to Python dict
- `.language` - Get configured language

### UastTree

- `.root()` - Get root node
- `.walk()` - Iterate all nodes
- `.query(query)` - Execute tree-sitter query
- `.find_by_kind(kind)` - Find nodes by type
- `.has_error()` - Check for parse errors
- `.to_json()` / `.to_dict()` - Serialize AST

### PatternMatcher

- `PatternMatcher(pattern, language=None)` - Create matcher
- `.matches(source, language)` - Match against source
- `.matches_tree(tree)` - Match against parsed tree
- `.is_uast_pattern()` - Check if UAST pattern

### RuleScanner

- `RuleScanner()` - Create scanner
- `.load_rules(path)` - Load from YAML file
- `.load_rules_from_string(yaml)` - Load from string
- `.scan(source, language, file_path=None)` - Scan code
- `.scan_to_sarif(...)` - Get SARIF output
- `.rule_count()` / `.rule_ids()` - Query loaded rules

## Development

```bash
# Install maturin
pip install maturin

# Build in development mode
cd python
maturin develop --features python

# Run tests
pytest tests/
```

## License

MIT
