# Python API

The Python API provides full access to UAST-Grep functionality via PyO3 bindings.

## Installation

```bash
# From PyPI
pip install uast-grep

# From source
cd UAST-Grep/python
pip install maturin
maturin develop --features python
```

## Quick Start

```python
from uast_grep import UastParser, PatternMatcher, RuleScanner

# Parse source code
parser = UastParser("python")
tree = parser.parse("def hello(): pass")
print(f"Root: {tree.root().kind()}")

# Pattern matching
matcher = PatternMatcher("FunctionDeclaration")
matches = matcher.matches("def hello(): pass", "python")
print(f"Found {len(matches)} matches")

# Rule scanning
scanner = RuleScanner()
scanner.load_rules_from_string('''
id: find-functions
language: python
severity: info
message: "Found function"
rule:
  pattern: FunctionDeclaration
''')
results = scanner.scan("def hello(): pass", "python")
```

## API Reference

### UastParser

Parses source code into an AST.

```python
from uast_grep import UastParser

# Create parser for a language
parser = UastParser("python")

# Parse source code
tree = parser.parse(source_code)

# Parse from file
tree = parser.parse_file("script.py")
```

#### Methods

| Method | Description |
|--------|-------------|
| `__init__(language: str)` | Create parser for language |
| `parse(source: str) -> UastTree` | Parse source string |
| `parse_file(path: str) -> UastTree` | Parse file |

### UastTree

Represents a parsed syntax tree.

```python
tree = parser.parse(source)

# Get root node
root = tree.root()

# Check for parse errors
if tree.has_errors():
    print("Parse errors detected")

# Get node count
print(f"Total nodes: {tree.node_count()}")
```

#### Methods

| Method | Description |
|--------|-------------|
| `root() -> UastNode` | Get root node |
| `has_errors() -> bool` | Check for parse errors |
| `node_count() -> int` | Total node count |

### UastNode

Represents a node in the syntax tree.

```python
node = tree.root()

# Basic info
print(f"Kind: {node.kind()}")
print(f"Text: {node.text()}")
print(f"Range: {node.range()}")

# Navigation
for child in node.children():
    print(f"  Child: {child.kind()}")

if node.parent():
    print(f"Parent: {node.parent().kind()}")

# Named children only
for child in node.named_children():
    print(f"  Named: {child.kind()}")

# Get child by field name
name_node = node.child_by_field("name")
if name_node:
    print(f"Name: {name_node.text()}")
```

#### Methods

| Method | Description |
|--------|-------------|
| `kind() -> str` | Node type |
| `text() -> str` | Source text |
| `range() -> SourceSpan` | Position info |
| `start_line() -> int` | Start line (0-indexed) |
| `start_column() -> int` | Start column |
| `end_line() -> int` | End line |
| `end_column() -> int` | End column |
| `parent() -> Optional[UastNode]` | Parent node |
| `children() -> List[UastNode]` | All children |
| `named_children() -> List[UastNode]` | Named children only |
| `child(index: int) -> Optional[UastNode]` | Child by index |
| `child_by_field(name: str) -> Optional[UastNode]` | Child by field name |
| `is_named() -> bool` | Is named node |
| `is_missing() -> bool` | Is missing (error) |
| `has_error() -> bool` | Has error in subtree |

### PatternMatcher

Matches patterns against source code.

```python
from uast_grep import PatternMatcher

# Create matcher with pattern
matcher = PatternMatcher("FunctionDeclaration")

# Match against source
matches = matcher.matches(source, "python")

for match in matches:
    print(f"Found at line {match.line}")
    print(f"Text: {match.text}")

# Use with metavariables
matcher = PatternMatcher("print(§§§ARGS)")
matches = matcher.matches("print('hello', 'world')", "python")
```

#### Methods

| Method | Description |
|--------|-------------|
| `__init__(pattern: str)` | Create matcher |
| `matches(source: str, language: str) -> List[MatchResult]` | Find matches |
| `matches_file(path: str, language: str) -> List[MatchResult]` | Match in file |

### MatchResult

Represents a pattern match.

```python
for match in matches:
    print(f"Line: {match.line}")
    print(f"Column: {match.column}")
    print(f"Text: {match.text}")
    print(f"Kind: {match.node_kind}")
    print(f"Span: {match.span}")
```

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `line` | `int` | Line number (1-indexed) |
| `column` | `int` | Column number (1-indexed) |
| `text` | `str` | Matched text |
| `node_kind` | `str` | Node type |
| `span` | `SourceSpan` | Full position info |

### RuleScanner

Scans code with YAML rules.

```python
from uast_grep import RuleScanner

scanner = RuleScanner()

# Load rules from string
scanner.load_rules_from_string(yaml_rules)

# Load rules from file
scanner.load_rules_from_file("rules/security.yaml")

# Load rules from directory
scanner.load_rules_from_directory("rules/")

# Scan source code
results = scanner.scan(source, "python")

# Scan file
results = scanner.scan_file("script.py")

# Scan directory
results = scanner.scan_directory("./src", "python")

for result in results:
    print(f"[{result.severity}] {result.message}")
    print(f"  {result.path}:{result.line}")
```

#### Methods

| Method | Description |
|--------|-------------|
| `__init__()` | Create scanner |
| `load_rules_from_string(yaml: str)` | Load rules from YAML string |
| `load_rules_from_file(path: str)` | Load rules from file |
| `load_rules_from_directory(path: str)` | Load rules from directory |
| `scan(source: str, language: str) -> List[ScanResult]` | Scan source |
| `scan_file(path: str) -> List[ScanResult]` | Scan file |
| `scan_directory(path: str, language: str) -> List[ScanResult]` | Scan directory |

### ScanResult

Represents a rule match.

```python
for result in results:
    print(f"Rule: {result.rule_id}")
    print(f"Severity: {result.severity}")  # error, warning, info
    print(f"Message: {result.message}")
    print(f"Path: {result.path}")
    print(f"Line: {result.line}")
    print(f"Column: {result.column}")
    if result.fix:
        print(f"Fix: {result.fix}")
```

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `rule_id` | `str` | Rule identifier |
| `severity` | `str` | Severity level |
| `message` | `str` | Human-readable message |
| `path` | `str` | File path |
| `line` | `int` | Line number |
| `column` | `int` | Column number |
| `fix` | `Optional[str]` | Suggested fix |
| `tags` | `List[str]` | Rule tags |

### Utility Functions

```python
from uast_grep import (
    supported_languages,
    language_for_extension,
    get_extensions,
    is_uast_pattern,
    version
)

# Get version
print(f"UAST-Grep version: {version()}")

# List supported languages
for lang in supported_languages():
    print(lang)

# Get language from extension
lang = language_for_extension(".py")  # "python"

# Get extensions for language
exts = get_extensions("python")  # [".py", ".pyw", ".pyi"]

# Check if pattern is UAST type
is_uast = is_uast_pattern("FunctionDeclaration")  # True
is_uast = is_uast_pattern("function_definition")   # False
```

## Examples

### Find All Functions

```python
from uast_grep import PatternMatcher

matcher = PatternMatcher("FunctionDeclaration")
matches = matcher.matches_file("app.py", "python")

for match in matches:
    print(f"{match.line}: {match.text}")
```

### Security Scanner

```python
from uast_grep import RuleScanner

scanner = RuleScanner()
scanner.load_rules_from_string('''
id: dangerous-eval
language: python
severity: error
message: "eval() is dangerous"
rule:
  pattern: "eval(§§§ARGS)"
---
id: sql-injection
language: python
severity: error
message: "Possible SQL injection"
rule:
  any:
    - pattern: 'cursor.execute(§QUERY + §INPUT)'
    - pattern: 'cursor.execute(f"§§§SQL")'
''')

import os
for root, dirs, files in os.walk("./src"):
    for file in files:
        if file.endswith(".py"):
            path = os.path.join(root, file)
            results = scanner.scan_file(path)
            for result in results:
                print(f"{result.severity}: {result.path}:{result.line} - {result.message}")
```

### AST Explorer

```python
from uast_grep import UastParser

def print_tree(node, indent=0):
    prefix = "  " * indent
    field = f"{node.field_name()}: " if hasattr(node, 'field_name') else ""
    print(f"{prefix}{field}{node.kind()} [{node.start_line()}:{node.start_column()}]")
    for child in node.children():
        print_tree(child, indent + 1)

parser = UastParser("python")
tree = parser.parse_file("example.py")
print_tree(tree.root())
```

### Custom Rule Engine

```python
from uast_grep import UastParser, PatternMatcher

class SecurityChecker:
    def __init__(self):
        self.rules = [
            ("eval(§§§ARGS)", "Dangerous eval()"),
            ("exec(§§§ARGS)", "Dangerous exec()"),
            ("os.system(§§§ARGS)", "Shell injection risk"),
        ]

    def check(self, source: str, language: str = "python"):
        findings = []
        for pattern, message in self.rules:
            matcher = PatternMatcher(pattern)
            for match in matcher.matches(source, language):
                findings.append({
                    "line": match.line,
                    "column": match.column,
                    "message": message,
                    "code": match.text
                })
        return findings

checker = SecurityChecker()
with open("app.py") as f:
    findings = checker.check(f.read())
    for finding in findings:
        print(f"Line {finding['line']}: {finding['message']}")
```

## Error Handling

```python
from uast_grep import UastParser, UastParseError

try:
    parser = UastParser("invalid_language")
except ValueError as e:
    print(f"Unknown language: {e}")

try:
    tree = parser.parse("invalid {{ code")
    if tree.has_errors():
        print("Parse errors detected")
except UastParseError as e:
    print(f"Parse error: {e}")
```

## Thread Safety

The Python API is thread-safe. Each parser instance maintains its own state:

```python
from concurrent.futures import ThreadPoolExecutor
from uast_grep import PatternMatcher

def scan_file(path):
    matcher = PatternMatcher("FunctionDeclaration")
    return matcher.matches_file(path, "python")

with ThreadPoolExecutor(max_workers=4) as executor:
    files = ["a.py", "b.py", "c.py", "d.py"]
    results = list(executor.map(scan_file, files))
```
