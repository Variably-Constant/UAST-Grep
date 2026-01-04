# Quick Start

This guide will get you searching code in under 5 minutes.

## Basic Search

The most common operation is searching for patterns in source files:

```bash
# Find all function declarations in Python files
uast-grep run -p FunctionDeclaration -l python ./src

# Find all class definitions in TypeScript
uast-grep run -p ClassDeclaration -l typescript ./src

# Find all if statements in Go
uast-grep run -p IfStatement -l go ./src
```

## Understanding the Output

When UAST-Grep finds matches, it displays them in a grep-like format:

```
./src/main.py:5:1: def process_data():
./src/main.py:15:1: def validate_input(data):
./src/utils.py:3:1: def helper():
```

Each line shows:
- **File path** - Where the match was found
- **Line:Column** - Exact position in the file
- **Preview** - The matched code

## Output Formats

### Count Mode

Just want to know how many matches? Use `-c`:

```bash
uast-grep run -p FunctionDeclaration -l python ./src -c
# Output:
# ./src/main.py:3
# ./src/utils.py:7
```

### JSON Output

For scripting and CI integration:

```bash
uast-grep run -p FunctionDeclaration -l python ./src --json
```

```json
{
  "files_scanned": 10,
  "total_matches": 25,
  "files_with_matches": 8,
  "results": [
    {
      "path": "./src/main.py",
      "matches": [
        {
          "line": 5,
          "column": 1,
          "text": "def process_data():",
          "node_kind": "function_definition"
        }
      ]
    }
  ]
}
```

## UAST vs Native Patterns

UAST-Grep supports two types of patterns:

### UAST Patterns (Cross-Language)

Use PascalCase for universal patterns that work across languages:

```bash
# Same pattern works for any language!
uast-grep run -p FunctionDeclaration -l python ./src
uast-grep run -p FunctionDeclaration -l rust ./src
uast-grep run -p FunctionDeclaration -l javascript ./src
```

### Native Patterns (Language-Specific)

Use snake_case for tree-sitter's native node types:

```bash
# Python-specific
uast-grep run -p function_definition -l python ./src

# Rust-specific
uast-grep run -p function_item -l rust ./src

# JavaScript-specific
uast-grep run -p function_declaration -l javascript ./src
```

> **Tip:** Use `uast-grep parse` to see what native node types are available for a language.

## Exploring the AST

To understand what patterns are available, parse a file:

```bash
# Parse a file and show the AST
uast-grep parse example.py -l python
```

Output:
```
module [0:0-10:0]
  function_definition [0:0-2:10]
    name: identifier [0:4-0:8] "main"
    parameters: parameters [0:8-0:10]
    body: block [1:4-2:10]
      expression_statement [1:4-1:18]
        call [1:4-1:18]
          function: identifier [1:4-1:9] "print"
          arguments: argument_list [1:9-1:18]
            string [1:10-1:17] "Hello"
```

## YAML Rule Scanning

For complex patterns and security checks, use YAML rules:

```yaml
# security-rules.yaml
id: no-eval
language: python
severity: error
message: "eval() is dangerous - use ast.literal_eval() instead"
rule:
  pattern: "eval(§§§ARGS)"
```

Run the scan:

```bash
uast-grep scan -r security-rules.yaml ./src
```

## Common Workflows

### Find All TODOs (Comments)

```bash
uast-grep run -p Comment -l python ./src | grep -i todo
```

### Security Audit

```bash
# Find dangerous patterns
uast-grep scan -r rules/security/ -f sarif ./src > security-report.sarif
```

### Pre-commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
if uast-grep scan -r rules/security/ ./src --quiet; then
    echo "Security check passed"
    exit 0
else
    echo "Security issues found!"
    exit 1
fi
```

## Filtering Files

### By Extension (Automatic)

UAST-Grep automatically filters files by extension based on the language:

```bash
# Only searches .py files
uast-grep run -p FunctionDeclaration -l python ./src
```

### Ignore Patterns

Respects `.gitignore` by default. Override with:

```bash
# Include hidden files
uast-grep run -p FunctionDeclaration -l python ./src --hidden

# Ignore .gitignore
uast-grep run -p FunctionDeclaration -l python ./src --no-ignore
```

### Limit Depth

```bash
# Only search top-level directory
uast-grep run -p FunctionDeclaration -l python ./src --max-depth 1
```

## Parallel Processing

UAST-Grep uses all CPU cores by default. Control parallelism with:

```bash
# Use 4 threads
uast-grep run -p FunctionDeclaration -l python ./src -j 4

# Single-threaded (useful for debugging)
uast-grep run -p FunctionDeclaration -l python ./src -j 1
```

## Piping Input

Search code from stdin:

```bash
echo 'def hello(): pass' | uast-grep run -p FunctionDeclaration -l python --stdin

# From another command
cat script.py | uast-grep run -p FunctionDeclaration -l python --stdin
```

## Next Steps

- Learn about [Metavariables](../patterns/metavariables.md) for capturing code
- Explore the full [CLI Reference](../integrations/cli.md)
- Write your own [YAML Rules](../rules/syntax.md)
