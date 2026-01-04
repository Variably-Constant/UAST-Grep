# CLI Reference

Complete reference for the UAST-Grep command-line interface.

## Synopsis

```bash
uast-grep [OPTIONS] <COMMAND>
```

## Global Options

| Option | Short | Description |
|--------|-------|-------------|
| `--quiet` | `-q` | Suppress progress output |
| `--verbose` | `-v` | Enable verbose output |
| `--help` | `-h` | Print help information |
| `--version` | `-V` | Print version information |

## Commands

### run

Search for pattern matches in files.

```bash
uast-grep run [OPTIONS] -p <PATTERN> -l <LANGUAGE> [PATHS]...
```

#### Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--pattern` | `-p` | Pattern to search for | *Required* |
| `--language` | `-l` | Language for parsing | *Required* |
| `--json` | | Output as JSON | `false` |
| `--count` | `-c` | Show only match counts | `false` |
| `--uast` | `-u` | Force UAST pattern interpretation | Auto-detect |
| `--jobs` | `-j` | Number of parallel jobs | CPU count |
| `--no-ignore` | | Don't respect .gitignore | `false` |
| `--hidden` | | Include hidden files | `false` |
| `--max-depth` | | Maximum directory depth | Unlimited |
| `--stdin` | | Read source from stdin | `false` |

#### Examples

```bash
# Basic search
uast-grep run -p FunctionDeclaration -l python ./src

# Search with metavariables
uast-grep run -p 'print(§§§ARGS)' -l python ./src

# Output as JSON
uast-grep run -p function_definition -l python ./src --json

# Count matches only
uast-grep run -p function_definition -l python ./src -c

# Use 4 parallel threads
uast-grep run -p function_definition -l python ./src -j 4

# Include hidden files
uast-grep run -p function_definition -l python ./src --hidden

# Limit search depth
uast-grep run -p function_definition -l python ./src --max-depth 2

# Search from stdin
echo "def foo(): pass" | uast-grep run -p function_definition -l python --stdin
```

---

### scan

Scan files using YAML rules.

```bash
uast-grep scan [OPTIONS] -r <RULES> [PATHS]...
```

#### Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--rules` | `-r` | YAML rule file or directory | *Required* |
| `--format` | `-f` | Output format (text, json, sarif) | `text` |
| `--language` | `-l` | Override language detection | Auto-detect |
| `--jobs` | `-j` | Number of parallel jobs | CPU count |
| `--no-ignore` | | Don't respect .gitignore | `false` |
| `--hidden` | | Include hidden files | `false` |

#### Examples

```bash
# Scan with single rule file
uast-grep scan -r rules/security.yaml ./src

# Scan with directory of rules
uast-grep scan -r rules/ ./src

# Output as SARIF (for CI/CD)
uast-grep scan -r rules/ -f sarif ./src > results.sarif

# Output as JSON
uast-grep scan -r rules/ -f json ./src

# Force specific language
uast-grep scan -r rules/python/ -l python ./src
```

---

### parse

Parse a file and print the AST.

```bash
uast-grep parse [OPTIONS] -l <LANGUAGE> <FILE>
```

#### Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--language` | `-l` | Language for parsing | *Required* |
| `--json` | | Output as JSON | `false` |
| `--stdin` | | Read from stdin | `false` |

#### Examples

```bash
# Parse a file and show AST
uast-grep parse example.py -l python

# Parse from stdin
echo "def foo(): pass" | uast-grep parse -l python --stdin

# Output as JSON
uast-grep parse example.py -l python --json
```

#### Output Format

```
module [0:0-5:0]
  function_definition [0:0-2:10]
    name: identifier [0:4-0:8] "main"
    parameters: parameters [0:8-0:10]
    body: block [1:4-2:10]
```

Each line shows:
- Node type
- Position `[start_line:start_col-end_line:end_col]`
- Field name (if applicable)
- Text content (for identifiers/literals)

---

### ts-query

Execute native tree-sitter query (S-expression).

```bash
uast-grep ts-query [OPTIONS] <QUERY> -l <LANGUAGE> [PATHS]...
```

#### Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--language` | `-l` | Language for parsing | *Required* |
| `--json` | | Output as JSON | `false` |
| `--stdin` | | Read from stdin | `false` |

#### Examples

```bash
# Find all functions, capture name
uast-grep ts-query "(function_definition name: (identifier) @name)" -l python ./src

# Find functions named "main"
uast-grep ts-query '(function_definition name: (identifier) @name (#eq? @name "main"))' -l python ./src

# Find strings containing "password"
uast-grep ts-query '(string) @s (#match? @s "password")' -l python ./src

# Multiple captures
uast-grep ts-query "(call function: (identifier) @func arguments: (argument_list) @args)" -l python ./src
```

#### Query Syntax

```
(node_type                    ; Match node type
  field_name: (child_type)    ; Match child by field name
  (anonymous_child)           ; Match anonymous child
  @capture_name               ; Capture matched node
)

; Predicates
(#eq? @capture "value")       ; Exact string match
(#match? @capture "regex")    ; Regex match
(#not-eq? @a @b)             ; Not equal
```

---

### languages

List supported languages.

```bash
uast-grep languages [OPTIONS]
```

#### Options

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |
| `--builtin` | Show only built-in languages |
| `--wasm` | Show only WASM languages |

#### Examples

```bash
# List all languages
uast-grep languages

# List as JSON
uast-grep languages --json

# List only built-in
uast-grep languages --builtin

# List only WASM
uast-grep languages --wasm
```

---

## Output Formats

### Text (Default)

```
./src/main.py:5:1: def process_data():
./src/main.py:15:1: def validate_input(data):
```

### JSON

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

### SARIF

SARIF (Static Analysis Results Interchange Format) 2.1.0 for CI/CD integration:

```json
{
  "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
  "version": "2.1.0",
  "runs": [
    {
      "tool": {
        "driver": {
          "name": "uast-grep",
          "version": "0.1.0",
          "rules": [...]
        }
      },
      "results": [...]
    }
  ]
}
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `UAST_WASM_PATH` | Custom WASM grammar directory | `~/.uast/grammars/` |
| `UAST_OFFLINE` | Disable WASM auto-download | `0` |
| `UAST_WASM_URL` | Custom WASM download URL | GitHub Releases |
| `NO_COLOR` | Disable colored output | Not set |
| `TERM` | Terminal type (affects colors) | Auto-detect |

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success (matches found or no errors) |
| `1` | Error (invalid arguments, parse failure, etc.) |
| `2` | No matches found (when `--no-matches-exit` is used) |

## Tips

### Performance

```bash
# Use more threads for large codebases
uast-grep run -p FunctionDeclaration -l python ./src -j 8

# Limit depth for faster searches
uast-grep run -p FunctionDeclaration -l python ./src --max-depth 3

# Ignore tests for faster scans
uast-grep run -p FunctionDeclaration -l python ./src --hidden
```

### CI/CD Integration

```bash
# GitHub Actions with SARIF
uast-grep scan -r rules/security/ -f sarif ./src > results.sarif
gh api -X POST /repos/{owner}/{repo}/code-scanning/sarifs \
  -H "Content-Type: application/json" \
  -d @results.sarif

# GitLab CI with JSON
uast-grep scan -r rules/ -f json ./src > gl-code-quality-report.json
```

### Shell Integration

```bash
# Add to PATH (Unix)
export PATH="$PATH:/path/to/uast-grep"

# Add to PATH (Windows PowerShell)
$env:PATH += ";C:\path\to\uast-grep"

# Create alias
alias ug='uast-grep'
```

### Debugging

```bash
# See what UAST types map to
uast-grep run -p FunctionDeclaration -l python ./src --verbose

# See the full AST of a file
uast-grep parse suspicious-file.py -l python

# Test a pattern interactively
echo "test code" | uast-grep run -p 'pattern' -l python --stdin
```
