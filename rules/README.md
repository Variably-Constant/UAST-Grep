# UAST-Grep Rules

> YAML rule files for code scanning with pattern-based detection and auto-fix.
>
> *Created by Mark Newton*

## Built-in Rules

UAST-Grep ships with **3,873 built-in rules** embedded directly in the binary:

| Ruleset | Rules | Description |
|---------|-------|-------------|
| **Security** | 1,587 | Vulnerability detection covering 179 CWEs and OWASP Top 10 |
| **Performance** | 1,334 | Performance anti-patterns and optimization opportunities |
| **Quality** | 952 | Code smells, complexity, naming, and best practices |

Rules cover **31 languages** with language-specific patterns, plus **universal rules** that work across all 71 supported languages.

## Quick Start

```bash
# Run ALL built-in rules (zero configuration)
uast-grep scan ./src

# Run specific rule categories
uast-grep scan -r security ./src
uast-grep scan -r performance ./src
uast-grep scan -r quality ./src

# Run external/custom rules only
uast-grep scan -e ./my-rules.yaml ./src

# Merge external rules WITH built-in rules
uast-grep scan -e ./my-rules.yaml -r security ./src
```

## CLI Options

| Option | Description |
|--------|-------------|
| `-r, --ruleset` | Built-in ruleset: `security`, `performance`, `quality`, `all` (default), `none` |
| `-e, --external` | Path to external/custom YAML rule file |
| `-f, --format` | Output format: `text`, `json`, `sarif` |
| `--fix` | Apply auto-fixes |
| `--dry-run` | Show fixes without applying |
| `-s, --severity` | Minimum severity: `hint`, `info`, `warning`, `error` |

### Smart Merging Logic

- **No options**: Runs ALL built-in rules (3,873 rules)
- **`-e` only**: Runs ONLY external rules (no built-in)
- **`-r` only**: Runs ONLY specified built-in ruleset
- **`-e` + `-r`**: MERGES external with built-in rules

## External Rules

Place custom rule files in this `rules/` directory or specify paths with `-e`:

```bash
# Load from rules directory
uast-grep scan -e rules/my-custom-rules.yaml ./src

# Load multiple external rule files
uast-grep scan -e rules/team-standards.yaml -r security ./src
```

## Rule Format

```yaml
id: unique-rule-id
language: powershell           # or "universal" for cross-language
severity: error|warning|info|hint
message: "Description with $CAPTURED refs"

rule:
  pattern: "Write-Host $$$ARGS"      # Simple pattern
  # OR: any/all/not for composite rules

constraints:                          # Optional
  VARNAME:
    kind: "HashtableAst"
    regex: "^pattern$"

fix: "replacement with $VARNAME"      # Optional auto-fix
url: "https://..."                    # Reference documentation
tags: [security, cwe-78]              # Categorization
```

## Metavariable Prefixes

UAST-Grep supports three equivalent prefixes:

| Prefix | Example | Notes |
|--------|---------|-------|
| `$` | `$NAME` | **Standard** - use single quotes in PowerShell |
| `§` | `§NAME` | Symbol alternative - clearly a metavar |
| `∀` | `∀NAME` | Mathematical ("for all") |

## Pattern Syntax

| Pattern | Meaning |
|---------|---------|
| `$NAME` | Capture single node as metavariable |
| `$$NAME` | One or more nodes |
| `$$$NAME` | Zero or more nodes |
| `$_` | Anonymous capture (match but don't bind) |
| `*` | Wildcard |

## Composite Rules

```yaml
# Match ANY pattern (OR logic)
rule:
  any:
    - pattern: "Invoke-Expression $$$ARGS"
    - pattern: "iex $$$ARGS"

# Match ALL patterns (AND logic)
rule:
  all:
    - pattern: "SELECT $$$COLS FROM $TABLE"
    - pattern: "$$$INPUT"
      inside:
        kind: string_literal

# Negate a pattern
rule:
  not:
    pattern: "parameterized_query($$$)"
```

## Relational Patterns

```yaml
rule:
  pattern: "$VAR"
  inside:                    # Must be inside this ancestor
    kind: function_definition
    stopBy: neighbor         # or "end" (default)
  has:                       # Must contain this descendant
    pattern: "dangerous_call()"
  notInside:                 # Must NOT be inside
    kind: try_statement
  notHas:                    # Must NOT contain
    pattern: "sanitize($VAR)"
```

## Constraints

```yaml
constraints:
  VARNAME:
    kind: "identifier"           # AST node type
    regex: "^user_"              # Text must match
    notKind: "string_literal"    # Must NOT be this type
    notRegex: "^safe_"           # Text must NOT match
    pattern:                     # Must match sub-pattern
      pattern: "$X.$Y"
```

## Example Rules

### Security: SQL Injection Detection

```yaml
id: sql-injection-string-concat
language: universal
severity: error
message: "Potential SQL injection via string concatenation"
tags: [security, cwe-89, owasp-a03]

rule:
  all:
    - pattern: "$QUERY + $INPUT"
    - pattern: "$QUERY"
      regex: "(?i)(SELECT|INSERT|UPDATE|DELETE)"

fix: "Use parameterized queries instead"
url: "https://cwe.mitre.org/data/definitions/89.html"
```

### Performance: String Concatenation in Loop

```yaml
id: string-concat-in-loop
language: universal
severity: warning
message: "String concatenation in loop causes O(n^2) allocations"
tags: [performance]

rule:
  pattern: "$STR += $$$"
  inside:
    any:
      - kind: for_statement
      - kind: while_statement
      - kind: foreach_statement

fix: "Use StringBuilder or collect strings and join once"
```

### Quality: Empty Catch Block

```yaml
id: empty-catch-block
language: universal
severity: error
message: "Empty catch block swallows exceptions silently"
tags: [quality, code-smell]

rule:
  all:
    - kind: catch_clause
    - notHas:
        any:
          - kind: expression_statement
          - kind: throw_statement
```

## Files in This Directory

| File | Description |
|------|-------------|
| `powershell-best-practices.yaml` | PowerShell coding standards |
| *(Add your custom rules here)* | External rules loaded via `-e` |

## Built-in Rule Locations

The built-in rules are embedded in the UAST.Core assembly from:

```
src/UAST.Core/Rules/BuiltIn/
  universal-security.yaml      # 1,587 security rules
  universal-performance.yaml   # 1,334 performance rules
  universal-quality.yaml       # 952 quality rules
```

## Documentation

For comprehensive documentation, see the [UAST-Grep Documentation](https://markusmcnugen.github.io/UAST-Grep/):

- [Rule Syntax](https://markusmcnugen.github.io/UAST-Grep/rules/syntax.html) - Complete rule authoring guide
- [Security Rules](https://markusmcnugen.github.io/UAST-Grep/rules/security.html) - Security rule details
- [Performance Rules](https://markusmcnugen.github.io/UAST-Grep/rules/performance.html) - Performance rule details
- [Quality Rules](https://markusmcnugen.github.io/UAST-Grep/rules/quality.html) - Quality rule details
- [Rules Catalog](https://markusmcnugen.github.io/UAST-Grep/rules/rules-catalog.html) - Complete rule index
- [CWE/OWASP Coverage](https://markusmcnugen.github.io/UAST-Grep/rules/cwe-owasp-coverage.html) - Security standards mapping
