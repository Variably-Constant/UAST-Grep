# YAML Rule Syntax

UAST-Grep uses YAML files to define reusable search patterns and security rules.

## Basic Structure

```yaml
id: rule-identifier
language: python
severity: error
message: "Description for developers"

rule:
  pattern: "code_pattern"

fix: "suggested_fix"  # Optional
```

## Required Fields

| Field | Description | Example |
|-------|-------------|---------|
| `id` | Unique rule identifier | `no-eval` |
| `language` | Target language | `python`, `javascript` |
| `severity` | Issue severity | `error`, `warning`, `info`, `hint` |
| `message` | Human-readable description | `"eval() is dangerous"` |
| `rule` | The matching rule | See below |

## Optional Fields

| Field | Description | Example |
|-------|-------------|---------|
| `fix` | Suggested fix text | `"Use ast.literal_eval()"` |
| `tags` | Categorization tags | `[security, injection]` |
| `note` | Additional notes | `"See CWE-94"` |
| `examples` | Test cases | See below |

## Rule Types

### Pattern Rules

Match code patterns with metavariables:

```yaml
rule:
  pattern: "eval(§§§ARGS)"
```

```yaml
rule:
  pattern: |
    if §COND:
        §BODY
```

### Kind Rules

Match by AST node type:

```yaml
rule:
  kind: function_definition
```

### Regex Rules

Match node text with regex:

```yaml
rule:
  regex: "password|secret|api_key"
```

### Kind + Regex Combination

Combine kind filtering with regex for precise matching:

```yaml
# Match Comment nodes containing TODO (case-insensitive)
rule:
  kind: Comment
  regex: "(?i)\\bTODO\\b"

# Match string literals containing hardcoded paths
rule:
  kind: string_literal
  regex: "([A-Z]:|/home|/root)[/\\\\]"
```

## Composite Rules

### Any (OR)

Match if any sub-rule matches:

```yaml
rule:
  any:
    - pattern: "eval(§§§ARGS)"
    - pattern: "exec(§§§ARGS)"
    - pattern: "compile(§§§ARGS)"
```

### All (AND)

Match if all sub-rules match:

```yaml
rule:
  all:
    - kind: function_definition
    - pattern: "def §NAME(§§§PARAMS):"
    - has:
        kind: decorator
```

### Not

Exclude matches:

```yaml
rule:
  pattern: "print(§§§ARGS)"
  not:
    inside:
      kind: function_definition
      has:
        pattern: "def test_§NAME"
```

## Relational Rules

### Has / NotHas

Match if node contains (or doesn't contain) a descendant matching the rule:

```yaml
# Function with a return statement
rule:
  kind: function_definition
  has:
    pattern: "return §VALUE"

# Empty if block (no Statement child)
rule:
  kind: IfStatement
  notHas:
    kind: Statement
```

#### Nested Has (Deep Matching)

Match nodes with specific nesting depth. This is powerful for detecting:
- Deep nesting (4+ nested ifs)
- Long condition chains (6+ else-if branches)
- Triple nested loops

```yaml
# Detect 4+ levels of nested if statements
rule:
  kind: IfStatement
  has:
    kind: IfStatement
    has:
      kind: IfStatement
      has:
        kind: IfStatement
```

```yaml
# Detect long if/else-if chains (6+ branches)
rule:
  kind: IfStatement
  has:
    kind: ElseIfClause
    has:
      kind: ElseIfClause
      has:
        kind: ElseIfClause
        has:
          kind: ElseIfClause
          has:
            kind: ElseIfClause
```

### Inside / NotInside

Match if node is (or isn't) inside a matching ancestor:

```yaml
# Match only inside a specific context
rule:
  pattern: "§A == §A"
  inside:
    kind: if_statement

# Skip matches in test methods
rule:
  kind: Comment
  regex: "(?i)\\bTODO\\b"
  notInside:
    kind: TestMethod
```

### Follows / Precedes

Match based on sibling relationships:

```yaml
rule:
  pattern: "print(§MSG)"
  follows:
    pattern: "try:"
    immediate: true  # Must be immediately after

rule:
  pattern: "§VAR = §VALUE"
  precedes:
    pattern: "del §VAR"
```

### Relation Options

| Option | Values | Description |
|--------|--------|-------------|
| `stop_by` | `end` (default), `neighbor` | `end` searches all descendants/ancestors, `neighbor` only checks immediate children/parent |
| `immediate` | `true`, `false` | For precedes/follows: require immediate siblings |

**Tip**: Use `stop_by: neighbor` when detecting empty blocks to avoid false positives from nested structures.

```yaml
# Only check immediate parent
inside:
  kind: TryStatement
  stop_by: neighbor

# Only check immediate children (for empty block detection)
has:
  kind: Block
  stop_by: neighbor
  notHas:
    kind: Statement

# Must be immediately before the next sibling
precedes:
  kind: ElseClause
  immediate: true
```

## Constraints

Add constraints to metavariables:

```yaml
rule:
  pattern: "§VAR = §VALUE"

constraints:
  VAR:
    regex: "^(password|secret|api_key)$"
  VALUE:
    kind: string
```

### Constraint Types

| Type | Description | Example |
|------|-------------|---------|
| `kind` | Required AST node type | `kind: string` |
| `regex` | Required text pattern | `regex: "^test_"` |
| `pattern` | Required sub-pattern | `pattern: "§A.§B"` |
| `notKind` | Prohibited AST node type | `notKind: string_literal` |
| `notRegex` | Prohibited text pattern | `notRegex: "^_"` |

```yaml
constraints:
  VAR:
    kind: "identifier"        # Must be identifier
    regex: "^(test_|spec_)"   # Must match regex
    notRegex: "^_internal"    # Must NOT match this
  VALUE:
    notKind: "string_literal" # Must NOT be a string
```

## Examples Section

Define test cases for your rules:

```yaml
examples:
  match:
    - "eval(user_input)"
    - "eval(request.data)"
  no-match:
    - "eval('1 + 1')"
    - "literal_eval(data)"
```

## Complete Example

```yaml
id: sql-injection
language: python
severity: error
message: "Possible SQL injection vulnerability"
tags:
  - security
  - injection
  - CWE-89
note: "Use parameterized queries instead of string formatting"

rule:
  any:
    - pattern: 'cursor.execute(§QUERY + §INPUT)'
    - pattern: 'cursor.execute(§QUERY % §INPUT)'
    - pattern: 'cursor.execute(f"§§§SQL")'
    - pattern: 'cursor.execute(§QUERY.format(§§§ARGS))'

constraints:
  QUERY:
    regex: "SELECT|INSERT|UPDATE|DELETE"

fix: "cursor.execute(query, (param1, param2))"

examples:
  match:
    - 'cursor.execute("SELECT * FROM users WHERE id = " + user_id)'
    - 'cursor.execute(f"SELECT * FROM users WHERE id = {user_id}")'
  no-match:
    - 'cursor.execute("SELECT * FROM users WHERE id = ?", (user_id,))'
```

## Multiple Rules

Separate rules with `---`:

```yaml
id: no-eval
language: python
severity: error
message: "eval() is dangerous"
rule:
  pattern: "eval(§§§ARGS)"
---
id: no-exec
language: python
severity: error
message: "exec() is dangerous"
rule:
  pattern: "exec(§§§ARGS)"
---
id: no-compile
language: python
severity: warning
message: "compile() with user input is dangerous"
rule:
  pattern: "compile(§INPUT, §§§REST)"
```

## Rule File Organization

Recommended structure:

```
rules/
├── security/
│   ├── injection.yaml
│   ├── authentication.yaml
│   └── cryptography.yaml
├── quality/
│   ├── complexity.yaml
│   └── style.yaml
├── performance/
│   └── optimization.yaml
└── custom/
    └── project-specific.yaml
```

## Loading Rules

```bash
# Load single file
uast-grep scan -r rules/security/injection.yaml ./src

# Load directory (all .yaml files)
uast-grep scan -r rules/security/ ./src

# Load all rules
uast-grep scan -r rules/ ./src
```

## Debugging Rules

```bash
# Test a rule against code
echo "eval(user_input)" | uast-grep scan -r rule.yaml -l python --stdin

# Verbose output
uast-grep scan -r rule.yaml ./src --verbose

# See matched node types
uast-grep parse test-file.py -l python
```

## Best Practices

1. **Use descriptive IDs** - `sql-injection` not `rule1`
2. **Write clear messages** - Explain what's wrong and why
3. **Provide fixes** - Help developers remediate
4. **Add test cases** - Validate rules work correctly
5. **Use tags** - Enable filtering and categorization
6. **Start simple** - Add complexity only when needed
7. **Document edge cases** - Note in `note` field
8. **Use `stop_by: neighbor` for empty-* rules** - When detecting empty blocks, use `stop_by: neighbor` to only check immediate children:
   ```yaml
   rule:
     kind: IfStatement
     has:
       kind: Block
       stop_by: neighbor  # Only check direct children
       notHas:
         kind: ExpressionStatement
   ```
