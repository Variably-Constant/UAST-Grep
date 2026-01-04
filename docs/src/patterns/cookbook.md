# Pattern Cookbook

Ready-to-use patterns for common search scenarios.

## Security Patterns

### Dangerous Function Calls

```yaml
# Python
id: dangerous-eval
language: python
severity: error
message: "eval() with dynamic input is a code injection risk"
rule:
  any:
    - pattern: "eval(§INPUT)"
    - pattern: "exec(§INPUT)"
    - pattern: "compile(§INPUT, §§§REST)"
```

```yaml
# JavaScript
id: dangerous-eval-js
language: javascript
severity: error
message: "eval() is dangerous"
rule:
  any:
    - pattern: "eval(§INPUT)"
    - pattern: "new Function(§§§ARGS)"
    - pattern: "setTimeout(§STRING, §§§REST)"
    - pattern: "setInterval(§STRING, §§§REST)"
```

```yaml
# PowerShell
id: dangerous-invoke-expression
language: powershell
severity: error
message: "Invoke-Expression can execute arbitrary code"
rule:
  any:
    - pattern: "Invoke-Expression §§§ARGS"
    - pattern: "iex §§§ARGS"
    - pattern: "& §DYNAMIC"
```

### SQL Injection

```yaml
id: sql-injection
language: python
severity: error
message: "Possible SQL injection - use parameterized queries"
rule:
  any:
    - pattern: 'cursor.execute(§QUERY + §INPUT)'
    - pattern: 'cursor.execute(§QUERY % §INPUT)'
    - pattern: 'cursor.execute(f"§§§SQL")'
    - pattern: 'cursor.execute(§QUERY.format(§§§ARGS))'
```

### Hardcoded Secrets

```yaml
id: hardcoded-password
language: python
severity: error
message: "Hardcoded password detected"
rule:
  pattern: "§VAR = §VALUE"
  constraints:
    VAR:
      regex: "(?i)(password|passwd|pwd|secret|api_key|apikey|token)"
    VALUE:
      kind: string
```

### Insecure Random

```yaml
id: insecure-random
language: python
severity: warning
message: "random module is not cryptographically secure"
rule:
  any:
    - pattern: "random.random()"
    - pattern: "random.randint(§§§ARGS)"
    - pattern: "random.choice(§§§ARGS)"
fix: "# Use secrets module for security-sensitive randomness"
```

## Code Quality Patterns

### Empty Exception Handlers

```yaml
# Python
id: empty-except
language: python
severity: warning
message: "Empty except block hides errors"
rule:
  pattern: |
    except§§§EXCEPTION:
        pass
```

```yaml
# JavaScript
id: empty-catch
language: javascript
severity: warning
message: "Empty catch block hides errors"
rule:
  kind: catch_clause
  has:
    kind: statement_block
    not:
      has:
        kind: _  # Any child = not empty
```

### Debug Statements

```yaml
# Python
id: debug-statements
language: python
severity: warning
message: "Remove debug statement before committing"
rule:
  any:
    - pattern: "print(§§§ARGS)"
    - pattern: "pprint(§§§ARGS)"
    - pattern: "breakpoint()"
    - pattern: "import pdb"
    - pattern: "pdb.set_trace()"
```

```yaml
# JavaScript
id: console-log
language: javascript
severity: info
message: "Remove console.log before production"
rule:
  any:
    - pattern: "console.log(§§§ARGS)"
    - pattern: "console.debug(§§§ARGS)"
    - pattern: "console.warn(§§§ARGS)"
    - pattern: "debugger"
```

### TODO Comments

```yaml
id: todo-comments
language: python
severity: info
message: "TODO comment found"
rule:
  kind: comment
  regex: "(?i)\\b(TODO|FIXME|HACK|XXX)\\b"
```

### Unused Variables (Simple)

```yaml
id: unused-variable
language: python
severity: info
message: "Variable assigned but possibly unused"
rule:
  pattern: "§VAR = §VALUE"
  constraints:
    VAR:
      regex: "^_"  # Underscore prefix = intentionally unused
      negate: true
```

## Performance Patterns

### N+1 Query Detection

```yaml
id: n-plus-one
language: python
severity: warning
message: "Possible N+1 query - consider prefetching"
rule:
  pattern: |
    for §ITEM in §QUERYSET:
        §ITEM.§RELATION
```

### Unnecessary List Comprehension

```yaml
id: unnecessary-list-comprehension
language: python
severity: info
message: "Use generator expression instead of list comprehension"
rule:
  any:
    - pattern: "sum([§EXPR for §VAR in §ITER])"
    - pattern: "any([§EXPR for §VAR in §ITER])"
    - pattern: "all([§EXPR for §VAR in §ITER])"
    - pattern: "min([§EXPR for §VAR in §ITER])"
    - pattern: "max([§EXPR for §VAR in §ITER])"
fix: "Use generator: sum(§EXPR for §VAR in §ITER)"
```

### String Concatenation in Loop

```yaml
id: string-concat-loop
language: python
severity: warning
message: "String concatenation in loop is O(n^2) - use join()"
rule:
  kind: for_statement
  has:
    pattern: "§STR = §STR + §OTHER"
```

## Framework-Specific Patterns

### React Patterns

```yaml
id: react-missing-key
language: javascript
severity: warning
message: "Missing key prop in list rendering"
rule:
  kind: jsx_element
  inside:
    pattern: "§ARRAY.map(§§§ARGS)"
  not:
    has:
      pattern: "key={§KEY}"
```

```yaml
id: react-direct-state-mutation
language: javascript
severity: error
message: "Don't mutate state directly - use setState"
rule:
  pattern: "this.state.§PROP = §VALUE"
```

### Django Patterns

```yaml
id: django-raw-sql
language: python
severity: warning
message: "Raw SQL - ensure proper parameterization"
rule:
  any:
    - pattern: "§MODEL.objects.raw(§§§ARGS)"
    - pattern: "connection.cursor().execute(§§§ARGS)"
```

### Express.js Patterns

```yaml
id: express-no-validation
language: javascript
severity: warning
message: "Direct use of req.body without validation"
rule:
  pattern: "req.body.§PROP"
  not:
    inside:
      pattern: "§VALIDATOR(req.body)"
```

## Language Migration Patterns

### Python 2 to 3

```yaml
id: python2-print
language: python
severity: error
message: "Use print() function (Python 3)"
rule:
  kind: print_statement  # Python 2 syntax
fix: "print(§§§ARGS)"
```

```yaml
id: python2-division
language: python
severity: warning
message: "Consider using // for integer division"
rule:
  pattern: "§A / §B"
  constraints:
    A:
      kind: integer
    B:
      kind: integer
```

### Callback to Async

```yaml
id: callback-to-async
language: javascript
severity: info
message: "Consider using async/await instead of callbacks"
rule:
  pattern: "§FUNC(§§§ARGS, function(§ERR, §RESULT) { §§§BODY })"
```

## Quick Reference

| Use Case | Key Pattern Element |
|----------|---------------------|
| Match any call | `§FUNC(§§§ARGS)` |
| Match with specific arg | `func(§ARG)` |
| Match property access | `§OBJ.§PROP` |
| Match assignment | `§VAR = §VALUE` |
| Match binary op | `§A + §B` |
| Match in loop | `inside: { kind: for_statement }` |
| Exclude pattern | `not: { pattern: "..." }` |
| Multiple options | `any: [...]` |
| All conditions | `all: [...]` |
| Regex constraint | `constraints: { VAR: { regex: "..." } }` |
