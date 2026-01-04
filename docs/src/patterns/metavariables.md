# Metavariables

Metavariables are placeholders in patterns that capture and match AST nodes. They're the key to writing powerful, flexible search patterns.

## Prefix Characters

UAST-Grep supports three prefix characters for metavariables:

| Prefix | Unicode | Name | Example |
|--------|---------|------|---------|
| `§` | U+00A7 | Section | `§NAME` |
| `∀` | U+2200 | For All | `∀NAME` |
| `$` | U+0024 | Dollar | `$NAME` |

### Which Should I Use?

**Recommended: `§` (Section)**

The section symbol is the best choice because:
- Clearly looks like a special symbol (not a letter)
- No escaping needed in any shell
- Easy to type on most keyboards (Alt+0167 on Windows, Option+6 on Mac)

**Alternative: `∀` (For All)**

The mathematical "for all" symbol works well:
- Semantic meaning: "matches any node"
- No escaping needed
- Harder to type (copy/paste or Alt codes)

**PowerShell Users: `$` with Single Quotes**

If you prefer the traditional dollar sign:
```powershell
# Must use single quotes to prevent variable expansion!
uast-grep run -p '$NAME' -l python ./src

# NOT this - PowerShell will expand $NAME
uast-grep run -p "$NAME" -l python ./src  # WRONG!
```

## Quantifiers

Quantifiers control how many AST nodes a metavariable matches:

| Pattern | Name | Matches |
|---------|------|---------|
| `§NAME` | Single | Exactly one node |
| `§§NAME` | One or More | 1, 2, 3, ... nodes |
| `§§§NAME` | Zero or More | 0, 1, 2, 3, ... nodes |

### Single Metavariable (`§NAME`)

Matches exactly one AST node:

```bash
# Match: print(x), print("hello")
# No match: print(), print(a, b)
uast-grep run -p 'print(§ARG)' -l python ./src
```

### One or More (`§§NAME`)

Matches one or more nodes. Useful for non-empty lists:

```bash
# Match: print(a), print(a, b), print(a, b, c)
# No match: print()
uast-grep run -p 'print(§§ARGS)' -l python ./src
```

### Zero or More (`§§§NAME`)

Matches zero or more nodes. Most flexible:

```bash
# Match: print(), print(a), print(a, b)
uast-grep run -p 'print(§§§ARGS)' -l python ./src
```

## Naming Rules

Metavariable names must:
- Start with a letter (uppercase recommended)
- Contain only letters, digits, and underscores
- Be UPPERCASE by convention (for clarity)

Valid names:
```
§NAME
§VAR1
§FIRST_ARG
§A
```

Invalid names:
```
§123       # Can't start with digit
§name      # Lowercase (allowed but not recommended)
§VAR-NAME  # No hyphens
§          # Empty name
```

## Special Metavariables

### Anonymous Metavariable (`§_`)

Matches any single node without capturing:

```bash
# Match function calls with exactly 2 arguments (don't care what they are)
uast-grep run -p 'func(§_, §_)' -l python ./src
```

### Wildcard (`*`)

Alternative to `§_` for matching any single node:

```bash
uast-grep run -p 'func(*, *)' -l python ./src
```

## Backreferences

When you use the same metavariable name twice, both must match identical nodes:

```bash
# Find x == x (always true)
uast-grep run -p '§A == §A' -l python ./src
# Matches: x == x, foo == foo
# No match: x == y

# Find duplicate arguments
uast-grep run -p 'func(§X, §X)' -l python ./src
# Matches: func(a, a)
# No match: func(a, b)
```

## Combining with Patterns

### In Structural Patterns

```yaml
rule:
  pattern: |
    if §COND:
        §BODY
    else:
        §BODY  # Same body in both branches!
```

This finds if/else where both branches have identical code.

### In YAML Rules

```yaml
rule:
  pattern: "§FUNC(§§§ARGS)"
  constraints:
    FUNC:
      regex: "^(eval|exec)$"  # Only match eval() or exec()
```

## Real-World Examples

### Find Repeated Conditions

```yaml
# Find: if x and x, if x or x
rule:
  any:
    - pattern: "§X and §X"
    - pattern: "§X or §X"
```

### Find Useless Comparisons

```yaml
# Find: x == x, x != x, x < x
rule:
  any:
    - pattern: "§X == §X"
    - pattern: "§X != §X"
    - pattern: "§X < §X"
    - pattern: "§X > §X"
```

### Find Assignment to Self

```yaml
# Find: x = x
rule:
  pattern: "§X = §X"
```

### Find Empty Catch/Except

```yaml
# Python
rule:
  pattern: |
    except §§§EXCEPTION:
        pass
```

### Find Unused Variables

```yaml
# Variable assigned but never used
rule:
  pattern: "§VAR = §VALUE"
  not:
    has:
      pattern: "§VAR"  # Check if VAR appears elsewhere
```

## Escaping Special Characters

If you need a literal `§`, `∀`, or `$` in your pattern:

```bash
# Use the opposite prefix
uast-grep run -p '§PRICE' -l python ./src  # Metavariable
uast-grep run -p '$PRICE' -l python ./src  # Could be literal or metavar depending on shell
```

For truly literal matches in YAML rules, use regex constraints or string literals.

## Best Practices

1. **Use UPPERCASE names** - Makes metavariables stand out
2. **Choose descriptive names** - `§FUNCTION` not `§X`
3. **Use `§§§` for optional lists** - Function arguments, array elements
4. **Use backreferences for duplication** - Same name = same value
5. **Use `§_` for don't-care positions** - When you need to match structure but not capture

## Debugging Patterns

Not getting the matches you expect?

```bash
# See what nodes your pattern could match
uast-grep parse file.py -l python

# Force UAST interpretation
uast-grep run -p FunctionDeclaration -l python ./src --uast

# See verbose output
uast-grep run -p '§FUNC(§§§ARGS)' -l python ./src --verbose
```
