# Your First Pattern

This tutorial walks you through creating your first UAST-Grep pattern, step by step.

## The Problem

Let's say you want to find all places in a Python codebase where someone calls `print()`. This is useful for:
- Finding debug statements before release
- Enforcing logging standards
- Code cleanup

## Step 1: Understand the AST

First, let's see how Python represents a `print()` call:

```bash
echo 'print("Hello, World!")' | uast-grep parse -l python --stdin
```

Output:
```
module [0:0-0:23]
  expression_statement [0:0-0:23]
    call [0:0-0:23]
      function: identifier [0:0-0:5] "print"
      arguments: argument_list [0:5-0:23]
        string [0:6-0:22] "Hello, World!"
```

We can see that a `print()` call is represented as a `call` node with a `function` field containing an `identifier`.

## Step 2: Simple Kind Match

The easiest pattern matches by node type:

```bash
# Find all function calls (any function)
uast-grep run -p CallExpression -l python ./src

# Or using native pattern
uast-grep run -p call -l python ./src
```

But this matches *all* function calls, not just `print`.

## Step 3: Using Metavariables

To be more specific, we need to capture and constrain the function name. UAST-Grep uses **metavariables** for this:

```bash
# Match any call, capture the function name
uast-grep run -p 'print(§§§ARGS)' -l python ./src
```

Here:
- `print` - Literal text to match
- `§§§ARGS` - Metavariable capturing zero or more arguments

## Step 4: Understanding Metavariable Syntax

UAST-Grep supports three prefix characters:

| Prefix | Example | Best For |
|--------|---------|----------|
| `§` | `§NAME` | **Recommended** - No escaping needed |
| `∀` | `∀NAME` | Mathematical style |
| `$` | `$NAME` | Standard - Use single quotes in PowerShell |

And three quantifiers:

| Pattern | Meaning | Matches |
|---------|---------|---------|
| `§NAME` | Single | Exactly one node |
| `§§NAME` | One or more | 1+ nodes |
| `§§§NAME` | Zero or more | 0+ nodes |

## Step 5: Build a YAML Rule

For reusable patterns, create a YAML rule:

```yaml
# rules/no-print.yaml
id: no-print-statements
language: python
severity: warning
message: "Avoid print() - use logging instead"

rule:
  pattern: "print(§§§ARGS)"

fix: "logger.info(§§§ARGS)"
```

Run it:

```bash
uast-grep scan -r rules/no-print.yaml ./src
```

## Step 6: Add Exceptions

Maybe `print()` is OK in test files. Add constraints:

```yaml
id: no-print-statements
language: python
severity: warning
message: "Avoid print() in production code"

rule:
  pattern: "print(§§§ARGS)"

# Only match in non-test files (handled by file filtering)
```

Or use `not` patterns:

```yaml
id: no-print-statements
language: python
severity: warning
message: "Avoid print() in production code"

rule:
  all:
    - pattern: "print(§§§ARGS)"
    - not:
        inside:
          kind: function_definition
          has:
            pattern: "test_§NAME"  # Skip test functions
```

## Step 7: Match Related Patterns

Extend to catch all print-like patterns:

```yaml
id: no-debug-output
language: python
severity: warning
message: "Remove debug output before committing"

rule:
  any:
    - pattern: "print(§§§ARGS)"
    - pattern: "pprint(§§§ARGS)"
    - pattern: "pp(§§§ARGS)"
    - pattern: "breakpoint()"
```

## Complete Example

Here's a full security rule that finds dangerous `eval()` calls:

```yaml
# rules/security/no-eval.yaml
id: dangerous-eval
language: python
severity: error
message: "eval() with user input is a code injection vulnerability"
tags:
  - security
  - injection
  - CWE-94

rule:
  pattern: "eval(§INPUT)"

constraints:
  INPUT:
    not:
      kind: string  # Literal strings are safer (though still not ideal)

fix: "# TODO: Replace eval() with ast.literal_eval() or safer alternative"

examples:
  match:
    - "eval(user_input)"
    - "eval(request.data)"
  no-match:
    - "eval('1 + 1')"  # Literal string (less dangerous)
```

## Practice Exercises

Try creating patterns for these scenarios:

1. **Find empty except blocks in Python**
   ```python
   try:
       risky()
   except:
       pass  # Find these!
   ```

2. **Find console.log in JavaScript**
   ```javascript
   console.log("debug");  // Find these!
   ```

3. **Find SQL string concatenation**
   ```python
   query = "SELECT * FROM users WHERE id = " + user_id  # Find these!
   ```

## Solutions

<details>
<summary>Click to reveal solutions</summary>

**1. Empty except blocks:**
```yaml
rule:
  pattern: |
    except:
        pass
```

**2. Console.log:**
```yaml
rule:
  pattern: "console.log(§§§ARGS)"
```

**3. SQL concatenation:**
```yaml
rule:
  all:
    - pattern: "§A + §B"
    - has:
        regex: "SELECT|INSERT|UPDATE|DELETE"
```

</details>

## Next Steps

- Learn more about [Metavariables](../patterns/metavariables.md)
- Explore [UAST Patterns](../patterns/uast-patterns.md) for cross-language search
- See the [Pattern Cookbook](../patterns/cookbook.md) for ready-to-use examples
