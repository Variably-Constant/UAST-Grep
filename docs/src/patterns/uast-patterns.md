# UAST Patterns

UAST (Universal Abstract Syntax Tree) patterns let you write one pattern that works across multiple programming languages.

## What is UAST?

Different languages represent the same concepts differently:

| Concept | Python | Rust | JavaScript |
|---------|--------|------|------------|
| Function | `function_definition` | `function_item` | `function_declaration` |
| If statement | `if_statement` | `if_expression` | `if_statement` |
| Class | `class_definition` | `impl_item` | `class_declaration` |

UAST provides a unified layer with PascalCase names:

| UAST Pattern | Matches In |
|--------------|------------|
| `FunctionDeclaration` | Functions in any language |
| `IfStatement` | If statements everywhere |
| `ClassDeclaration` | Classes in any language |

## How It Works

When you use a UAST pattern like `FunctionDeclaration`:

1. UAST-Grep looks up the mapping for your target language
2. Translates to the native tree-sitter type(s)
3. Searches for those native types

```bash
# This UAST pattern...
uast-grep run -p FunctionDeclaration -l python ./src

# ...becomes this native query:
uast-grep run -p function_definition -l python ./src
```

## Available UAST Types

### Declarations

| UAST Type | Description | Example Languages |
|-----------|-------------|-------------------|
| `FunctionDeclaration` | Function/method definitions | All |
| `ClassDeclaration` | Class definitions | Python, JS, TS, Java, C# |
| `VariableDeclaration` | Variable declarations | All |
| `ParameterDeclaration` | Function parameters | All |
| `ImportDeclaration` | Import/require/use | All |
| `ExportDeclaration` | Export statements | JS, TS |

### Statements

| UAST Type | Description | Example Languages |
|-----------|-------------|-------------------|
| `IfStatement` | If/else conditionals | All |
| `ForStatement` | For loops | All |
| `WhileStatement` | While loops | All |
| `ReturnStatement` | Return statements | All |
| `TryStatement` | Try/catch/except | All |
| `SwitchStatement` | Switch/match | JS, TS, Rust, C# |
| `BreakStatement` | Break | All |
| `ContinueStatement` | Continue | All |

### Expressions

| UAST Type | Description | Example Languages |
|-----------|-------------|-------------------|
| `CallExpression` | Function calls | All |
| `BinaryExpression` | Binary operators | All |
| `UnaryExpression` | Unary operators | All |
| `AssignmentExpression` | Assignment | All |
| `MemberExpression` | Property access | All |
| `IndexExpression` | Array/dict access | All |
| `LambdaExpression` | Lambda/arrow functions | Python, JS, TS, Rust |
| `ConditionalExpression` | Ternary operator | JS, TS, C, Python |

### Literals

| UAST Type | Description |
|-----------|-------------|
| `StringLiteral` | String values |
| `NumberLiteral` | Numeric values |
| `BooleanLiteral` | true/false |
| `NullLiteral` | null/None/nil |
| `ArrayLiteral` | Array/list literals |
| `ObjectLiteral` | Object/dict literals |

### Other

| UAST Type | Description |
|-----------|-------------|
| `Comment` | Comments |
| `Identifier` | Names/identifiers |
| `Block` | Code blocks |
| `Program` | Root/module node |

## Using UAST Patterns

### Basic Usage

```bash
# Find all functions
uast-grep run -p FunctionDeclaration -l python ./src
uast-grep run -p FunctionDeclaration -l rust ./src
uast-grep run -p FunctionDeclaration -l typescript ./src
```

### Force UAST Mode

If UAST-Grep interprets your pattern as native, force UAST mode:

```bash
uast-grep run -p FunctionDeclaration -l python ./src --uast
```

### Combine with Metavariables

```bash
# Find function calls
uast-grep run -p 'CallExpression' -l python ./src

# With captured arguments
uast-grep run -p '§FUNC(§§§ARGS)' -l python ./src
```

## Cross-Language Search

The power of UAST is writing patterns once:

```yaml
# security-rules.yaml - Works for Python, JS, and more
id: no-eval
message: "eval() is dangerous"
rule:
  any:
    - pattern: "eval(§§§ARGS)"
    - pattern: "exec(§§§ARGS)"
```

Run against multiple languages:

```bash
# Python
uast-grep scan -r security-rules.yaml -l python ./src

# JavaScript
uast-grep scan -r security-rules.yaml -l javascript ./src
```

## Language-Specific Mappings

### Python

| UAST | Native |
|------|--------|
| `FunctionDeclaration` | `function_definition` |
| `ClassDeclaration` | `class_definition` |
| `IfStatement` | `if_statement` |
| `ForStatement` | `for_statement` |
| `TryStatement` | `try_statement` |
| `CallExpression` | `call` |
| `Comment` | `comment` |

### JavaScript/TypeScript

| UAST | Native |
|------|--------|
| `FunctionDeclaration` | `function_declaration`, `arrow_function`, `method_definition` |
| `ClassDeclaration` | `class_declaration` |
| `IfStatement` | `if_statement` |
| `ForStatement` | `for_statement`, `for_in_statement` |
| `TryStatement` | `try_statement` |
| `CallExpression` | `call_expression` |
| `Comment` | `comment` |

### Rust

| UAST | Native |
|------|--------|
| `FunctionDeclaration` | `function_item` |
| `ClassDeclaration` | `impl_item`, `struct_item` |
| `IfStatement` | `if_expression` |
| `ForStatement` | `for_expression` |
| `MatchStatement` | `match_expression` |
| `CallExpression` | `call_expression` |
| `Comment` | `line_comment`, `block_comment` |

### PowerShell

| UAST | Native |
|------|--------|
| `FunctionDeclaration` | `function_definition` |
| `ClassDeclaration` | `class_definition` |
| `IfStatement` | `if_statement` |
| `ForStatement` | `for_statement`, `foreach_statement` |
| `TryStatement` | `try_statement` |
| `CallExpression` | `command_expression`, `command` |
| `Comment` | `comment` |

## Checking Mappings

To see what native types a UAST pattern maps to:

```bash
uast-grep run -p FunctionDeclaration -l python --verbose
# Output: [info] UAST 'FunctionDeclaration' -> native types: ["function_definition"]
```

## Limitations

1. **Not all types map to all languages** - Some concepts don't exist everywhere
2. **Semantic differences** - A "function" in Python vs Rust has different semantics
3. **Multiple mappings** - One UAST type might map to several native types

When you need precision, use [Native Patterns](native-patterns.md) instead.

## Best Practices

1. **Start with UAST** - Simpler and more portable
2. **Fall back to native** - When you need language-specific precision
3. **Test across languages** - Make sure your pattern works everywhere you expect
4. **Use verbose mode** - To see the actual native types being matched

## Examples

### Find All Functions Across Languages

```bash
for lang in python rust javascript typescript go; do
    echo "=== $lang ==="
    uast-grep run -p FunctionDeclaration -l $lang ./src -c
done
```

### Universal Security Rules

```yaml
id: dangerous-functions
message: "Potentially dangerous function call"
rule:
  any:
    - pattern: "eval(§§§ARGS)"
    - pattern: "exec(§§§ARGS)"
    - pattern: "system(§§§ARGS)"
    - pattern: "shell_exec(§§§ARGS)"
    - pattern: "os.system(§§§ARGS)"
    - pattern: "subprocess.call(§§§ARGS, shell=True)"
```
