# Native Patterns

Native patterns use tree-sitter's exact node types for precise, language-specific matching.

## When to Use Native Patterns

Use native patterns when you need:
- **Precision** - Match exactly one language construct
- **Performance** - Skip UAST translation layer
- **Access to all node types** - Some nodes don't have UAST mappings
- **S-expression queries** - Advanced tree-sitter query syntax

## Basic Native Patterns

Native node types use `snake_case`:

```bash
# Python function
uast-grep run -p function_definition -l python ./src

# Rust function
uast-grep run -p function_item -l rust ./src

# JavaScript function
uast-grep run -p function_declaration -l javascript ./src
```

## Discovering Node Types

Use the `parse` command to explore:

```bash
# Parse a file and see all node types
uast-grep parse example.py -l python
```

Output:
```
module [0:0-15:0]
  function_definition [0:0-5:10]
    name: identifier [0:4-0:8] "main"
    parameters: parameters [0:8-0:10]
    body: block [1:4-5:10]
      if_statement [1:4-4:15]
        condition: comparison_operator [1:7-1:13]
        consequence: block [2:8-2:20]
        alternative: else_clause [3:4-4:15]
```

## S-Expression Queries

For complex matching, use tree-sitter's S-expression syntax with `ts-query`:

```bash
# Find functions with specific name patterns
uast-grep ts-query "(function_definition name: (identifier) @name)" -l python ./src

# Find if statements with specific conditions
uast-grep ts-query "(if_statement condition: (_) @cond)" -l python ./src

# Find string literals containing "password"
uast-grep ts-query '(string) @s (#match? @s "password")' -l python ./src
```

### S-Expression Syntax

```
(node_type                    ; Match node type
  field_name: (child_type)    ; Match child by field
  (another_child)             ; Match anonymous child
  @capture_name               ; Capture the node
)
```

### Predicates

```
(#eq? @name "value")      ; Exact match
(#match? @name "regex")   ; Regex match
(#not-eq? @a @b)          ; Not equal
```

### Examples

```bash
# Functions named "main"
uast-grep ts-query '(function_definition name: (identifier) @name (#eq? @name "main"))' -l python ./src

# Import statements for "os"
uast-grep ts-query '(import_statement (dotted_name) @mod (#eq? @mod "os"))' -l python ./src

# All string literals
uast-grep ts-query '(string) @s' -l python ./src
```

## Common Native Types by Language

### Python

| Type | Description | Example |
|------|-------------|---------|
| `function_definition` | Function def | `def foo():` |
| `class_definition` | Class def | `class Foo:` |
| `if_statement` | If block | `if x:` |
| `for_statement` | For loop | `for x in y:` |
| `while_statement` | While loop | `while x:` |
| `try_statement` | Try/except | `try:` |
| `with_statement` | Context manager | `with open():` |
| `import_statement` | Import | `import os` |
| `import_from_statement` | From import | `from os import path` |
| `call` | Function call | `foo()` |
| `assignment` | Assignment | `x = 1` |
| `augmented_assignment` | Aug assign | `x += 1` |
| `comparison_operator` | Comparison | `x == y` |
| `boolean_operator` | Bool ops | `x and y` |
| `string` | String literal | `"hello"` |
| `integer` | Integer | `42` |
| `list` | List literal | `[1, 2]` |
| `dictionary` | Dict literal | `{"a": 1}` |
| `comment` | Comment | `# note` |
| `decorator` | Decorator | `@property` |

### JavaScript/TypeScript

| Type | Description | Example |
|------|-------------|---------|
| `function_declaration` | Function | `function foo()` |
| `arrow_function` | Arrow func | `() => {}` |
| `class_declaration` | Class | `class Foo {}` |
| `if_statement` | If | `if (x) {}` |
| `for_statement` | For | `for (;;) {}` |
| `for_in_statement` | For-in | `for (x in y)` |
| `for_of_statement` | For-of | `for (x of y)` |
| `while_statement` | While | `while (x) {}` |
| `try_statement` | Try/catch | `try {} catch {}` |
| `call_expression` | Call | `foo()` |
| `assignment_expression` | Assignment | `x = 1` |
| `binary_expression` | Binary op | `x + y` |
| `template_string` | Template | `` `hello` `` |
| `string` | String | `"hello"` |
| `number` | Number | `42` |
| `array` | Array | `[1, 2]` |
| `object` | Object | `{a: 1}` |
| `comment` | Comment | `// note` |
| `jsx_element` | JSX | `<div/>` |
| `import_statement` | Import | `import x` |
| `export_statement` | Export | `export x` |

### Rust

| Type | Description | Example |
|------|-------------|---------|
| `function_item` | Function | `fn foo()` |
| `impl_item` | Impl block | `impl Foo {}` |
| `struct_item` | Struct | `struct Foo {}` |
| `enum_item` | Enum | `enum Foo {}` |
| `trait_item` | Trait | `trait Foo {}` |
| `if_expression` | If | `if x {}` |
| `match_expression` | Match | `match x {}` |
| `for_expression` | For | `for x in y {}` |
| `while_expression` | While | `while x {}` |
| `loop_expression` | Loop | `loop {}` |
| `call_expression` | Call | `foo()` |
| `macro_invocation` | Macro | `println!()` |
| `let_declaration` | Let | `let x = 1;` |
| `assignment_expression` | Assign | `x = 1` |
| `binary_expression` | Binary | `x + y` |
| `string_literal` | String | `"hello"` |
| `raw_string_literal` | Raw string | `r#"hello"#` |
| `integer_literal` | Integer | `42` |
| `line_comment` | Comment | `// note` |
| `block_comment` | Block comment | `/* note */` |
| `attribute_item` | Attribute | `#[derive()]` |
| `use_declaration` | Use | `use std::io;` |

### PowerShell

| Type | Description | Example |
|------|-------------|---------|
| `function_definition` | Function | `function Foo {}` |
| `class_definition` | Class | `class Foo {}` |
| `if_statement` | If | `if ($x) {}` |
| `foreach_statement` | ForEach | `foreach ($x in $y) {}` |
| `for_statement` | For | `for ($i=0;;) {}` |
| `while_statement` | While | `while ($x) {}` |
| `try_statement` | Try | `try {} catch {}` |
| `switch_statement` | Switch | `switch ($x) {}` |
| `command_expression` | Cmdlet call | `Get-Item` |
| `command` | Command | `dir` |
| `pipeline` | Pipeline | `Get-Item \| Select` |
| `assignment_expression` | Assignment | `$x = 1` |
| `variable` | Variable | `$foo` |
| `string_expandable` | Expandable | `"hello $x"` |
| `string_literal` | Literal | `'hello'` |
| `hash_literal_expression` | Hashtable | `@{a=1}` |
| `array_expression` | Array | `@(1,2)` |
| `comment` | Comment | `# note` |
| `param_block` | Param | `param()` |
| `attribute` | Attribute | `[Parameter()]` |

## Combining Native with Metavariables

Native patterns work with metavariables:

```bash
# Match any function definition with captured name
uast-grep run -p 'function_definition' -l python ./src
```

In YAML rules:

```yaml
rule:
  kind: function_definition
  has:
    pattern: "§NAME"
```

## Mixed Mode

Use native node kinds in YAML rules:

```yaml
rule:
  all:
    - kind: function_definition  # Native type
    - pattern: "def §NAME(§§§PARAMS):"  # Pattern with metavars
    - has:
        kind: decorator
        pattern: "@property"
```

## Performance Tips

1. **Native patterns are faster** - No UAST translation
2. **Use specific types** - `function_definition` vs general `statement`
3. **Avoid deep nesting** - Flat queries are faster
4. **Use S-expressions for complex queries** - More efficient than pattern matching

## Debugging

```bash
# See the AST structure
uast-grep parse file.py -l python

# Verbose output shows native types
uast-grep run -p FunctionDeclaration -l python ./src --verbose

# Debug your S-expression query
uast-grep ts-query "(function_definition) @f" -l python ./src --verbose
```
