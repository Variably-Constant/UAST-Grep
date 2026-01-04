# C#

C# is a built-in language with comprehensive support for all modern C# features.

## File Extensions

- `.cs` - C# source files

## Basic Usage

```bash
# Find all classes
uast-grep run -p class_declaration -l csharp ./src

# Find all methods
uast-grep run -p method_declaration -l csharp ./src

# Find all async methods
uast-grep run -p 'async §TYPE §NAME(§§§PARAMS)' -l csharp ./src
```

## Common Node Types

### Types and Declarations

| Node Type | Description | Example |
|-----------|-------------|---------|
| `class_declaration` | Class | `class Foo {}` |
| `struct_declaration` | Struct | `struct Bar {}` |
| `interface_declaration` | Interface | `interface IFoo {}` |
| `enum_declaration` | Enum | `enum Color {}` |
| `record_declaration` | Record | `record Person {}` |
| `record_struct_declaration` | Record struct | `record struct Point {}` |
| `delegate_declaration` | Delegate | `delegate void Handler()` |

### Members

| Node Type | Description | Example |
|-----------|-------------|---------|
| `method_declaration` | Method | `void Foo() {}` |
| `constructor_declaration` | Constructor | `Foo() {}` |
| `destructor_declaration` | Destructor | `~Foo() {}` |
| `property_declaration` | Property | `int Prop { get; set; }` |
| `field_declaration` | Field | `private int _field;` |
| `event_declaration` | Event | `event Handler OnChange;` |
| `indexer_declaration` | Indexer | `this[int i] { get; }` |
| `operator_declaration` | Operator | `operator +(...)` |

### Statements

| Node Type | Description | Example |
|-----------|-------------|---------|
| `if_statement` | If/else | `if (x) { }` |
| `switch_statement` | Switch | `switch (x) { }` |
| `switch_expression` | Switch expr | `x switch { }` |
| `for_statement` | For | `for (;;) { }` |
| `foreach_statement` | ForEach | `foreach (var x in y) { }` |
| `while_statement` | While | `while (x) { }` |
| `do_statement` | Do-While | `do { } while (x);` |
| `try_statement` | Try/Catch | `try { } catch { }` |
| `using_statement` | Using | `using (var x = ...) { }` |
| `lock_statement` | Lock | `lock (obj) { }` |
| `return_statement` | Return | `return x;` |
| `throw_statement` | Throw | `throw ex;` |

### Expressions

| Node Type | Description | Example |
|-----------|-------------|---------|
| `invocation_expression` | Method call | `Foo()` |
| `object_creation_expression` | New | `new Foo()` |
| `member_access_expression` | Member access | `obj.Prop` |
| `element_access_expression` | Indexer | `arr[0]` |
| `binary_expression` | Binary op | `x + y` |
| `assignment_expression` | Assignment | `x = y` |
| `conditional_expression` | Ternary | `x ? y : z` |
| `lambda_expression` | Lambda | `x => x + 1` |
| `anonymous_method_expression` | Anonymous | `delegate { }` |
| `query_expression` | LINQ | `from x in y select z` |
| `await_expression` | Await | `await task` |
| `throw_expression` | Throw expr | `x ?? throw new Ex()` |

### Patterns (C# 8+)

| Node Type | Description | Example |
|-----------|-------------|---------|
| `type_pattern` | Type pattern | `is string` |
| `constant_pattern` | Const pattern | `is 42` |
| `relational_pattern` | Relational | `is > 10` |
| `and_pattern` | And | `is > 0 and < 100` |
| `or_pattern` | Or | `is null or ""` |
| `not_pattern` | Not | `is not null` |
| `property_pattern` | Property | `is { Length: > 0 }` |
| `positional_pattern` | Positional | `is (x, y)` |
| `list_pattern` | List (C# 11) | `is [a, b, ..]` |

### Other

| Node Type | Description | Example |
|-----------|-------------|---------|
| `using_directive` | Using | `using System;` |
| `namespace_declaration` | Namespace | `namespace Foo { }` |
| `file_scoped_namespace_declaration` | File namespace | `namespace Foo;` |
| `attribute_list` | Attributes | `[Attr]` |
| `parameter` | Parameter | `int x` |
| `type_parameter` | Generic param | `<T>` |
| `local_declaration_statement` | Local var | `var x = 1;` |
| `comment` | Comment | `// comment` |
| `xml_comment` | XML doc | `/// <summary>` |

## Security Patterns

### SQL Injection

```yaml
id: sql-injection-csharp
language: csharp
severity: error
message: "Possible SQL injection - use parameterized queries"
rule:
  any:
    - pattern: 'cmd.CommandText = §QUERY + §INPUT'
    - pattern: '$"SELECT §§§ {§INPUT} §§§"'
    - pattern: 'string.Format("SELECT §§§", §INPUT)'
```

### Path Traversal

```yaml
id: path-traversal
language: csharp
severity: error
message: "Possible path traversal vulnerability"
rule:
  any:
    - pattern: "File.ReadAllText(§INPUT)"
    - pattern: "File.Open(§INPUT, §§§REST)"
    - pattern: "new FileStream(§INPUT, §§§REST)"
  not:
    has:
      pattern: "Path.Combine(§§§SAFE, §INPUT)"
```

### Insecure Deserialization

```yaml
id: insecure-deserialization
language: csharp
severity: error
message: "BinaryFormatter is insecure - use safer alternatives"
rule:
  any:
    - pattern: "new BinaryFormatter()"
    - pattern: "BinaryFormatter.Deserialize(§§§ARGS)"
```

### Weak Cryptography

```yaml
id: weak-crypto
language: csharp
severity: error
message: "MD5/SHA1 are cryptographically weak"
rule:
  any:
    - pattern: "MD5.Create()"
    - pattern: "SHA1.Create()"
    - pattern: "new MD5CryptoServiceProvider()"
    - pattern: "new SHA1CryptoServiceProvider()"
```

## Code Quality Patterns

### Empty Catch

```yaml
id: empty-catch-csharp
language: csharp
severity: warning
message: "Empty catch block hides exceptions"
rule:
  kind: catch_clause
  has:
    kind: block
    not:
      has:
        kind: _  # No children = empty
```

### Catch-All Without Logging

```yaml
id: catch-all-no-log
language: csharp
severity: warning
message: "Catching Exception without logging"
rule:
  pattern: "catch (Exception §EX) { §§§BODY }"
  not:
    has:
      any:
        - pattern: "§LOGGER.§METHOD(§§§ARGS)"
        - pattern: "Console.§METHOD(§§§ARGS)"
```

### Async Void

```yaml
id: async-void
language: csharp
severity: warning
message: "Avoid async void - use async Task instead"
rule:
  kind: method_declaration
  has:
    kind: modifier
    pattern: "async"
  has:
    kind: predefined_type
    pattern: "void"
```

### Missing Await

```yaml
id: missing-await
language: csharp
severity: warning
message: "Task not awaited - possible fire-and-forget"
rule:
  kind: expression_statement
  has:
    kind: invocation_expression
  not:
    has:
      kind: await_expression
```

## Best Practices

### Using Declaration (C# 8+)

```yaml
id: old-using-statement
language: csharp
severity: info
message: "Consider using declaration instead of using statement"
rule:
  kind: using_statement
  has:
    kind: block
```

### Pattern Matching Opportunities

```yaml
id: is-null-check
language: csharp
severity: info
message: "Consider using 'is null' pattern"
rule:
  pattern: "§X == null"
```

### Record Types

```yaml
id: mutable-dto
language: csharp
severity: info
message: "Consider using record for immutable DTOs"
rule:
  kind: class_declaration
  has:
    pattern: "§TYPE §PROP { get; set; }"
  has:
    kind: attribute_list
    pattern: "[Serializable]"
```

## Example Searches

### Find All Async Methods

```bash
uast-grep run -p 'async §TYPE §NAME(§§§PARAMS)' -l csharp ./src
```

### Find LINQ Queries

```bash
uast-grep run -p query_expression -l csharp ./src
```

### Find All Interfaces

```bash
uast-grep run -p interface_declaration -l csharp ./src
```

### Find Nullable Reference Types

```bash
uast-grep run -p nullable_type -l csharp ./src
```

### Find All Attributes

```bash
uast-grep run -p attribute_list -l csharp ./src
```

### Find Switch Expressions

```bash
uast-grep run -p switch_expression -l csharp ./src
```

### Find Extension Methods

```bash
uast-grep ts-query '(method_declaration (parameter_list (parameter (modifier) @m (#eq? @m "this"))))' -l csharp ./src
```

### Find IDisposable Implementations

```bash
uast-grep run -p 'class §NAME : §§§BASES, IDisposable' -l csharp ./src
```

## C#-Specific Tips

1. **Namespaces:** File-scoped namespaces (`namespace Foo;`) are a separate node type from block namespaces.

2. **Records:** Records are parsed as `record_declaration`, not `class_declaration`.

3. **Pattern Matching:** C# 8+ patterns have dedicated node types for each pattern kind.

4. **Nullable Types:** `string?` has a `nullable_type` wrapper around the base type.

5. **Primary Constructors:** In C# 12, primary constructor parameters are part of the class declaration.

6. **Top-Level Statements:** In C# 9+, top-level statements appear directly in the compilation unit.
