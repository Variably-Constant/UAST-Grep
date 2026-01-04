# PowerShell

PowerShell is one of UAST-Grep's core languages, with comprehensive support for all AST node types.

## File Extensions

- `.ps1` - PowerShell scripts
- `.psm1` - PowerShell modules
- `.psd1` - PowerShell data files

## Basic Usage

```bash
# Find all functions
uast-grep run -p function_definition -l powershell ./scripts

# Find all cmdlet calls
uast-grep run -p command -l powershell ./scripts

# Find all try/catch blocks
uast-grep run -p try_statement -l powershell ./scripts
```

## Common Node Types

### Functions and Scripts

| Node Type | Description | Example |
|-----------|-------------|---------|
| `function_definition` | Function declaration | `function Get-Item {}` |
| `param_block` | Param block | `param([string]$Name)` |
| `script_block` | Script block | `{ Get-Item }` |
| `script_block_expression` | Script block expr | `$sb = { }` |

### Commands and Pipelines

| Node Type | Description | Example |
|-----------|-------------|---------|
| `command` | Command invocation | `Get-Item` |
| `command_expression` | Command expression | `Get-Item -Path $x` |
| `pipeline` | Pipeline | `Get-Item \| Select` |
| `command_name` | Command name | `Get-Item` |
| `command_parameter` | Parameter | `-Path` |
| `command_argument` | Argument | `$path` |

### Control Flow

| Node Type | Description | Example |
|-----------|-------------|---------|
| `if_statement` | If/elseif/else | `if ($x) { }` |
| `switch_statement` | Switch | `switch ($x) { }` |
| `foreach_statement` | ForEach | `foreach ($x in $y) { }` |
| `for_statement` | For loop | `for ($i=0; $i -lt 10; $i++) { }` |
| `while_statement` | While loop | `while ($x) { }` |
| `do_while_statement` | Do-While | `do { } while ($x)` |
| `do_until_statement` | Do-Until | `do { } until ($x)` |
| `try_statement` | Try/Catch/Finally | `try { } catch { }` |
| `trap_statement` | Trap | `trap { }` |

### Variables and Expressions

| Node Type | Description | Example |
|-----------|-------------|---------|
| `variable` | Variable | `$foo` |
| `assignment_expression` | Assignment | `$x = 1` |
| `binary_expression` | Binary op | `$x + $y` |
| `unary_expression` | Unary op | `-not $x` |
| `comparison_operator` | Comparison | `-eq`, `-ne`, `-lt` |
| `logical_operator` | Logical | `-and`, `-or` |
| `member_access` | Member access | `$obj.Property` |
| `index_expression` | Index access | `$arr[0]` |
| `invoke_expression` | Method call | `$obj.Method()` |

### Literals and Data

| Node Type | Description | Example |
|-----------|-------------|---------|
| `string_literal` | Single-quoted | `'hello'` |
| `string_expandable` | Double-quoted | `"hello $name"` |
| `here_string_literal` | Here-string | `@'...'@` |
| `here_string_expandable` | Expandable here-string | `@"..."@` |
| `integer_literal` | Integer | `42` |
| `real_literal` | Float | `3.14` |
| `hash_literal_expression` | Hashtable | `@{a=1}` |
| `array_expression` | Array | `@(1,2,3)` |
| `array_literal` | Array literal | `1,2,3` |

### Classes and Types

| Node Type | Description | Example |
|-----------|-------------|---------|
| `class_definition` | Class | `class Foo {}` |
| `enum_definition` | Enum | `enum Color {}` |
| `type_literal` | Type | `[string]` |
| `attribute` | Attribute | `[Parameter()]` |
| `attribute_argument` | Attribute arg | `Mandatory=$true` |

### Other

| Node Type | Description | Example |
|-----------|-------------|---------|
| `comment` | Comment | `# comment` |
| `requires_statement` | Requires | `#Requires -Version 5.1` |
| `using_statement` | Using | `using namespace System` |
| `data_statement` | Data | `data { }` |

## Security Patterns

### Dangerous Commands

```yaml
id: dangerous-invoke-expression
language: powershell
severity: error
message: "Invoke-Expression can execute arbitrary code"
rule:
  any:
    - pattern: "Invoke-Expression §§§ARGS"
    - pattern: "iex §§§ARGS"
```

### Script Block Injection

```yaml
id: scriptblock-injection
language: powershell
severity: error
message: "Dynamic script block creation is dangerous"
rule:
  any:
    - pattern: "[scriptblock]::Create(§INPUT)"
    - pattern: "& §DYNAMIC"
    - pattern: ". §DYNAMIC"
```

### Credential Handling

```yaml
id: plaintext-credentials
language: powershell
severity: error
message: "Don't store credentials in plaintext"
rule:
  any:
    - pattern: "$§VAR = ConvertTo-SecureString §STRING -AsPlainText"
    - pattern: 'New-Object PSCredential(§USER, (ConvertTo-SecureString §PASS -AsPlainText -Force))'
```

### Bypassing Execution Policy

```yaml
id: execution-policy-bypass
language: powershell
severity: warning
message: "Execution policy bypass detected"
rule:
  any:
    - pattern: "Set-ExecutionPolicy Bypass"
    - pattern: "Set-ExecutionPolicy Unrestricted"
    - pattern: "-ExecutionPolicy Bypass"
```

## Code Quality Patterns

### Empty Catch Blocks

```yaml
id: empty-catch
language: powershell
severity: warning
message: "Empty catch block hides errors"
rule:
  pattern: |
    catch {
    }
```

### Write-Host Overuse

```yaml
id: avoid-write-host
language: powershell
severity: info
message: "Prefer Write-Output or Write-Verbose over Write-Host"
rule:
  pattern: "Write-Host §§§ARGS"
```

### Missing CmdletBinding

```yaml
id: missing-cmdletbinding
language: powershell
severity: info
message: "Consider adding [CmdletBinding()] for advanced function features"
rule:
  kind: function_definition
  not:
    has:
      kind: param_block
      has:
        pattern: "[CmdletBinding(§§§ARGS)]"
```

### Missing Error Handling

```yaml
id: no-error-handling
language: powershell
severity: warning
message: "Function should have error handling"
rule:
  kind: function_definition
  not:
    has:
      kind: try_statement
```

## Best Practices

### Finding Non-Standard Aliases

```yaml
id: avoid-aliases
language: powershell
severity: info
message: "Use full cmdlet names instead of aliases for clarity"
rule:
  any:
    - pattern: "gci §§§ARGS"
    - pattern: "gc §§§ARGS"
    - pattern: "% { §§§BODY }"
    - pattern: "? { §§§BODY }"
    - pattern: "select §§§ARGS"
    - pattern: "where §§§ARGS"
```

### Finding Backticks (Line Continuation)

```yaml
id: avoid-backticks
language: powershell
severity: info
message: "Consider using splatting instead of backticks"
rule:
  kind: line_continuation_token
```

## Example Searches

### Find All Functions

```bash
uast-grep run -p function_definition -l powershell ./Modules
```

### Find Cmdlet Calls with Specific Parameters

```bash
# Find Get-ChildItem with -Recurse
uast-grep run -p 'Get-ChildItem §§§ARGS -Recurse §§§REST' -l powershell ./scripts
```

### Find Pipeline Operations

```bash
# Find Where-Object in pipelines
uast-grep run -p 'Where-Object { §§§BODY }' -l powershell ./scripts
uast-grep run -p '? { §§§BODY }' -l powershell ./scripts
```

### Find Variable Assignments

```bash
uast-grep run -p assignment_expression -l powershell ./scripts
```

### Find Try/Catch Blocks

```bash
uast-grep run -p try_statement -l powershell ./scripts
```

### Find Class Definitions

```bash
uast-grep run -p class_definition -l powershell ./Modules
```

## PowerShell-Specific Tips

1. **Command vs Expression Mode:** PowerShell has two parsing modes. UAST-Grep handles both.

2. **Splatting:** Splatted arguments (`@params`) are captured as variables.

3. **Here-Strings:** Both `@'...'@` and `@"..."@` are supported.

4. **Comments:** Both `#` line comments and `<# #>` block comments are recognized.

5. **Pipeline Parsing:** Each stage of a pipeline is a separate AST node.
