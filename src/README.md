# UAST-Grep Source Code

C# projects for the UAST-Grep unified AST search tool.

## Projects

### UAST.Core
Core library with schema, matching, and rules.
- `Schema/` - UAST node types (FunctionDeclaration, CallExpression, etc.)
- `Matching/` - Pattern parsing, matching engine, native tree-sitter integration
- `Rules/` - YAML rule parsing and validation
- `Transform/` - Code rewriting and output formats (JSON, SARIF)
- `Caching/` - AST cache for repeated queries
- `Interfaces/` - ILanguageMapper, IPatternMatcher

### UAST.Cli
Command-line interface using System.CommandLine and Spectre.Console.
- `run` - Search with UAST patterns
- `scan` - Apply YAML rules with optional auto-fix
- `parse` - Dump AST as JSON or tree view
- `native` - Direct tree-sitter pattern search
- `ts-query` - Execute S-expression queries

### UAST.Native
Rust FFI bindings (P/Invoke to uast_core.dll).
- `RustUastParser.cs` - High-level wrapper for Rust parsing
- `NativeInterop.cs` - P/Invoke declarations

### UAST.Parsers
Language mappers converting tree-sitter nodes to UAST.
- `TreeSitter/` - 70+ mappers (JavaScriptMapper, PythonMapper, etc.)
- `PowerShell/` - Native PowerShell mapper using System.Management.Automation
- `Dynamic/` - Dynamic language loading support

## Build

```bash
# Build all projects (also builds Rust native code)
dotnet build -c Release

# Publish self-contained executable
dotnet publish -c Release -r win-x64 --self-contained
```

## Dependencies

- Microsoft.CodeAnalysis.CSharp 4.12.0 (Roslyn)
- Microsoft.PowerShell.SDK 7.4.0
- System.CommandLine, Spectre.Console (CLI)
- YamlDotNet (rule parsing)
