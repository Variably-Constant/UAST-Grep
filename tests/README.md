# UAST-Grep Tests

xUnit test projects for UAST-Grep components.

## Test Projects

### UAST.Core.Tests
Schema and matching engine tests.
- `SchemaTests.cs` - UAST node structure validation
- `PatternMatcherTests.cs` - Pattern matching logic
- `PatternParserTests.cs` - Pattern syntax parsing
- `Extensions/` - Language-specific extension tests (C#, Python, Rust, etc.)

### UAST.Cli.Tests
CLI command tests.
- `DualLayerQueryTests.cs` - UAST vs native query behavior
- `TreeSitterMapperTests.cs` - Tree-sitter mapper integration
- `Benchmarks/` - Performance benchmarks

### UAST.Native.Tests
Rust FFI binding tests.
- `InteropTests.cs` - P/Invoke interop validation

### UAST.PowerShell.Tests
PowerShell mapper tests.
- `PowerShellMapperTests.cs` - PS AST to UAST conversion

### UAST.Integration.Tests
End-to-end tests.
- `CrossLanguageTests.cs` - Same pattern across languages
- `CachingAndParallelTests.cs` - Performance scenarios
- `MapperIntegrationTests.cs` - Full mapper pipeline

## TestData

Sample files for all 70 supported languages:
```
TestData/Languages/
├── test.cs, test.py, test.js, test.rs   # Popular languages
├── test.go, test.java, test.cpp         # Systems languages
├── test.ps1, test.sh                     # Scripting
├── test.yaml, test.json, test.xml       # Data formats
└── ... (70+ files total)
```

## Running Tests

```bash
# Run all tests
dotnet test

# Run specific project
dotnet test tests/UAST.Core.Tests

# Run with verbosity
dotnet test --logger "console;verbosity=detailed"

# Filter by test name
dotnet test --filter "FullyQualifiedName~PatternMatcher"
```
