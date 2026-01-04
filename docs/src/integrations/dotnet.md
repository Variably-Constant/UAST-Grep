# .NET API

The .NET API provides low-level P/Invoke bindings to the Rust core library.

## Installation

### From NuGet

```bash
dotnet add package UAST.Native
```

### From Source

```bash
# Clone the repository
git clone https://github.com/Variably-Constant/UAST-Grep.git
cd UAST-Grep

# Build the native library first
cd native/uast_core
cargo build --release
cd ../..

# Copy the DLL to the .NET project
# Windows: copy native/uast_core/target/release/uast_core.dll to your project

# Build .NET wrapper
dotnet build src/UAST.Native -c Release
```

## Quick Start

```csharp
using UAST.Native;

// Initialize the library
UastNative.Init();

// Check version
Console.WriteLine($"Version: {UastNativeExtensions.GetVersion()}");

// Create a parser
var parser = UastNative.ParserNew("python");
if (parser == IntPtr.Zero)
{
    Console.WriteLine("Failed to create parser");
    return;
}

// Parse source code
var source = "def hello(): pass";
var sourcePtr = Marshal.StringToHGlobalAnsi(source);
var tree = UastNative.Parse(parser, sourcePtr, (uint)source.Length);
Marshal.FreeHGlobal(sourcePtr);

if (tree != IntPtr.Zero)
{
    // Get node count
    Console.WriteLine($"Nodes: {UastNative.TreeNodeCount(tree)}");

    // Check for errors
    if (UastNative.TreeHasError(tree))
    {
        Console.WriteLine("Parse errors detected");
    }

    // Clean up
    UastNative.TreeFree(tree);
}

UastNative.ParserFree(parser);
```

## API Reference

### Initialization

```csharp
// Initialize the library (call once)
UastNative.UastResult result = UastNative.Init();
if (!result.IsSuccess())
{
    throw new Exception("Failed to initialize UAST");
}

// Get version string
string version = UastNativeExtensions.GetVersion();

// Get language count
uint count = UastNative.LanguageCount();
```

### Parser Management

```csharp
// Create parser by language name
IntPtr parser = UastNative.ParserNew("python");

// Create parser by file extension
IntPtr parser = UastNative.ParserForExtension(".py");

// Create parser with language pointer (for dynamic grammars)
IntPtr parser = UastNative.ParserNewWithLanguage(languagePtr, "custom");

// Free parser when done
UastNative.ParserFree(parser);
```

### Parsing

```csharp
// Parse source code
IntPtr tree = UastNative.Parse(parser, sourcePtr, sourceLen);

// Check if parsing succeeded
if (tree == IntPtr.Zero)
{
    // Handle error
}

// Free tree when done
UastNative.TreeFree(tree);
```

### Tree Information

```csharp
// Get node count
uint nodeCount = UastNative.TreeNodeCount(tree);

// Check for parse errors
bool hasErrors = UastNative.TreeHasError(tree);

// Get root node
unsafe
{
    UastNative.UastNode rootNode;
    var result = UastNative.TreeRootNode(tree, &rootNode);
    if (result.IsSuccess())
    {
        Console.WriteLine($"Root kind: {rootNode.GetKind()}");
    }
}
```

### Tree Walking

Walk all nodes in the tree:

```csharp
// Define callback
unsafe bool NodeHandler(UastNative.UastNode* node, IntPtr userData)
{
    Console.WriteLine($"Node: {node->GetKind()}");
    Console.WriteLine($"  Line: {node->Range.StartPoint.Row + 1}");
    Console.WriteLine($"  Named: {node->IsNamed}");
    return true; // Continue walking
}

// Walk all nodes
UastNative.TreeWalk(tree, NodeHandler, IntPtr.Zero);

// Walk only named nodes
UastNative.TreeWalkNamed(tree, NodeHandler, IntPtr.Zero);
```

### Query Execution

Execute tree-sitter queries:

```csharp
unsafe bool MatchHandler(UastNative.UastMatch* match, IntPtr userData)
{
    Console.WriteLine($"Pattern {match->PatternIndex} matched");

    for (uint i = 0; i < match->CaptureCount; i++)
    {
        var capture = match->Captures[i];
        Console.WriteLine($"  @{capture.GetName()}: {capture.Node.GetKind()}");
    }

    return true; // Continue matching
}

// Execute query
var query = "(function_definition name: (identifier) @name)";
var queryPtr = Marshal.StringToHGlobalAnsi(query);
UastNative.QueryExecute(tree, queryPtr, (uint)query.Length, MatchHandler, IntPtr.Zero);
Marshal.FreeHGlobal(queryPtr);
```

### Language Registration

Register dynamic grammars:

```csharp
// Load grammar from DLL
using var library = NativeLibrary.Load("tree-sitter-kotlin.dll");
var languageFunc = NativeLibrary.GetExport(library, "tree_sitter_kotlin");

// Get language pointer
var languagePtr = ((delegate* unmanaged[Cdecl]<IntPtr>)languageFunc)();

// Register with UAST
bool success = UastNative.RegisterLanguage("kotlin", languagePtr);

// Register extensions
var extensions = new[] { ".kt", ".kts" };
// ... marshal and register

// Check if registered
bool hasKotlin = UastNative.HasLanguage("kotlin");

// Unregister when done
UastNative.UnregisterLanguage("kotlin");
```

## Structs

### UastResult

Return codes from native functions:

```csharp
public enum UastResult : int
{
    Ok = 0,
    NullPointer = 1,
    InvalidUtf8 = 2,
    UnknownLanguage = 3,
    ParseFailed = 4,
    QueryFailed = 5,
    InternalError = 99
}
```

### UastNode

Represents an AST node:

```csharp
[StructLayout(LayoutKind.Sequential)]
public unsafe struct UastNode
{
    public byte* Kind;
    public uint KindLen;
    public byte* FieldName;
    public uint FieldNameLen;
    public UastRange Range;
    public uint ChildCount;
    public uint NamedChildCount;
    public bool IsNamed;
    public bool IsMissing;
    public bool HasError;
    public ulong Id;
    public ulong ParentId;

    // Helper methods
    public readonly string GetKind();
    public readonly string? GetFieldName();
}
```

### UastRange

Position information:

```csharp
[StructLayout(LayoutKind.Sequential)]
public struct UastRange
{
    public uint StartByte;
    public uint EndByte;
    public UastPoint StartPoint;
    public UastPoint EndPoint;
}

[StructLayout(LayoutKind.Sequential)]
public struct UastPoint
{
    public uint Row;     // 0-indexed line
    public uint Column;  // 0-indexed column
}
```

## Examples

### Complete Parser Example

```csharp
using System;
using System.Runtime.InteropServices;
using UAST.Native;

public class UastExample
{
    public static void Main()
    {
        // Initialize
        UastNative.Init().ThrowIfFailed("Init");

        // Create parser
        var parser = UastNative.ParserNew("python");
        if (parser == IntPtr.Zero)
            throw new Exception("Failed to create parser");

        try
        {
            // Parse source
            var source = @"
def hello(name):
    print(f'Hello, {name}!')

def goodbye():
    print('Goodbye!')
";
            var sourceBytes = System.Text.Encoding.UTF8.GetBytes(source);
            var sourcePtr = Marshal.AllocHGlobal(sourceBytes.Length);
            Marshal.Copy(sourceBytes, 0, sourcePtr, sourceBytes.Length);

            var tree = UastNative.Parse(parser, sourcePtr, (uint)sourceBytes.Length);
            Marshal.FreeHGlobal(sourcePtr);

            if (tree == IntPtr.Zero)
                throw new Exception("Failed to parse");

            try
            {
                // Walk the tree
                var functionCount = 0;
                unsafe
                {
                    UastNative.TreeWalkNamed(tree, (node, _) =>
                    {
                        var kind = node->GetKind();
                        if (kind == "function_definition")
                        {
                            functionCount++;
                            Console.WriteLine($"Found function at line {node->Range.StartPoint.Row + 1}");
                        }
                        return true;
                    }, IntPtr.Zero);
                }

                Console.WriteLine($"Total functions: {functionCount}");
            }
            finally
            {
                UastNative.TreeFree(tree);
            }
        }
        finally
        {
            UastNative.ParserFree(parser);
        }
    }
}
```

### Query Example

```csharp
using System;
using System.Runtime.InteropServices;
using UAST.Native;

public class QueryExample
{
    public static void FindFunctions(string source)
    {
        UastNative.Init();
        var parser = UastNative.ParserNew("python");

        var sourceBytes = System.Text.Encoding.UTF8.GetBytes(source);
        var sourcePtr = Marshal.AllocHGlobal(sourceBytes.Length);
        Marshal.Copy(sourceBytes, 0, sourcePtr, sourceBytes.Length);

        var tree = UastNative.Parse(parser, sourcePtr, (uint)sourceBytes.Length);
        Marshal.FreeHGlobal(sourcePtr);

        // Query for function definitions with names
        var query = "(function_definition name: (identifier) @name)";
        var queryBytes = System.Text.Encoding.UTF8.GetBytes(query);
        var queryPtr = Marshal.AllocHGlobal(queryBytes.Length);
        Marshal.Copy(queryBytes, 0, queryPtr, queryBytes.Length);

        unsafe
        {
            UastNative.QueryExecute(tree, queryPtr, (uint)queryBytes.Length,
                (match, _) =>
                {
                    for (uint i = 0; i < match->CaptureCount; i++)
                    {
                        var capture = match->Captures[i];
                        if (capture.GetName() == "name")
                        {
                            var start = capture.Node.Range.StartByte;
                            var end = capture.Node.Range.EndByte;
                            var name = source.Substring((int)start, (int)(end - start));
                            Console.WriteLine($"Function: {name}");
                        }
                    }
                    return true;
                }, IntPtr.Zero);
        }

        Marshal.FreeHGlobal(queryPtr);
        UastNative.TreeFree(tree);
        UastNative.ParserFree(parser);
    }
}
```

### High-Level Wrapper

Consider creating a high-level wrapper for easier use:

```csharp
public class UastParser : IDisposable
{
    private IntPtr _parser;

    public UastParser(string language)
    {
        UastNative.Init();
        _parser = UastNative.ParserNew(language);
        if (_parser == IntPtr.Zero)
            throw new ArgumentException($"Unknown language: {language}");
    }

    public UastTree Parse(string source)
    {
        var bytes = System.Text.Encoding.UTF8.GetBytes(source);
        var ptr = Marshal.AllocHGlobal(bytes.Length);
        Marshal.Copy(bytes, 0, ptr, bytes.Length);

        try
        {
            var tree = UastNative.Parse(_parser, ptr, (uint)bytes.Length);
            if (tree == IntPtr.Zero)
                throw new InvalidOperationException("Parse failed");
            return new UastTree(tree, source);
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
    }

    public void Dispose()
    {
        if (_parser != IntPtr.Zero)
        {
            UastNative.ParserFree(_parser);
            _parser = IntPtr.Zero;
        }
    }
}

public class UastTree : IDisposable
{
    private IntPtr _tree;
    private readonly string _source;

    internal UastTree(IntPtr tree, string source)
    {
        _tree = tree;
        _source = source;
    }

    public int NodeCount => (int)UastNative.TreeNodeCount(_tree);
    public bool HasErrors => UastNative.TreeHasError(_tree);

    public void Dispose()
    {
        if (_tree != IntPtr.Zero)
        {
            UastNative.TreeFree(_tree);
            _tree = IntPtr.Zero;
        }
    }
}
```

## Platform Support

The .NET bindings support:

- **.NET 8.0+** (recommended)
- **.NET Framework 4.7.2+** (requires native DLL in PATH)
- **Windows x64** - `uast_core.dll`
- **Linux x64** - `libuast_core.so`
- **macOS x64/ARM64** - `libuast_core.dylib`

## Troubleshooting

### DLL Not Found

Ensure the native library is in one of:
- Same directory as the executable
- System PATH
- Explicitly loaded with `NativeLibrary.SetDllImportResolver`

### Memory Issues

Always free resources:
```csharp
// Always free trees
UastNative.TreeFree(tree);

// Always free parsers
UastNative.ParserFree(parser);

// Always free marshalled strings
Marshal.FreeHGlobal(ptr);
```

### Unicode Issues

Use UTF-8 encoding for source code:
```csharp
var bytes = System.Text.Encoding.UTF8.GetBytes(source);
```
