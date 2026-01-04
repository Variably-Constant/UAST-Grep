using System.Runtime.InteropServices;
using UAST.Parsers.Dynamic;

var basePath = @"C:\Program Files\WindowsPowerShell\Modules\UAST-Grep\runtimes\win-x64\native";

// Check what symbols yaml exports - try many patterns
Console.WriteLine("=== Checking yaml exports ===");
var yamlPath = Path.Combine(basePath, "tree-sitter-yaml.dll");
if (NativeLibrary.TryLoad(yamlPath, out var yamlHandle))
{
    var yamlPatterns = new[]
    {
        "tree_sitter_yaml",
        "tree_sitter_YAML",
        "tree_sitter_Yaml",
        "ts_language_yaml",
        "_tree_sitter_yaml",
    };
    foreach (var p in yamlPatterns)
    {
        if (NativeLibrary.TryGetExport(yamlHandle, p, out var exp))
            Console.WriteLine($"  FOUND: {p}");
    }
    NativeLibrary.Free(yamlHandle);
}

// Test swift with registry to ensure core is loaded first
Console.WriteLine("\n=== Testing swift via GrammarRegistry ===");
var registry = GrammarRegistry.Default;
Console.WriteLine($"  Registry search paths loaded, available: {registry.AvailableGrammars.Count()} grammars");
Console.WriteLine($"  Has swift: {registry.HasGrammar("swift")}");

var swiftLang = registry.GetLanguage("swift");
if (swiftLang != null)
{
    Console.WriteLine($"  Swift loaded! ABI Version: {swiftLang.Version}, Symbols: {swiftLang.SymbolCount}");

    try
    {
        using var parser = registry.CreateParser("swift");
        if (parser != null)
        {
            Console.WriteLine("  Parser created!");
            using var tree = parser.Parse("func hello() { }");
            Console.WriteLine($"  Parse result: {(tree != null ? $"Root: {tree.RootNode.Type}" : "null")}");
        }
        else
        {
            Console.WriteLine("  CreateParser returned null");
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine($"  Parser error: {ex.Message}");
    }
}
else
{
    Console.WriteLine("  GetLanguage returned null");
}

// Check expected ABI version
Console.WriteLine("\n=== Reference parser ABI versions ===");
foreach (var lang in new[] { "python", "javascript", "c" })
{
    var l = registry.GetLanguage(lang);
    if (l != null)
    {
        Console.WriteLine($"  {lang}: ABI version {l.Version}");
    }
}
