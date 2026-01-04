using UAST.Core.Interfaces;
using UAST.Core.Schema;

namespace UAST.Native;

/// <summary>
/// Language mapper backed by the Rust native parser.
/// Provides 10-16x performance improvement over C# tree-sitter bindings.
/// </summary>
public class RustBackedMapper : ILanguageMapper
{
    private readonly string _languageName;
    private readonly string[] _fileExtensions;
    private readonly List<ParseError> _errors = [];

    /// <summary>
    /// Creates a Rust-backed mapper for the specified language.
    /// </summary>
    /// <param name="languageName">The language name (e.g., "rust", "c", "cpp")</param>
    /// <param name="fileExtensions">File extensions this mapper handles</param>
    public RustBackedMapper(string languageName, string[] fileExtensions)
    {
        _languageName = languageName;
        _fileExtensions = fileExtensions;
    }

    /// <inheritdoc />
    public string LanguageName => _languageName;

    /// <inheritdoc />
    public IReadOnlyList<string> FileExtensions => _fileExtensions;

    /// <inheritdoc />
    public UastNode Parse(string source, string? filePath = null)
    {
        // Clear previous errors
        _errors.Clear();

        var result = RustUastParser.ParseSource(_languageName, source, filePath);

        if (result?.Root == null)
        {
            // Add a parse failure error
            _errors.Add(new ParseError(
                "Failed to parse source - Rust parser returned null",
                SourceSpan.Empty,
                "RUST001",
                ParseErrorSeverity.Error
            ));

            // Return an empty root node on parse failure
            return new UnknownNode
            {
                NodeKind = "SourceFile",
                Language = _languageName,
                NativeNodeType = "source_file",
                Span = SourceSpan.Empty,
                RawSource = source,
                ChildNodes = []
            };
        }

        // Convert parse errors from Rust
        if (result.Errors != null)
        {
            foreach (var err in result.Errors)
            {
                var span = err.Span != null
                    ? new SourceSpan(
                        err.Span.StartLine,
                        err.Span.StartColumn,
                        err.Span.EndLine,
                        err.Span.EndColumn,
                        err.Span.StartOffset,
                        err.Span.EndOffset)
                    : SourceSpan.Empty;

                var severity = err.Severity?.ToLowerInvariant() switch
                {
                    "warning" => ParseErrorSeverity.Warning,
                    "information" or "info" => ParseErrorSeverity.Information,
                    _ => ParseErrorSeverity.Error
                };

                _errors.Add(new ParseError(err.Message, span, null, severity));
            }
        }

        var rootNode = ConvertToUastNode(result.Root, source);

        // Set parent references after tree construction
        rootNode.SetParentReferences();

        return rootNode;
    }

    /// <inheritdoc />
    public UastNode ParseFile(string filePath)
    {
        var source = File.ReadAllText(filePath);
        return Parse(source, filePath);
    }

    /// <inheritdoc />
    public IReadOnlyList<ParseError> GetErrors() => _errors;

    /// <summary>
    /// Converts a RustUastNode to a UastNode (UnknownNode).
    /// </summary>
    private UastNode ConvertToUastNode(RustUastNode rustNode, string source)
    {
        // Convert children recursively
        var children = new List<UastNode>();
        if (rustNode.Children != null)
        {
            foreach (var child in rustNode.Children)
            {
                children.Add(ConvertToUastNode(child, source));
            }
        }

        // Convert span
        var span = rustNode.Span != null
            ? new SourceSpan(
                rustNode.Span.StartLine,
                rustNode.Span.StartColumn,
                rustNode.Span.EndLine,
                rustNode.Span.EndColumn,
                rustNode.Span.StartOffset,
                rustNode.Span.EndOffset)
            : SourceSpan.Empty;

        // Get raw source from span if not provided
        var rawSource = rustNode.RawSource;
        if (string.IsNullOrEmpty(rawSource) && span.IsValid && span.EndOffset <= source.Length)
        {
            rawSource = source[span.StartOffset..span.EndOffset];
        }

        // Get native node type from extensions if present
        var nativeNodeType = rustNode.NodeKind;
        if (rustNode.Extensions?.TryGetValue("nativeNodeType", out var nativeType) == true)
        {
            nativeNodeType = nativeType?.ToString() ?? rustNode.NodeKind;
        }

        // Build extensions dictionary
        Dictionary<string, object>? extensions = null;
        if (rustNode.Extensions != null || !string.IsNullOrEmpty(rustNode.Name))
        {
            extensions = new Dictionary<string, object>();
            if (rustNode.Extensions != null)
            {
                foreach (var kvp in rustNode.Extensions)
                {
                    extensions[kvp.Key] = kvp.Value;
                }
            }
            if (!string.IsNullOrEmpty(rustNode.Name))
            {
                extensions["name"] = rustNode.Name;
            }
        }

        var node = new UnknownNode
        {
            NodeKind = rustNode.NodeKind,
            Language = rustNode.Language,
            NativeNodeType = nativeNodeType,
            Span = span,
            RawSource = rawSource,
            Extensions = extensions,
            ChildNodes = children
        };

        return node;
    }

    /// <summary>
    /// Creates a Rust-backed mapper for the specified language.
    /// Supports all 70 languages with statically-compiled tree-sitter grammars.
    /// </summary>
    /// <param name="language">Language name (e.g., "python", "javascript", "go")</param>
    /// <returns>A RustBackedMapper for the language</returns>
    public static RustBackedMapper Create(string language)
    {
        var langLower = language.ToLowerInvariant();
        var extensions = GetExtensionsForLanguage(langLower);
        return new RustBackedMapper(langLower, extensions);
    }

    /// <summary>
    /// Gets file extensions for a language.
    /// </summary>
    private static string[] GetExtensionsForLanguage(string language) => language switch
    {
        // Tier 1: Official tree-sitter (21)
        "agda" => [".agda", ".lagda"],
        "bash" or "sh" or "shell" => [".sh", ".bash", ".zsh"],
        "c" => [".c", ".h"],
        "cpp" or "c++" or "cxx" => [".cpp", ".hpp", ".cc", ".cxx", ".hh", ".hxx"],
        "c-sharp" or "csharp" or "cs" => [".cs"],
        "css" => [".css"],
        "go" or "golang" => [".go"],
        "haskell" or "hs" => [".hs", ".lhs"],
        "html" => [".html", ".htm"],
        "java" => [".java"],
        "javascript" or "js" => [".js", ".mjs", ".cjs", ".jsx"],
        "json" => [".json"],
        "julia" or "jl" => [".jl"],
        "ocaml" or "ml" => [".ml", ".mli"],
        "php" => [".php", ".phtml"],
        "python" or "py" => [".py", ".pyi", ".pyw"],
        "ruby" or "rb" => [".rb", ".rake", ".gemspec"],
        "rust" or "rs" => [".rs"],
        "scala" => [".scala", ".sc"],
        "typescript" or "ts" => [".ts", ".mts", ".cts"],
        "tsx" => [".tsx"],
        "verilog" or "v" => [".v", ".vh"],

        // Tier 2: tree-sitter-grammars (18)
        "arduino" or "ino" => [".ino", ".pde"],
        "bicep" => [".bicep"],
        "bitbake" or "bb" => [".bb", ".bbappend", ".bbclass"],
        "cairo" => [".cairo"],
        "csv" => [".csv", ".tsv"],
        "cuda" or "cu" => [".cu", ".cuh"],
        "doxygen" => [],
        "hcl" or "terraform" or "tf" => [".hcl", ".tf", ".tfvars"],
        "lua" => [".lua"],
        "make" or "makefile" => [".mk"],
        "markdown" or "md" => [".md", ".markdown"],
        "objc" or "objective-c" => [".m", ".mm"],
        "toml" => [".toml"],
        "vim" or "vimscript" => [".vim", ".vimrc"],
        "vue" => [".vue"],
        "xml" => [".xml", ".xsd", ".xsl"],
        "yaml" or "yml" => [".yaml", ".yml"],
        "zig" => [".zig"],

        // Tier 3: Community grammars (31)
        "ada" => [".ada", ".adb", ".ads"],
        "angular" => [],
        "apex" or "sfapex" => [".cls", ".trigger"],
        "awk" => [".awk"],
        "clojure" or "clj" => [".clj", ".cljs", ".cljc", ".edn"],
        "cmake" => [".cmake"],
        "cobol" or "cob" => [".cob", ".cbl", ".cpy"],
        "comment" => [],
        "commonlisp" or "lisp" or "cl" => [".lisp", ".lsp", ".cl"],
        "crystal" or "cr" => [".cr"],
        "cue" => [".cue"],
        "d" or "dlang" => [".d", ".di"],
        "dart" => [".dart"],
        "dhall" => [".dhall"],
        "dockerfile" or "docker" => [".dockerfile"],
        "elixir" or "ex" or "exs" => [".ex", ".exs"],
        "elm" => [".elm"],
        "erlang" or "erl" => [".erl", ".hrl"],
        "fsharp" or "fs" or "f#" => [".fs", ".fsi", ".fsx"],
        "fortran" or "f90" or "f95" => [".f", ".for", ".f90", ".f95", ".f03", ".f08"],
        "graphql" or "gql" => [".graphql", ".gql"],
        "groovy" => [".groovy", ".gvy", ".gy", ".gsh", ".gradle"],
        "kotlin" or "kt" => [".kt", ".kts"],
        "latex" or "tex" => [".tex", ".sty", ".cls"],
        "nix" => [".nix"],
        "perl" or "pl" => [".pl", ".pm"],
        "powershell" or "ps1" or "pwsh" => [".ps1", ".psm1", ".psd1"],
        "proto" or "protobuf" => [".proto"],
        "r" => [".r", ".R"],
        "sql" => [".sql"],
        "swift" => [".swift"],

        _ => []
    };

    /// <summary>
    /// Creates a Rust-backed mapper for Rust language.
    /// </summary>
    public static RustBackedMapper CreateRustMapper() =>
        new("rust", [".rs"]);

    /// <summary>
    /// Creates a Rust-backed mapper for C language.
    /// </summary>
    public static RustBackedMapper CreateCMapper() =>
        new("c", [".c", ".h"]);

    /// <summary>
    /// Creates a Rust-backed mapper for C++ language.
    /// </summary>
    public static RustBackedMapper CreateCppMapper() =>
        new("cpp", [".cpp", ".hpp", ".cc", ".cxx", ".hh", ".hxx", ".c++", ".h++"]);
}
