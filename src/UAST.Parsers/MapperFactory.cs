using UAST.Core.Interfaces;
using UAST.Native;
using UAST.Parsers.Roslyn;
using UAST.Parsers.PowerShell;

namespace UAST.Parsers;

/// <summary>
/// Factory for creating language mappers.
/// Routes to appropriate parser backend:
/// - PowerShell: Native PowerShell AST (System.Management.Automation)
/// - C#: Roslyn (Microsoft.CodeAnalysis)
/// - All other languages (70+): Rust native backend (uast_core.dll with static grammars)
/// </summary>
public static class MapperFactory
{
    // Cache for Rust backend availability (checked once per app lifetime)
    private static readonly Lazy<bool> _rustBackendAvailable = new(() =>
    {
        try { return RustUastParser.IsNativeLibraryLoaded(); }
        catch { return false; }
    });

    /// <summary>
    /// Whether the Rust native backend is available.
    /// When true, 70+ languages are parsed via statically-compiled tree-sitter grammars.
    /// </summary>
    public static bool IsRustBackendAvailable => _rustBackendAvailable.Value;

    /// <summary>
    /// Gets a language mapper for the specified language.
    /// </summary>
    /// <param name="language">The language name (case-insensitive).</param>
    /// <returns>A language mapper instance.</returns>
    /// <exception cref="ArgumentException">Thrown when the language is not supported.</exception>
    public static ILanguageMapper GetMapper(string language)
    {
        var langLower = language.ToLowerInvariant();

        // Native SDK parsers for deep semantic analysis
        return langLower switch
        {
            // PowerShell: Use native PowerShell AST for rich semantic info
            "powershell" or "ps1" or "pwsh" => new PowerShellMapper(),

            // C#: Use Roslyn for rich semantic info
            "csharp" or "c#" or "cs" => new CSharpMapper(),

            // All other languages: Rust backend with static tree-sitter grammars
            _ => GetRustBackedMapper(langLower)
        };
    }

    /// <summary>
    /// Gets a Rust-backed mapper for the specified language.
    /// Uses uast_core.dll with 70 statically-compiled tree-sitter grammars.
    /// </summary>
    private static ILanguageMapper GetRustBackedMapper(string language)
    {
        if (!IsRustBackendAvailable)
        {
            throw new InvalidOperationException(
                $"Rust native backend not available. Ensure uast_core.dll is present.");
        }

        if (!RustUastParser.IsAvailable(language))
        {
            throw new ArgumentException(
                $"Unknown language: {language}. Use MapperFactory.SupportedLanguages to see available options.",
                nameof(language));
        }

        return RustBackedMapper.Create(language);
    }

    /// <summary>
    /// Gets a mapper for a file based on its extension.
    /// </summary>
    /// <param name="filePath">The file path or extension.</param>
    /// <returns>A language mapper instance, or null if no mapper handles this extension.</returns>
    public static ILanguageMapper? GetMapperForFile(string filePath)
    {
        var extension = Path.GetExtension(filePath).ToLowerInvariant();
        if (string.IsNullOrEmpty(extension))
            return null;

        var language = GetLanguageForExtension(extension);
        if (language == null)
            return null;

        try
        {
            return GetMapper(language);
        }
        catch
        {
            return null;
        }
    }

    /// <summary>
    /// Gets the language name for a file extension.
    /// </summary>
    private static string? GetLanguageForExtension(string extension) => extension switch
    {
        // PowerShell (native)
        ".ps1" or ".psm1" or ".psd1" => "powershell",

        // C# (Roslyn)
        ".cs" => "csharp",

        // Tier 1: Major languages
        ".php" or ".phtml" => "php",
        ".rb" or ".rake" or ".gemspec" or ".ru" => "ruby",
        ".py" or ".pyi" or ".pyw" => "python",
        ".go" => "go",
        ".java" => "java",
        ".rs" => "rust",
        ".swift" => "swift",
        ".ts" or ".mts" or ".cts" => "typescript",
        ".tsx" => "tsx",
        ".js" or ".jsx" or ".mjs" or ".cjs" => "javascript",
        ".kt" or ".kts" => "kotlin",
        ".dart" => "dart",
        ".scala" or ".sc" => "scala",

        // Tier 2: Systems & low-level
        ".c" or ".h" => "c",
        ".cpp" or ".hpp" or ".cc" or ".hh" or ".cxx" or ".hxx" => "cpp",
        ".zig" => "zig",
        ".erl" or ".hrl" => "erlang",
        ".ex" or ".exs" => "elixir",

        // Tier 3: Functional languages
        ".ml" or ".mli" => "ocaml",
        ".hs" or ".lhs" => "haskell",
        ".fs" or ".fsi" or ".fsx" => "fsharp",
        ".clj" or ".cljs" or ".cljc" or ".edn" => "clojure",

        // Tier 4: Scripting languages
        ".sh" or ".bash" or ".zsh" => "bash",
        ".lua" => "lua",
        ".pl" or ".pm" => "perl",
        ".r" or ".R" => "r",
        ".jl" => "julia",
        ".groovy" or ".gvy" or ".gy" or ".gsh" => "groovy",

        // Tier 5: Web & markup
        ".html" or ".htm" => "html",
        ".css" => "css",
        ".json" => "json",
        ".xml" or ".xsl" or ".xslt" or ".xsd" => "xml",
        ".yaml" or ".yml" => "yaml",
        ".md" or ".markdown" => "markdown",
        ".vue" => "vue",

        // Tier 6: Build & config
        ".dockerfile" => "dockerfile",
        ".mk" => "make",
        ".cmake" => "cmake",
        ".toml" => "toml",
        ".hcl" or ".tf" or ".tfvars" => "hcl",
        ".nix" => "nix",

        // Tier 7: Data & query languages
        ".sql" => "sql",
        ".graphql" or ".gql" => "graphql",
        ".proto" => "proto",

        // Tier 8: Specialized
        ".agda" or ".lagda" => "agda",
        ".sv" or ".svh" => "verilog",
        ".v" or ".vh" => "verilog",

        // Tier 9: Additional languages
        ".ada" or ".adb" or ".ads" => "ada",
        ".ino" or ".pde" => "arduino",
        ".awk" => "awk",
        ".bicep" => "bicep",
        ".cairo" => "cairo",
        ".cob" or ".cbl" or ".cpy" => "cobol",
        ".lisp" or ".lsp" or ".cl" => "commonlisp",
        ".cr" => "crystal",
        ".csv" or ".tsv" => "csv",
        ".cu" or ".cuh" => "cuda",
        ".cue" => "cue",
        ".d" or ".di" => "d",
        ".dhall" => "dhall",
        ".elm" => "elm",
        ".erb" or ".ejs" => "embeddedtemplate",
        ".f" or ".for" or ".f90" or ".f95" or ".f03" or ".f08" => "fortran",
        ".tex" or ".sty" => "latex",
        ".m" or ".mm" => "objc",
        ".trigger" or ".cls" => "apex",
        ".vim" or ".vimrc" => "vim",

        _ => null
    };

    /// <summary>
    /// Gets instances of all available language mappers.
    /// </summary>
    /// <returns>A list of all mapper instances.</returns>
    public static IReadOnlyList<ILanguageMapper> GetAllMappers()
    {
        var mappers = new List<ILanguageMapper>();

        // Native SDK mappers (always available)
        mappers.Add(new PowerShellMapper());
        mappers.Add(new CSharpMapper());

        // Rust-backed mappers (when backend available)
        if (IsRustBackendAvailable)
        {
            foreach (var lang in RustBackedLanguages)
            {
                try
                {
                    if (RustUastParser.IsAvailable(lang))
                    {
                        mappers.Add(RustBackedMapper.Create(lang));
                    }
                }
                catch
                {
                    // Skip unavailable languages
                }
            }
        }

        return mappers;
    }

    /// <summary>
    /// Languages handled by the Rust backend (70 languages).
    /// </summary>
    private static readonly string[] RustBackedLanguages =
    [
        // Tier 1: Official tree-sitter (21)
        "agda", "bash", "c", "cpp", "css", "go", "haskell",
        "html", "java", "javascript", "json", "julia", "ocaml", "php",
        "python", "ruby", "rust", "scala", "typescript", "tsx", "verilog",

        // Tier 2: tree-sitter-grammars (18)
        "arduino", "bicep", "bitbake", "cairo", "csv", "cuda", "doxygen",
        "hcl", "lua", "make", "markdown", "objc", "toml", "vim", "vue",
        "xml", "yaml", "zig",

        // Tier 3: Community grammars (31)
        "ada", "angular", "apex", "awk", "clojure", "cmake", "cobol",
        "comment", "commonlisp", "crystal", "cue", "d", "dart", "dhall",
        "dockerfile", "elixir", "elm", "erlang", "fsharp", "fortran",
        "graphql", "groovy", "kotlin", "latex", "nix", "perl", "powershell",
        "proto", "r", "sql", "swift"
    ];

    /// <summary>
    /// Gets the names of all supported languages (72 languages).
    /// </summary>
    public static IReadOnlyList<string> SupportedLanguages =>
    [
        // Native SDK parsers
        "PowerShell", "CSharp",

        // Tier 1: Official tree-sitter (21)
        "Agda", "Bash", "C", "C++", "CSS", "Go", "Haskell",
        "HTML", "Java", "JavaScript", "JSON", "Julia", "OCaml", "PHP",
        "Python", "Ruby", "Rust", "Scala", "TypeScript", "TSX", "Verilog",

        // Tier 2: tree-sitter-grammars (18)
        "Arduino", "Bicep", "BitBake", "Cairo", "CSV", "CUDA", "Doxygen",
        "HCL", "Lua", "Make", "Markdown", "ObjC", "TOML", "Vim", "Vue",
        "XML", "YAML", "Zig",

        // Tier 3: Community grammars (31)
        "Ada", "Angular", "Apex", "AWK", "Clojure", "CMake", "COBOL",
        "Comment", "CommonLisp", "Crystal", "Cue", "D", "Dart", "Dhall",
        "Dockerfile", "Elixir", "Elm", "Erlang", "FSharp", "Fortran",
        "GraphQL", "Groovy", "Kotlin", "LaTeX", "Nix", "Perl",
        "Proto", "R", "SQL", "Swift"
    ];

    /// <summary>
    /// Gets all supported file extensions across all mappers.
    /// </summary>
    public static IReadOnlyList<string> SupportedExtensions =>
    [
        // PowerShell (native)
        ".ps1", ".psm1", ".psd1",

        // C# (Roslyn)
        ".cs",

        // Tier 1: Major languages
        ".php", ".phtml",
        ".rb", ".rake", ".gemspec", ".ru",
        ".py", ".pyi", ".pyw",
        ".go",
        ".java",
        ".rs",
        ".swift",
        ".ts", ".tsx", ".mts", ".cts",
        ".js", ".jsx", ".mjs", ".cjs",
        ".kt", ".kts",
        ".dart",
        ".scala", ".sc",

        // Tier 2: Systems & low-level
        ".c", ".h",
        ".cpp", ".hpp", ".cc", ".hh", ".cxx", ".hxx",
        ".zig",
        ".erl", ".hrl",
        ".ex", ".exs",

        // Tier 3: Functional languages
        ".ml", ".mli",
        ".hs", ".lhs",
        ".fs", ".fsi", ".fsx",
        ".clj", ".cljs", ".cljc", ".edn",

        // Tier 4: Scripting languages
        ".sh", ".bash", ".zsh",
        ".lua",
        ".pl", ".pm",
        ".r", ".R",
        ".jl",
        ".groovy", ".gvy", ".gy", ".gsh",

        // Tier 5: Web & markup
        ".html", ".htm",
        ".css",
        ".json",
        ".xml", ".xsl", ".xslt", ".xsd",
        ".yaml", ".yml",
        ".md", ".markdown",
        ".vue",

        // Tier 6: Build & config
        "Dockerfile", ".dockerfile",
        "Makefile", ".mk",
        "CMakeLists.txt", ".cmake",
        ".toml",
        ".hcl", ".tf", ".tfvars",
        ".nix",

        // Tier 7: Data & query languages
        ".sql",
        ".graphql", ".gql",
        ".proto",

        // Tier 8: Specialized
        ".agda", ".lagda",
        ".sv", ".svh",
        ".v", ".vh",

        // Tier 9: Additional languages
        ".ada", ".adb", ".ads",
        ".ino", ".pde",
        ".awk",
        ".bicep",
        ".cairo",
        ".cob", ".cbl", ".cpy",
        ".lisp", ".lsp", ".cl",
        ".cr",
        ".csv", ".tsv",
        ".cu", ".cuh",
        ".cue",
        ".d", ".di",
        ".dhall",
        ".elm",
        ".erb", ".ejs",
        ".f", ".for", ".f90", ".f95", ".f03", ".f08",
        ".tex", ".sty", ".cls",
        ".m", ".mm",
        ".trigger",
        ".vim", ".vimrc"
    ];

    /// <summary>
    /// Checks if a language is supported.
    /// </summary>
    /// <param name="language">The language name to check.</param>
    /// <returns>True if the language is supported.</returns>
    public static bool IsLanguageSupported(string language)
    {
        try
        {
            GetMapper(language);
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Checks if a file extension is supported.
    /// </summary>
    /// <param name="extension">The file extension (with or without leading dot).</param>
    /// <returns>True if the extension is supported.</returns>
    public static bool IsExtensionSupported(string extension)
    {
        if (!extension.StartsWith('.'))
            extension = "." + extension;

        return SupportedExtensions.Contains(extension, StringComparer.OrdinalIgnoreCase);
    }
}
