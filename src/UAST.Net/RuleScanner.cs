using System.Runtime.InteropServices;
using System.Text;
using System.Text.Json;
using UAST.Net.Native;

namespace UAST.Net;

/// <summary>
/// Scanner for applying YAML rules to source code.
/// Detects patterns and issues based on configured rules.
/// </summary>
/// <remarks>
/// Rules are defined in YAML format with patterns, messages, and optional fixes.
/// The scanner can load rules from strings, files, or directories.
/// </remarks>
/// <example>
/// <code>
/// using var scanner = new RuleScanner();
/// scanner.LoadRules(@"
/// id: no-write-host
/// language: powershell
/// message: Use Write-Output instead of Write-Host
/// rule:
///   pattern: Write-Host $$$ARGS
/// ");
/// var results = scanner.Scan(psCode, "powershell");
/// </code>
/// </example>
public sealed class RuleScanner : IDisposable
{
    private readonly ScannerHandle _scanner;
    private bool _disposed;

    /// <summary>
    /// Creates a new rule scanner.
    /// </summary>
    public RuleScanner()
    {
        _scanner = ScannerHandle.Create();
    }

    /// <summary>
    /// Number of rules loaded.
    /// </summary>
    public int RuleCount => (int)_scanner.RuleCount;

    /// <summary>
    /// Load rules from a YAML string.
    /// </summary>
    /// <param name="yaml">YAML rule definition(s).</param>
    /// <exception cref="UastException">If rule parsing fails.</exception>
    public void LoadRules(string yaml)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        var result = UastNativeBindings.ScannerAddRules(_scanner.DangerousGetHandle(), yaml);
        if (result != UastNativeBindings.UastResult.Ok)
        {
            throw new UastException(
                (UastErrorCode)(int)result,
                "Failed to load rules from YAML");
        }
    }

    /// <summary>
    /// Load rules from a file.
    /// </summary>
    /// <param name="path">Path to YAML file.</param>
    /// <exception cref="FileNotFoundException">If file doesn't exist.</exception>
    /// <exception cref="UastException">If rule parsing fails.</exception>
    public void LoadRulesFromFile(string path)
    {
        if (!File.Exists(path))
        {
            throw new FileNotFoundException("Rule file not found", path);
        }

        var yaml = File.ReadAllText(path);
        LoadRules(yaml);
    }

    /// <summary>
    /// Load rules from a directory (recursively).
    /// </summary>
    /// <param name="path">Path to directory.</param>
    /// <param name="pattern">File search pattern (default: "*.yaml").</param>
    /// <exception cref="DirectoryNotFoundException">If directory doesn't exist.</exception>
    public void LoadRulesFromDirectory(string path, string pattern = "*.yaml")
    {
        if (!Directory.Exists(path))
        {
            throw new DirectoryNotFoundException($"Rule directory not found: {path}");
        }

        foreach (var file in Directory.EnumerateFiles(path, pattern, SearchOption.AllDirectories))
        {
            try
            {
                LoadRulesFromFile(file);
            }
            catch (UastException)
            {
                // Log or skip invalid rule files
            }
        }
    }

    /// <summary>
    /// Scan source code for rule violations.
    /// </summary>
    /// <param name="source">Source code to scan.</param>
    /// <param name="language">Language of the source.</param>
    /// <returns>Enumerable of scan results.</returns>
    public IEnumerable<ScanResult> Scan(string source, string language)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        // Parse to UAST JSON first
        using var parser = new UastParser(language);
        var json = parser.ParseToJson(source);
        if (json == null)
        {
            return [];
        }

        return ScanJson(json, language);
    }

    /// <summary>
    /// Scan a file for rule violations.
    /// </summary>
    /// <param name="path">Path to source file.</param>
    /// <param name="language">Language override (auto-detected from extension if null).</param>
    /// <returns>Enumerable of scan results with file paths.</returns>
    public IEnumerable<ScanResult> ScanFile(string path, string? language = null)
    {
        if (!File.Exists(path))
        {
            throw new FileNotFoundException("Source file not found", path);
        }

        var source = File.ReadAllText(path);
        var lang = language ?? GetLanguageFromExtension(path);

        if (string.IsNullOrEmpty(lang))
        {
            throw new UastException(
                UastErrorCode.UnknownLanguage,
                $"Cannot determine language for file: {path}");
        }

        var results = Scan(source, lang);

        // Add file path to results
        return results.Select(r => new ScanResult
        {
            RuleId = r.RuleId,
            Severity = r.Severity,
            Message = r.Message,
            Location = r.Location,
            FilePath = path,
            Captures = r.Captures,
            Fix = r.Fix
        });
    }

    /// <summary>
    /// Scan UAST JSON for rule violations.
    /// </summary>
    /// <param name="uastJson">UAST JSON string.</param>
    /// <param name="language">Language of the source.</param>
    /// <returns>Enumerable of scan results.</returns>
    public IEnumerable<ScanResult> ScanJson(string uastJson, string language)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        var jsonBytes = Encoding.UTF8.GetBytes(uastJson);
        var langBytes = Encoding.UTF8.GetBytes(language);

        unsafe
        {
            fixed (byte* jsonPtr = jsonBytes)
            fixed (byte* langPtr = langBytes)
            {
                var resultsPtr = UastNativeBindings.ScannerScanJson(
                    _scanner.DangerousGetHandle(),
                    (IntPtr)jsonPtr,
                    (uint)jsonBytes.Length,
                    (IntPtr)langPtr);

                if (resultsPtr == IntPtr.Zero)
                {
                    return [];
                }

                try
                {
                    var resultsJson = Marshal.PtrToStringUTF8(resultsPtr);
                    if (string.IsNullOrEmpty(resultsJson))
                    {
                        return [];
                    }

                    return JsonSerializer.Deserialize<List<ScanResult>>(resultsJson, JsonOptions) ?? [];
                }
                finally
                {
                    UastNativeBindings.FreeString(resultsPtr);
                }
            }
        }
    }

    /// <summary>
    /// Scan source code and return results as SARIF JSON.
    /// </summary>
    /// <param name="source">Source code to scan.</param>
    /// <param name="language">Language of the source.</param>
    /// <param name="filePath">Optional file path for SARIF output.</param>
    /// <param name="toolName">Tool name for SARIF (default: "UAST-Grep").</param>
    /// <param name="toolVersion">Tool version for SARIF (default: "1.0.0").</param>
    /// <returns>SARIF JSON string.</returns>
    public string? ScanToSarif(
        string source,
        string language,
        string? filePath = null,
        string toolName = "UAST-Grep",
        string toolVersion = "1.0.0")
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        // Parse to UAST JSON first
        using var parser = new UastParser(language);
        var json = parser.ParseToJson(source);
        if (json == null)
        {
            return null;
        }

        var jsonBytes = Encoding.UTF8.GetBytes(json);
        var langBytes = Encoding.UTF8.GetBytes(language);
        var nameBytes = Encoding.UTF8.GetBytes(toolName);
        var versionBytes = Encoding.UTF8.GetBytes(toolVersion);
        var pathBytes = filePath != null ? Encoding.UTF8.GetBytes(filePath) : null;

        unsafe
        {
            fixed (byte* jsonPtr = jsonBytes)
            fixed (byte* langPtr = langBytes)
            fixed (byte* namePtr = nameBytes)
            fixed (byte* versionPtr = versionBytes)
            fixed (byte* pathPtr = pathBytes)
            {
                var sarifPtr = UastNativeBindings.ScannerScanToSarif(
                    _scanner.DangerousGetHandle(),
                    (IntPtr)jsonPtr,
                    (uint)jsonBytes.Length,
                    (IntPtr)langPtr,
                    pathPtr != null ? (IntPtr)pathPtr : IntPtr.Zero,
                    (IntPtr)namePtr,
                    (IntPtr)versionPtr);

                if (sarifPtr == IntPtr.Zero)
                {
                    return null;
                }

                try
                {
                    return Marshal.PtrToStringUTF8(sarifPtr);
                }
                finally
                {
                    UastNativeBindings.FreeString(sarifPtr);
                }
            }
        }
    }

    /// <summary>
    /// Disposes the scanner and frees native resources.
    /// </summary>
    public void Dispose()
    {
        if (!_disposed)
        {
            _scanner.Dispose();
            _disposed = true;
        }
    }

    // ========================================================================
    // Static Methods
    // ========================================================================

    /// <summary>
    /// Get the SARIF schema version supported by this library.
    /// </summary>
    public static string SarifVersion
    {
        get
        {
            var ptr = UastNativeBindings.SarifVersion();
            return Marshal.PtrToStringUTF8(ptr) ?? "2.1.0";
        }
    }

    /// <summary>
    /// Convert scan results to SARIF JSON.
    /// </summary>
    /// <param name="results">Scan results to convert.</param>
    /// <param name="toolName">Tool name.</param>
    /// <param name="toolVersion">Tool version.</param>
    /// <returns>SARIF JSON string.</returns>
    public static string? ToSarif(
        IEnumerable<ScanResult> results,
        string toolName = "UAST-Grep",
        string toolVersion = "1.0.0")
    {
        var resultsJson = JsonSerializer.Serialize(results, JsonOptions);
        var resultsBytes = Encoding.UTF8.GetBytes(resultsJson);

        unsafe
        {
            fixed (byte* resultsPtr = resultsBytes)
            {
                var sarifPtr = UastNativeBindings.ScanResultsToSarifSimple(
                    (IntPtr)resultsPtr,
                    (uint)resultsBytes.Length);

                if (sarifPtr == IntPtr.Zero)
                {
                    return null;
                }

                try
                {
                    return Marshal.PtrToStringUTF8(sarifPtr);
                }
                finally
                {
                    UastNativeBindings.FreeString(sarifPtr);
                }
            }
        }
    }

    private static string? GetLanguageFromExtension(string path)
    {
        var ext = Path.GetExtension(path).ToLowerInvariant();
        return ext switch
        {
            ".rs" => "rust",
            ".py" => "python",
            ".js" => "javascript",
            ".ts" => "typescript",
            ".tsx" => "tsx",
            ".jsx" => "javascript",
            ".c" => "c",
            ".h" => "c",
            ".cpp" or ".cxx" or ".cc" => "cpp",
            ".hpp" or ".hxx" => "cpp",
            ".cs" => "c-sharp",
            ".java" => "java",
            ".go" => "go",
            ".rb" => "ruby",
            ".ps1" or ".psm1" or ".psd1" => "powershell",
            ".sh" or ".bash" => "bash",
            ".yaml" or ".yml" => "yaml",
            ".json" => "json",
            ".html" or ".htm" => "html",
            ".css" => "css",
            ".sql" => "sql",
            ".lua" => "lua",
            ".kt" or ".kts" => "kotlin",
            ".swift" => "swift",
            ".scala" => "scala",
            ".ex" or ".exs" => "elixir",
            ".erl" or ".hrl" => "erlang",
            ".hs" => "haskell",
            ".ml" or ".mli" => "ocaml",
            ".fs" or ".fsi" or ".fsx" => "fsharp",
            ".clj" or ".cljs" or ".cljc" => "clojure",
            ".php" => "php",
            ".pl" or ".pm" => "perl",
            ".r" => "r",
            ".jl" => "julia",
            ".zig" => "zig",
            ".nim" => "nim",
            ".d" => "d",
            ".dart" => "dart",
            ".vue" => "vue",
            ".svelte" => "svelte",
            ".tf" or ".hcl" => "hcl",
            ".toml" => "toml",
            ".xml" => "xml",
            ".md" or ".markdown" => "markdown",
            ".dockerfile" => "dockerfile",
            ".cmake" => "cmake",
            ".make" or ".mk" => "make",
            ".groovy" or ".gradle" => "groovy",
            ".proto" => "proto",
            ".graphql" or ".gql" => "graphql",
            _ => null
        };
    }

    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        PropertyNameCaseInsensitive = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };
}
