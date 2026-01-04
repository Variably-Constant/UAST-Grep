using System.Runtime.InteropServices;
using System.Text.Json;
using UAST.Net.Native;

namespace UAST.Net;

/// <summary>
/// Pattern matcher for UAST and native tree-sitter patterns.
/// Supports both cross-language UAST patterns (PascalCase) and native patterns (snake_case).
/// </summary>
/// <remarks>
/// UAST patterns use PascalCase (e.g., "FunctionDeclaration") and are automatically
/// translated to native types for the target language. Native patterns use the
/// tree-sitter node types directly.
/// </remarks>
/// <example>
/// <code>
/// // Cross-language UAST pattern
/// var matcher = new PatternMatcher("FunctionDeclaration", "rust");
/// var matches = matcher.Matches(rustSource);
///
/// // Also works for Python with the same pattern
/// var pyMatcher = new PatternMatcher("FunctionDeclaration", "python");
/// var pyMatches = pyMatcher.Matches(pythonSource);
/// </code>
/// </example>
public sealed class PatternMatcher : IDisposable
{
    private readonly PatternHandle? _pattern;
    private readonly string _patternString;
    private readonly string _language;
    private readonly bool _isUastPattern;
    private bool _disposed;

    /// <summary>
    /// The pattern string.
    /// </summary>
    public string Pattern => _patternString;

    /// <summary>
    /// The language this matcher is configured for.
    /// </summary>
    public string? Language => _language;

    /// <summary>
    /// Whether this is a UAST pattern (PascalCase) vs native (snake_case).
    /// </summary>
    public bool IsUastPattern => _isUastPattern;

    /// <summary>
    /// Creates a pattern matcher for the given pattern and language.
    /// </summary>
    /// <param name="pattern">Pattern string (UAST PascalCase or native snake_case).</param>
    /// <param name="language">Target language (e.g., "rust", "python").</param>
    /// <exception cref="UastException">If pattern compilation fails.</exception>
    public PatternMatcher(string pattern, string? language = null)
    {
        _patternString = pattern;
        _language = language ?? "";
        _isUastPattern = UastNativeBindings.IsUastPattern(pattern);

        if (!string.IsNullOrEmpty(_language))
        {
            _pattern = PatternHandle.Compile(pattern, _language);
        }
    }

    /// <summary>
    /// Find all matches in the given source code.
    /// </summary>
    /// <param name="source">Source code to search.</param>
    /// <param name="language">Language override (uses constructor language if null).</param>
    /// <returns>Enumerable of match results.</returns>
    public IEnumerable<MatchResult> Matches(string source, string? language = null)
    {
        var lang = language ?? _language;
        if (string.IsNullOrEmpty(lang))
        {
            throw new UastException(
                UastErrorCode.InternalError,
                "Language must be specified either in constructor or method call.");
        }

        // First parse to UAST JSON
        using var parser = new UastParser(lang);
        var json = parser.ParseToJson(source);
        if (json == null)
        {
            return [];
        }

        return MatchesJson(json, lang);
    }

    /// <summary>
    /// Find all matches in a parsed tree.
    /// </summary>
    /// <param name="tree">Parsed syntax tree.</param>
    /// <returns>Enumerable of match results.</returns>
    public IEnumerable<MatchResult> Matches(UastTree tree)
    {
        var json = GetTreeJson(tree);
        if (json == null)
        {
            return [];
        }

        return MatchesJson(json, tree.Language);
    }

    /// <summary>
    /// Find all matches in UAST JSON.
    /// </summary>
    /// <param name="uastJson">UAST JSON string.</param>
    /// <param name="language">Language of the source.</param>
    /// <returns>Enumerable of match results.</returns>
    public IEnumerable<MatchResult> MatchesJson(string uastJson, string language)
    {
        var result = UastNativeBindings.PatternMatch(
            _patternString, language, uastJson, out var matchesPtr);

        if (result != 0 || matchesPtr == IntPtr.Zero)
        {
            return [];
        }

        try
        {
            var matchesJson = Marshal.PtrToStringUTF8(matchesPtr);
            if (string.IsNullOrEmpty(matchesJson))
            {
                return [];
            }

            return JsonSerializer.Deserialize<List<MatchResult>>(matchesJson, JsonOptions) ?? [];
        }
        finally
        {
            UastNativeBindings.FreeString(matchesPtr);
        }
    }

    /// <summary>
    /// Get the count of matches without enumerating results.
    /// </summary>
    /// <param name="source">Source code to search.</param>
    /// <param name="language">Language override.</param>
    /// <returns>Number of matches.</returns>
    public int Count(string source, string? language = null) =>
        Matches(source, language).Count();

    /// <summary>
    /// Check if there are any matches.
    /// </summary>
    /// <param name="source">Source code to search.</param>
    /// <param name="language">Language override.</param>
    /// <returns>True if any matches exist.</returns>
    public bool Any(string source, string? language = null) =>
        Matches(source, language).Any();

    /// <summary>
    /// Compile the pattern to a tree-sitter query string.
    /// </summary>
    /// <param name="language">Language for the query.</param>
    /// <returns>Tree-sitter query string, or null on failure.</returns>
    public string? ToTreeSitterQuery(string? language = null)
    {
        var lang = language ?? _language;
        if (string.IsNullOrEmpty(lang))
        {
            return null;
        }

        var result = UastNativeBindings.PatternToQuery(_patternString, lang, out var queryPtr);
        if (result != 0 || queryPtr == IntPtr.Zero)
        {
            return null;
        }

        try
        {
            return Marshal.PtrToStringUTF8(queryPtr);
        }
        finally
        {
            UastNativeBindings.FreeString(queryPtr);
        }
    }

    /// <summary>
    /// Disposes resources.
    /// </summary>
    public void Dispose()
    {
        if (!_disposed)
        {
            _pattern?.Dispose();
            _disposed = true;
        }
    }

    // ========================================================================
    // Static Methods
    // ========================================================================

    /// <summary>
    /// Check if a pattern is a UAST pattern (PascalCase).
    /// </summary>
    /// <param name="pattern">Pattern to check.</param>
    /// <returns>True if it's a UAST pattern.</returns>
    public static bool IsUast(string pattern) =>
        UastNativeBindings.IsUastPattern(pattern);

    /// <summary>
    /// Get native tree-sitter types for a UAST kind.
    /// </summary>
    /// <param name="uastKind">UAST kind (e.g., "FunctionDeclaration").</param>
    /// <param name="language">Target language.</param>
    /// <returns>Array of native type names.</returns>
    public static string[] GetNativeTypes(string uastKind, string language) =>
        UastParser.GetNativeTypesForKind(uastKind, language);

    private static string? GetTreeJson(UastTree tree)
    {
        // Re-parse to get JSON
        using var parser = new UastParser(tree.Language);
        return parser.ParseToJson(tree.Source);
    }

    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        PropertyNameCaseInsensitive = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };
}
