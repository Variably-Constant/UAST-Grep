using System.Runtime.InteropServices;
using System.Text.Json;
using UAST.Net.Native;

namespace UAST.Net;

/// <summary>
/// High-level parser for source code using UAST-Grep's Rust backend.
/// Provides cross-language AST parsing with 70+ language support.
/// </summary>
/// <remarks>
/// This class wraps the native uast_core library and provides a clean .NET API.
/// Create an instance for a specific language and reuse it for parsing multiple files.
/// </remarks>
/// <example>
/// <code>
/// using var parser = new UastParser("rust");
/// using var tree = parser.Parse("fn main() { println!(\"Hello\"); }");
/// Console.WriteLine($"Nodes: {tree.NodeCount}");
/// </code>
/// </example>
public sealed class UastParser : IDisposable
{
    private readonly ParserHandle _parser;
    private readonly string _language;
    private bool _disposed;

    /// <summary>
    /// The language this parser is configured for.
    /// </summary>
    public string Language => _language;

    /// <summary>
    /// Creates a parser for the specified language.
    /// </summary>
    /// <param name="language">Language name (e.g., "rust", "python", "javascript")</param>
    /// <exception cref="UastException">If the language is not registered.</exception>
    public UastParser(string language)
    {
        _parser = ParserHandle.Create(language);
        _language = language;
    }

    /// <summary>
    /// Parse source code and return a syntax tree.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <returns>A parsed syntax tree. Dispose when done.</returns>
    /// <exception cref="ObjectDisposedException">If the parser has been disposed.</exception>
    /// <exception cref="UastException">If parsing fails.</exception>
    public UastTree Parse(string source)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        var tree = TreeHandle.Parse(_parser, source);
        return new UastTree(tree, source, _language);
    }

    /// <summary>
    /// Parse source code and return JSON representation.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <param name="sourcePath">Optional source file path for error messages.</param>
    /// <returns>JSON string representing the UAST, or null on failure.</returns>
    public string? ParseToJson(string source, string? sourcePath = null)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        var result = UastNativeBindings.ParseUast(_language, source, sourcePath, out var jsonPtr);

        if (result != 0 || jsonPtr == IntPtr.Zero)
            return null;

        try
        {
            return Marshal.PtrToStringUTF8(jsonPtr);
        }
        finally
        {
            UastNativeBindings.FreeJson(jsonPtr);
        }
    }

    /// <summary>
    /// Parse source code to typed UAST JSON with options.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <param name="sourcePath">Optional source file path.</param>
    /// <param name="includeAllText">Include source text for all nodes (not just leaves).</param>
    /// <param name="includeNativeTypes">Include native tree-sitter type names.</param>
    /// <returns>JSON string representing the typed UAST document.</returns>
    public string? ParseToTypedJson(
        string source,
        string? sourcePath = null,
        bool includeAllText = false,
        bool includeNativeTypes = false)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        var result = UastNativeBindings.ParseToTypedJson(
            _language, source, sourcePath,
            includeAllText, includeNativeTypes,
            out var jsonPtr);

        if (result != 0 || jsonPtr == IntPtr.Zero)
            return null;

        try
        {
            return Marshal.PtrToStringUTF8(jsonPtr);
        }
        finally
        {
            UastNativeBindings.FreeJson(jsonPtr);
        }
    }

    /// <summary>
    /// Disposes the parser and frees native resources.
    /// </summary>
    public void Dispose()
    {
        if (!_disposed)
        {
            _parser.Dispose();
            _disposed = true;
        }
    }

    // ========================================================================
    // Static Methods
    // ========================================================================

    /// <summary>
    /// Get the version of the native library.
    /// </summary>
    public static string Version
    {
        get
        {
            var ptr = UastNativeBindings.Version();
            return Marshal.PtrToStringUTF8(ptr) ?? "unknown";
        }
    }

    /// <summary>
    /// Get the number of registered languages.
    /// </summary>
    public static int LanguageCount => (int)UastNativeBindings.LanguageCount();

    /// <summary>
    /// Check if a language is registered and available.
    /// </summary>
    /// <param name="language">Language name to check.</param>
    /// <returns>True if the language is available.</returns>
    public static bool IsLanguageSupported(string language) =>
        UastNativeBindings.HasLanguage(language);

    /// <summary>
    /// Register a language from a raw grammar pointer.
    /// </summary>
    /// <param name="name">Language name (e.g., "kotlin").</param>
    /// <param name="languagePtr">TSLanguage* pointer from a grammar DLL.</param>
    /// <returns>True if registration succeeded.</returns>
    public static bool RegisterLanguage(string name, IntPtr languagePtr)
    {
        var result = UastNativeBindings.RegisterLanguage(name, languagePtr);
        return result == UastNativeBindings.UastResult.Ok;
    }

    /// <summary>
    /// Unregister a language.
    /// </summary>
    /// <param name="name">Language name to unregister.</param>
    /// <returns>True if the language was found and removed.</returns>
    public static bool UnregisterLanguage(string name) =>
        UastNativeBindings.UnregisterLanguage(name);

    /// <summary>
    /// Create a parser for a file extension.
    /// </summary>
    /// <param name="extension">File extension (e.g., ".rs", ".py").</param>
    /// <returns>A parser for the language associated with the extension.</returns>
    /// <exception cref="UastException">If no language is registered for the extension.</exception>
    public static UastParser ForExtension(string extension)
    {
        var parser = ParserHandle.CreateForExtension(extension);
        return new UastParser(parser, extension);
    }

    /// <summary>
    /// Check if a pattern is a UAST pattern (PascalCase) vs native (snake_case).
    /// </summary>
    /// <param name="pattern">Pattern string to check.</param>
    /// <returns>True if the pattern appears to be a UAST type.</returns>
    public static bool IsUastPattern(string pattern) =>
        UastNativeBindings.IsUastPattern(pattern);

    /// <summary>
    /// Get native tree-sitter types for a UAST kind.
    /// </summary>
    /// <param name="uastKind">UAST kind name (e.g., "FunctionDeclaration").</param>
    /// <param name="language">Language name.</param>
    /// <returns>Array of native type names.</returns>
    public static string[] GetNativeTypesForKind(string uastKind, string language)
    {
        var result = UastNativeBindings.GetNativeTypesForKind(language, uastKind, out var typesPtr);
        if (result != 0 || typesPtr == IntPtr.Zero)
            return [];

        try
        {
            var typesStr = Marshal.PtrToStringUTF8(typesPtr);
            if (string.IsNullOrEmpty(typesStr))
                return [];

            return typesStr.Split(',', StringSplitOptions.RemoveEmptyEntries);
        }
        finally
        {
            UastNativeBindings.FreeString(typesPtr);
        }
    }

    // Private constructor for extension-based creation
    private UastParser(ParserHandle parser, string language)
    {
        _parser = parser;
        _language = language;
    }
}
