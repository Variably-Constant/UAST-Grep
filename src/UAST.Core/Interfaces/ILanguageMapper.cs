using UAST.Core.Schema;

namespace UAST.Core.Interfaces;

/// <summary>
/// Interface for language-specific AST mappers.
/// Each supported language implements this interface to convert its native AST to UAST.
/// </summary>
public interface ILanguageMapper
{
    /// <summary>
    /// The name of the language this mapper handles.
    /// </summary>
    string LanguageName { get; }

    /// <summary>
    /// The file extensions this mapper handles (e.g., ".ps1", ".psm1").
    /// </summary>
    IReadOnlyList<string> FileExtensions { get; }

    /// <summary>
    /// Parses source code and returns a UAST tree.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <param name="filePath">Optional file path for error reporting.</param>
    /// <returns>The root of the UAST tree.</returns>
    UastNode Parse(string source, string? filePath = null);

    /// <summary>
    /// Parses a file and returns a UAST tree.
    /// </summary>
    /// <param name="filePath">The path to the file to parse.</param>
    /// <returns>The root of the UAST tree.</returns>
    UastNode ParseFile(string filePath);

    /// <summary>
    /// Gets any parse errors from the last parse operation.
    /// </summary>
    IReadOnlyList<ParseError> GetErrors();
}

/// <summary>
/// Represents a parse error.
/// </summary>
public record ParseError(
    string Message,
    SourceSpan Span,
    string? ErrorId = null,
    ParseErrorSeverity Severity = ParseErrorSeverity.Error
);

/// <summary>
/// Parse error severity levels.
/// </summary>
public enum ParseErrorSeverity
{
    Information,
    Warning,
    Error
}

/// <summary>
/// Extended language mapper interface that supports lazy/filtered conversion.
/// Mappers implementing this interface can skip converting nodes that don't match target kinds,
/// providing 3-5x speedup for pattern matching operations.
/// </summary>
public interface ILazyLanguageMapper : ILanguageMapper
{
    /// <summary>
    /// Parses source code lazily, only converting nodes matching target kinds.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <param name="targetKinds">UAST NodeKind values to look for (null means convert all).</param>
    /// <param name="filePath">Optional file path for error reporting.</param>
    /// <returns>A UAST tree with only relevant nodes fully converted.</returns>
    UastNode ParseLazy(string source, IReadOnlySet<string>? targetKinds, string? filePath = null);

    /// <summary>
    /// Parses a file lazily, only converting nodes matching target kinds.
    /// </summary>
    /// <param name="filePath">The path to the file to parse.</param>
    /// <param name="targetKinds">UAST NodeKind values to look for (null means convert all).</param>
    /// <returns>A UAST tree with only relevant nodes fully converted.</returns>
    UastNode ParseFileLazy(string filePath, IReadOnlySet<string>? targetKinds);

    /// <summary>
    /// Maps a native parser node type to the UAST NodeKind it would produce.
    /// Used to determine if a native node needs full conversion.
    /// </summary>
    /// <param name="nativeType">The native parser node type.</param>
    /// <returns>The corresponding UAST NodeKind, or null if not mappable.</returns>
    string? GetUastKindForNativeType(string nativeType);
}
