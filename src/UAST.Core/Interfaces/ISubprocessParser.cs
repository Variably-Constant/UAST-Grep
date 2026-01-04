using UAST.Core.Schema;

namespace UAST.Core.Interfaces;

/// <summary>
/// Interface for parsers that use subprocesses.
/// These parsers invoke external processes (e.g., pwsh, python) to parse source code
/// and return JSON AST that is then converted to UAST.
/// </summary>
public interface ISubprocessParser
{
    /// <summary>
    /// The language name.
    /// </summary>
    string LanguageName { get; }

    /// <summary>
    /// File extensions handled by this parser.
    /// </summary>
    IReadOnlyList<string> FileExtensions { get; }

    /// <summary>
    /// The command to invoke the parser (e.g., "pwsh", "python").
    /// </summary>
    string ParserCommand { get; }

    /// <summary>
    /// Arguments to pass to the parser command.
    /// </summary>
    IReadOnlyList<string> ParserArguments { get; }

    /// <summary>
    /// Parses source code via subprocess and returns JSON AST.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <param name="filePath">Optional file path for error reporting.</param>
    /// <returns>JSON representation of the AST.</returns>
    Task<string> ParseToJsonAsync(string source, string? filePath = null);

    /// <summary>
    /// Converts JSON AST to UAST.
    /// </summary>
    /// <param name="jsonAst">The JSON AST from the subprocess.</param>
    /// <returns>The root of the UAST tree.</returns>
    UastNode ConvertFromJson(string jsonAst);
}
