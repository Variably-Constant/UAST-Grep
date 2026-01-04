namespace UAST.Net;

/// <summary>
/// Error codes for UAST operations.
/// </summary>
public enum UastErrorCode
{
    /// <summary>Operation succeeded.</summary>
    Ok = 0,

    /// <summary>A null pointer was passed where non-null was expected.</summary>
    NullPointer = 1,

    /// <summary>Invalid UTF-8 in a string argument.</summary>
    InvalidUtf8 = 2,

    /// <summary>Unknown or unsupported language.</summary>
    UnknownLanguage = 3,

    /// <summary>Parse operation failed.</summary>
    ParseFailed = 4,

    /// <summary>Query compilation or execution failed.</summary>
    QueryFailed = 5,

    /// <summary>Internal error in the native library.</summary>
    InternalError = 99
}

/// <summary>
/// Exception thrown when a UAST operation fails.
/// </summary>
public class UastException : Exception
{
    /// <summary>
    /// The error code from the native library.
    /// </summary>
    public UastErrorCode ErrorCode { get; }

    /// <summary>
    /// Creates a new UastException.
    /// </summary>
    public UastException(UastErrorCode errorCode, string message)
        : base(message)
    {
        ErrorCode = errorCode;
    }

    /// <summary>
    /// Creates a new UastException with an inner exception.
    /// </summary>
    public UastException(UastErrorCode errorCode, string message, Exception innerException)
        : base(message, innerException)
    {
        ErrorCode = errorCode;
    }

    /// <summary>
    /// Creates an exception for when the native library is not found.
    /// </summary>
    public static UastException LibraryNotFound() => new(
        UastErrorCode.InternalError,
        "The uast_core native library could not be found. " +
        "Ensure that uast_core.dll is in the application directory or in the " +
        "runtimes/win-x64/native folder.");

    /// <summary>
    /// Creates an exception for an unknown language.
    /// </summary>
    public static UastException UnknownLanguage(string language) => new(
        UastErrorCode.UnknownLanguage,
        $"Unknown language: {language}. " +
        "Register the language first using UastParser.RegisterLanguage() or use a built-in language.");

    /// <summary>
    /// Creates an exception for a parse failure.
    /// </summary>
    public static UastException ParseFailed(string? details = null) => new(
        UastErrorCode.ParseFailed,
        details ?? "Failed to parse source code.");

    /// <summary>
    /// Creates an exception for a query failure.
    /// </summary>
    public static UastException QueryFailed(string pattern, string? details = null) => new(
        UastErrorCode.QueryFailed,
        $"Failed to compile or execute query pattern: {pattern}" +
        (details != null ? $". {details}" : ""));
}
