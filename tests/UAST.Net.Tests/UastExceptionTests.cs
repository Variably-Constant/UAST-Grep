using Xunit;

namespace UAST.Net.Tests;

/// <summary>
/// Tests for UastException.
/// </summary>
public class UastExceptionTests
{
    [Fact]
    public void Constructor_SetsErrorCodeAndMessage()
    {
        // Arrange & Act
        var ex = new UastException(UastErrorCode.ParseFailed, "Test message");

        // Assert
        Assert.Equal(UastErrorCode.ParseFailed, ex.ErrorCode);
        Assert.Equal("Test message", ex.Message);
    }

    [Fact]
    public void LibraryNotFound_ReturnsCorrectErrorCode()
    {
        // Arrange & Act
        var ex = UastException.LibraryNotFound();

        // Assert
        Assert.Equal(UastErrorCode.InternalError, ex.ErrorCode);
        Assert.Contains("uast_core", ex.Message);
    }

    [Fact]
    public void UnknownLanguage_IncludesLanguageName()
    {
        // Arrange & Act
        var ex = UastException.UnknownLanguage("cobol");

        // Assert
        Assert.Equal(UastErrorCode.UnknownLanguage, ex.ErrorCode);
        Assert.Contains("cobol", ex.Message);
    }

    [Fact]
    public void ParseFailed_WithDetails_IncludesDetails()
    {
        // Arrange & Act
        var ex = UastException.ParseFailed("Syntax error at line 5");

        // Assert
        Assert.Equal(UastErrorCode.ParseFailed, ex.ErrorCode);
        Assert.Contains("Syntax error at line 5", ex.Message);
    }

    [Fact]
    public void QueryFailed_IncludesPattern()
    {
        // Arrange & Act
        var ex = UastException.QueryFailed("(function_item)");

        // Assert
        Assert.Equal(UastErrorCode.QueryFailed, ex.ErrorCode);
        Assert.Contains("(function_item)", ex.Message);
    }
}
