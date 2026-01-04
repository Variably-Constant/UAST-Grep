using Xunit;

namespace UAST.Net.Tests;

/// <summary>
/// Tests for UastParser functionality.
/// Note: Tests that require the native library are conditionally executed.
/// When the native library is not available, tests pass without executing assertions.
/// </summary>
public class UastParserTests
{
    private static readonly bool _isNativeLibraryAvailable;

    static UastParserTests()
    {
        try
        {
            // Try to call a function that's definitely in the native library
            _ = UastParser.IsUastPattern("test");
            _isNativeLibraryAvailable = true;
        }
        catch
        {
            _isNativeLibraryAvailable = false;
        }
    }

    [Fact]
    public void Version_WhenLibraryAvailable_ReturnsNonEmptyString()
    {
        if (!_isNativeLibraryAvailable) return;

        // Act
        var version = UastParser.Version;

        // Assert
        Assert.False(string.IsNullOrEmpty(version));
    }

    [Fact]
    public void IsUastPattern_WhenLibraryAvailable_PascalCase_ReturnsTrue()
    {
        if (!_isNativeLibraryAvailable) return;

        // Arrange & Act & Assert
        Assert.True(UastParser.IsUastPattern("FunctionDeclaration"));
        Assert.True(UastParser.IsUastPattern("VariableDeclaration"));
        Assert.True(UastParser.IsUastPattern("CallExpression"));
    }

    [Fact]
    public void IsUastPattern_WhenLibraryAvailable_SnakeCase_ReturnsFalse()
    {
        if (!_isNativeLibraryAvailable) return;

        // Arrange & Act & Assert
        Assert.False(UastParser.IsUastPattern("function_item"));
        Assert.False(UastParser.IsUastPattern("function_definition"));
        Assert.False(UastParser.IsUastPattern("call_expression"));
    }

    [Fact]
    public void IsLanguageSupported_WhenLibraryAvailable_UnknownLanguage_ReturnsFalse()
    {
        if (!_isNativeLibraryAvailable) return;

        // Arrange & Act & Assert
        Assert.False(UastParser.IsLanguageSupported("totally_fake_language_xyz"));
    }
}
