using Xunit;

namespace UAST.Net.Tests;

/// <summary>
/// Tests for SourceSpan struct.
/// </summary>
public class SourceSpanTests
{
    [Fact]
    public void Length_CalculatesCorrectly()
    {
        // Arrange
        var span = new SourceSpan
        {
            StartOffset = 10,
            EndOffset = 25,
            StartLine = 1,
            EndLine = 1,
            StartColumn = 0,
            EndColumn = 15
        };

        // Act & Assert
        Assert.Equal(15, span.Length);
    }

    [Fact]
    public void ToString_FormatsCorrectly()
    {
        // Arrange
        var span = new SourceSpan
        {
            StartLine = 5,
            StartColumn = 10,
            EndLine = 5,
            EndColumn = 25
        };

        // Act
        var result = span.ToString();

        // Assert
        Assert.Equal("5:10-5:25", result);
    }

    [Fact]
    public void Equality_SameValues_AreEqual()
    {
        // Arrange
        var span1 = new SourceSpan
        {
            StartLine = 1,
            StartColumn = 0,
            EndLine = 1,
            EndColumn = 10,
            StartOffset = 0,
            EndOffset = 10
        };
        var span2 = new SourceSpan
        {
            StartLine = 1,
            StartColumn = 0,
            EndLine = 1,
            EndColumn = 10,
            StartOffset = 0,
            EndOffset = 10
        };

        // Act & Assert
        Assert.Equal(span1, span2);
    }

    [Fact]
    public void Equality_DifferentValues_AreNotEqual()
    {
        // Arrange
        var span1 = new SourceSpan { StartLine = 1, StartColumn = 0 };
        var span2 = new SourceSpan { StartLine = 2, StartColumn = 0 };

        // Act & Assert
        Assert.NotEqual(span1, span2);
    }
}
