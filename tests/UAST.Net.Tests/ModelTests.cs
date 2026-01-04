using System.Text.Json;
using Xunit;

namespace UAST.Net.Tests;

/// <summary>
/// Tests for model serialization/deserialization.
/// </summary>
public class ModelTests
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        PropertyNameCaseInsensitive = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };

    [Fact]
    public void MatchResult_DeserializesFromJson()
    {
        // Arrange
        var json = """
            {
                "nodeKind": "FunctionDeclaration",
                "span": {
                    "startLine": 1,
                    "startColumn": 0,
                    "endLine": 3,
                    "endColumn": 1
                },
                "captures": {
                    "NAME": {
                        "count": 1,
                        "nodes": [
                            { "kind": "identifier", "text": "myFunc", "name": "myFunc" }
                        ]
                    }
                }
            }
            """;

        // Act
        var result = JsonSerializer.Deserialize<MatchResult>(json, JsonOptions);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("FunctionDeclaration", result.NodeKind);
        Assert.Equal(1, result.Span.StartLine);
        Assert.Equal(3, result.Span.EndLine);
        Assert.True(result.Captures.ContainsKey("NAME"));
        Assert.Equal(1, result.Captures["NAME"].Count);
        Assert.Equal("myFunc", result.Captures["NAME"].Nodes[0].Text);
    }

    [Fact]
    public void ScanResult_DeserializesFromJson()
    {
        // Arrange
        var json = """
            {
                "rule_id": "no-empty-function",
                "severity": "warning",
                "message": "Empty function detected",
                "location": {
                    "startLine": 10,
                    "startColumn": 0,
                    "endLine": 10,
                    "endColumn": 20,
                    "startOffset": 100,
                    "endOffset": 120
                },
                "file_path": "/path/to/file.rs",
                "captures": {}
            }
            """;

        // Act
        var result = JsonSerializer.Deserialize<ScanResult>(json, JsonOptions);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("no-empty-function", result.RuleId);
        Assert.Equal("warning", result.Severity);
        Assert.Equal("Empty function detected", result.Message);
        Assert.Equal(10, result.Location.StartLine);
        Assert.Equal("/path/to/file.rs", result.FilePath);
    }

    [Fact]
    public void Severity_ToLevel_ReturnsCorrectValues()
    {
        Assert.Equal(4, Severity.ToLevel("error"));
        Assert.Equal(3, Severity.ToLevel("warning"));
        Assert.Equal(2, Severity.ToLevel("info"));
        Assert.Equal(1, Severity.ToLevel("hint"));
        Assert.Equal(0, Severity.ToLevel("unknown"));
    }

    [Fact]
    public void Severity_ToLevel_IsCaseInsensitive()
    {
        Assert.Equal(4, Severity.ToLevel("ERROR"));
        Assert.Equal(3, Severity.ToLevel("Warning"));
        Assert.Equal(2, Severity.ToLevel("INFO"));
    }

    [Fact]
    public void ScanLocation_ToSourceSpan_ConvertsCorrectly()
    {
        // Arrange
        var location = new ScanLocation
        {
            StartLine = 5,
            StartColumn = 10,
            EndLine = 5,
            EndColumn = 25,
            StartOffset = 100,
            EndOffset = 115
        };

        // Act
        var span = location.ToSourceSpan();

        // Assert
        Assert.Equal(5, span.StartLine);
        Assert.Equal(10, span.StartColumn);
        Assert.Equal(5, span.EndLine);
        Assert.Equal(25, span.EndColumn);
        Assert.Equal(100, span.StartOffset);
        Assert.Equal(115, span.EndOffset);
    }

    [Fact]
    public void ScanFix_DeserializesFromJson()
    {
        // Arrange
        var json = """
            {
                "replacement": "Write-Output",
                "start_byte": 50,
                "end_byte": 60
            }
            """;

        // Act
        var fix = JsonSerializer.Deserialize<ScanFix>(json, JsonOptions);

        // Assert
        Assert.NotNull(fix);
        Assert.Equal("Write-Output", fix.Replacement);
        Assert.Equal(50, fix.StartByte);
        Assert.Equal(60, fix.EndByte);
    }
}
