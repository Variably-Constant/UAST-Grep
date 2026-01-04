using UAST.Core.Schema.Statements;

namespace UAST.Core.Schema.Declarations;

/// <summary>
/// Base class for all declaration nodes.
/// Extends StatementNode because declarations can appear as statements in most languages.
/// </summary>
public abstract class DeclarationNode : StatementNode
{
    /// <summary>
    /// The name of the declared entity.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// Visibility modifier (public, private, etc.).
    /// </summary>
    public Visibility Visibility { get; init; } = Visibility.Default;

    /// <summary>
    /// Attributes/decorators applied to this declaration.
    /// </summary>
    public IReadOnlyList<AttributeNode> Attributes { get; init; } = [];

    /// <summary>
    /// Modifiers (static, abstract, etc.).
    /// </summary>
    public IReadOnlyList<ModifierNode> Modifiers { get; init; } = [];
}

/// <summary>
/// Represents an attribute/decorator applied to a declaration.
/// </summary>
public class AttributeNode : UastNode
{
    /// <summary>
    /// The name of the attribute.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// Arguments passed to the attribute.
    /// </summary>
    public IReadOnlyList<Expressions.ArgumentNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Arguments.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a modifier keyword (static, abstract, readonly, etc.).
/// </summary>
public class ModifierNode : UastNode
{
    /// <summary>
    /// The modifier keyword.
    /// </summary>
    public required string Keyword { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}
