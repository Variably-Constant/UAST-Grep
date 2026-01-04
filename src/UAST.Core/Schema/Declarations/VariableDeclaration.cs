using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Declarations;

/// <summary>
/// Represents a variable, field, or property declaration.
/// Maps to: PowerShell AssignmentStatementAst (for declarations), C# FieldDeclarationSyntax, etc.
/// </summary>
public class VariableDeclaration : DeclarationNode
{
    /// <summary>
    /// The declared type (if any).
    /// </summary>
    public TypeReference? Type { get; init; }

    /// <summary>
    /// The initial value (if any).
    /// </summary>
    public ExpressionNode? Initializer { get; init; }

    /// <summary>
    /// The kind of variable (local, field, property, constant).
    /// </summary>
    public VariableKind Kind { get; init; }

    /// <summary>
    /// Whether this is a readonly/const variable.
    /// </summary>
    public bool IsReadOnly { get; init; }

    /// <summary>
    /// Destructuring pattern for this variable (if any).
    /// Used for languages like Swift/JavaScript with destructuring:
    /// let (first, _, _) = tuple  // Pattern contains wildcard patterns
    /// </summary>
    public UastNode? DestructuringPattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        if (DestructuringPattern != null) children.Add(DestructuringPattern);
        if (Initializer != null) children.Add(Initializer);
        return children;
    }
}

/// <summary>
/// Represents a property declaration with accessors.
/// </summary>
public class PropertyDeclaration : DeclarationNode
{
    /// <summary>
    /// The property type.
    /// </summary>
    public TypeReference? Type { get; init; }

    /// <summary>
    /// The getter body (if any).
    /// </summary>
    public Statements.BlockNode? Getter { get; init; }

    /// <summary>
    /// The setter body (if any).
    /// </summary>
    public Statements.BlockNode? Setter { get; init; }

    /// <summary>
    /// The initial value (if any).
    /// </summary>
    public ExpressionNode? Initializer { get; init; }

    /// <summary>
    /// Whether this is an auto-property.
    /// </summary>
    public bool IsAuto { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        if (Getter != null) children.Add(Getter);
        if (Setter != null) children.Add(Setter);
        if (Initializer != null) children.Add(Initializer);
        return children;
    }
}

/// <summary>
/// Represents a namespace/module declaration.
/// </summary>
public class NamespaceDeclaration : DeclarationNode
{
    /// <summary>
    /// Members of this namespace.
    /// </summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Members.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents an import/using statement.
/// </summary>
public class ImportDeclaration : UastNode
{
    /// <summary>
    /// The module/namespace being imported.
    /// </summary>
    public required string ModuleName { get; init; }

    /// <summary>
    /// Specific items being imported (for selective imports).
    /// </summary>
    public IReadOnlyList<string> ImportedNames { get; init; } = [];

    /// <summary>
    /// Alias for the import (if any).
    /// </summary>
    public string? Alias { get; init; }

    /// <summary>
    /// Whether this imports all exports (*).
    /// </summary>
    public bool IsWildcard { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}
