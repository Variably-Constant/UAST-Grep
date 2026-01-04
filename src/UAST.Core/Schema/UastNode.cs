using System.Runtime.CompilerServices;
using System.Text.Json.Serialization;

namespace UAST.Core.Schema;

/// <summary>
/// Base class for all Unified AST nodes.
/// Every node inherits from this base and provides identity, position, and tree structure.
///
/// Performance optimizations:
/// - RawSourceMemory uses ReadOnlyMemory to avoid Substring allocations
/// - NodeKind strings are interned to reduce memory usage
/// - Lazy children enumeration for filtering scenarios
/// </summary>
public abstract class UastNode
{
    private string? _rawSource;
    private ReadOnlyMemory<char>? _rawSourceMemory;

    /// <summary>
    /// The kind of node (e.g., "FunctionDeclaration", "BinaryExpression").
    /// Should be an interned string for memory efficiency.
    /// </summary>
    public required string NodeKind { get; init; }

    /// <summary>
    /// The source language this node came from (e.g., "PowerShell", "CSharp").
    /// </summary>
    public required string Language { get; init; }

    /// <summary>
    /// The source location of this node.
    /// </summary>
    public required SourceSpan Span { get; init; }

    /// <summary>
    /// Parent node in the tree. Set during tree construction.
    /// </summary>
    [JsonIgnore]
    public UastNode? Parent { get; internal set; }

    /// <summary>
    /// Language-specific extensions that don't fit the unified schema.
    /// </summary>
    public Dictionary<string, object>? Extensions { get; init; }

    /// <summary>
    /// Gets all direct children of this node.
    /// </summary>
    [JsonIgnore]
    public IReadOnlyList<UastNode> Children => GetChildren();

    /// <summary>
    /// Gets the raw source text of this node if available.
    /// For string-based source access. Use RawSourceMemory for zero-allocation access.
    /// </summary>
    public string? RawSource
    {
        get => _rawSource ?? (_rawSourceMemory.HasValue ? _rawSourceMemory.Value.ToString() : null);
        init => _rawSource = value;
    }

    /// <summary>
    /// Gets the raw source text as ReadOnlyMemory for zero-allocation slicing.
    /// This is the preferred way to access source text when performing analysis.
    /// </summary>
    [JsonIgnore]
    public ReadOnlyMemory<char> RawSourceMemory
    {
        get => _rawSourceMemory ?? (_rawSource?.AsMemory() ?? ReadOnlyMemory<char>.Empty);
        init => _rawSourceMemory = value;
    }

    /// <summary>
    /// Gets the raw source as a ReadOnlySpan for efficient string operations.
    /// </summary>
    [JsonIgnore]
    public ReadOnlySpan<char> RawSourceSpan
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => RawSourceMemory.Span;
    }

    /// <summary>
    /// Derived classes implement this to enumerate their children.
    /// </summary>
    protected abstract IReadOnlyList<UastNode> GetChildren();

    /// <summary>
    /// Sets parent references for all children.
    /// Called after tree construction.
    /// </summary>
    public void SetParentReferences()
    {
        foreach (var child in Children)
        {
            child.Parent = this;
            child.SetParentReferences();
        }
    }

    /// <summary>
    /// Gets an ancestor of a specific type.
    /// </summary>
    public T? GetAncestor<T>() where T : UastNode
    {
        var current = Parent;
        while (current != null)
        {
            if (current is T typed)
                return typed;
            current = current.Parent;
        }
        return null;
    }

    /// <summary>
    /// Traverses all descendants using depth-first pre-order traversal.
    /// </summary>
    public IEnumerable<UastNode> DescendantsAndSelf()
    {
        yield return this;
        foreach (var child in Children)
        {
            foreach (var descendant in child.DescendantsAndSelf())
            {
                yield return descendant;
            }
        }
    }

    /// <summary>
    /// Traverses all descendants.
    /// </summary>
    public IEnumerable<UastNode> Descendants()
    {
        foreach (var child in Children)
        {
            foreach (var descendant in child.DescendantsAndSelf())
            {
                yield return descendant;
            }
        }
    }

    /// <summary>
    /// Finds all descendants of a specific type.
    /// </summary>
    public IEnumerable<T> DescendantsOfType<T>() where T : UastNode
    {
        return Descendants().OfType<T>();
    }

    public override string ToString() => $"{NodeKind} @ {Span}";
}

/// <summary>
/// Represents an unknown or unmappable node from the source language.
/// Used as a fallback when a language construct cannot map to a standard UAST node.
/// </summary>
public class UnknownNode : UastNode
{
    /// <summary>
    /// The native node type name from the source language's AST.
    /// </summary>
    public required string NativeNodeType { get; init; }

    /// <summary>
    /// Semantic roles that describe this node's purpose (e.g., "loop", "conditional").
    /// Allows pattern matching to work even on unknown structures.
    /// </summary>
    public IReadOnlyList<string> Roles { get; init; } = [];

    /// <summary>
    /// Child nodes that were successfully mapped.
    /// </summary>
    public IReadOnlyList<UastNode> ChildNodes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() => ChildNodes;
}
