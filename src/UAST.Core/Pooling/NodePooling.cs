using System.Buffers;
using System.Collections.Frozen;
using System.Runtime.CompilerServices;

namespace UAST.Core.Pooling;

/// <summary>
/// Provides object pooling and allocation optimizations for UAST node conversion.
/// Key optimizations:
/// - ArrayPool for children collections (avoid List allocations)
/// - String interning for NodeKind values (deduplicate strings)
/// - ReadOnlyMemory slicing for source text (avoid Substring allocations)
/// </summary>
public static class NodePooling
{
    #region String Interning for NodeKind

    /// <summary>
    /// Frozen set of all known UAST node kinds for fast O(1) lookups.
    /// These strings are interned to ensure reference equality.
    /// </summary>
    private static readonly FrozenSet<string> KnownNodeKinds = new HashSet<string>(StringComparer.Ordinal)
    {
        // Core nodes
        "BlockNode", "UnknownNode", "UnknownExpression",

        // Declarations
        "FunctionDeclaration", "TypeDeclaration", "VariableDeclaration",
        "ParameterNode", "AttributeNode", "DeclarationNode",

        // Statements
        "ExpressionStatement", "IfStatement", "WhileStatement", "ForStatement",
        "ForEachStatement", "TryStatement", "SwitchStatement", "ReturnStatement",
        "ThrowStatement", "BreakStatement", "ContinueStatement", "EmptyStatement",
        "UsingStatement", "LockStatement", "FixedStatement",
        "CheckedStatement", "UncheckedStatement", "YieldStatement", "AwaitStatement",
        "LocalDeclarationStatement", "SwitchCase", "CatchClause",

        // Expressions
        "LiteralExpression", "IdentifierExpression", "BinaryExpression",
        "UnaryExpression", "CallExpression", "MemberExpression", "IndexExpression",
        "AssignmentExpression", "ConditionalExpression", "LambdaExpression",
        "NewExpression", "ArrayExpression", "ObjectExpression", "TupleExpression",
        "InterpolatedStringExpression", "InterpolatedStringPart", "PropertyNode",
        "ParenthesizedExpression", "CastExpression", "TypeOfExpression",
        "DefaultExpression", "SizeOfExpression", "NameOfExpression",
        "ArgumentNode", "SpreadExpression",

        // Types
        "NamedTypeReference", "ArrayTypeReference", "GenericTypeReference",
        "NullableTypeReference", "TupleTypeReference", "FunctionTypeReference",
        "UnionTypeReference", "IntersectionTypeReference",

        // PowerShell extensions
        "PipelineExpression", "CommandExpression", "ScriptBlockExpression",
        "HashTableExpression", "ExpandableStringExpression", "VariableExpression",
        "MemberAccessExpression", "InvokeMemberExpression", "StatementBlockAst",

        // C# extensions
        "ObjectCreationExpression", "AnonymousObjectCreationExpression",
        "ArrayCreationExpression", "ImplicitArrayCreationExpression",
        "ElementAccessExpression", "InvocationExpression", "QueryExpression",
        "AwaitExpression", "ThrowExpression", "RefExpression",
        "RefTypeExpression", "MakeRefExpression", "RefValueExpression",
        "CheckedExpression", "UncheckedExpression", "DefaultValueExpression",
        "AnonymousMethodExpression",
        "ParenthesizedLambdaExpression", "SimpleLambdaExpression"
    }.ToFrozenSet(StringComparer.Ordinal);

    /// <summary>
    /// Maps native tree-sitter node types to UAST node kinds.
    /// Using FrozenDictionary for O(1) lookups with minimal memory overhead.
    /// </summary>
    private static readonly FrozenDictionary<string, string> NativeToUastKind = new Dictionary<string, string>(StringComparer.Ordinal)
    {
        // Python
        ["function_definition"] = "FunctionDeclaration",
        ["class_definition"] = "TypeDeclaration",
        ["if_statement"] = "IfStatement",
        ["while_statement"] = "WhileStatement",
        ["for_statement"] = "ForEachStatement",
        ["try_statement"] = "TryStatement",
        ["match_statement"] = "SwitchStatement",
        ["return_statement"] = "ReturnStatement",
        ["raise_statement"] = "ThrowStatement",
        ["pass_statement"] = "EmptyStatement",
        ["break_statement"] = "BreakStatement",
        ["continue_statement"] = "ContinueStatement",
        ["expression_statement"] = "ExpressionStatement",
        ["call"] = "CallExpression",
        ["attribute"] = "MemberExpression",
        ["subscript"] = "IndexExpression",
        ["binary_operator"] = "BinaryExpression",
        ["unary_operator"] = "UnaryExpression",
        ["assignment"] = "AssignmentExpression",
        ["identifier"] = "IdentifierExpression",
        ["string"] = "LiteralExpression",
        ["integer"] = "LiteralExpression",
        ["float"] = "LiteralExpression",
        ["list"] = "ArrayExpression",
        ["tuple"] = "TupleExpression",
        ["dictionary"] = "ObjectExpression",
        ["lambda"] = "LambdaExpression",
        ["block"] = "BlockNode",

        // JavaScript/TypeScript
        ["function_declaration"] = "FunctionDeclaration",
        ["arrow_function"] = "LambdaExpression",
        ["class_declaration"] = "TypeDeclaration",
        ["call_expression"] = "CallExpression",
        ["member_expression"] = "MemberExpression",
        ["subscript_expression"] = "IndexExpression",
        ["binary_expression"] = "BinaryExpression",
        ["unary_expression"] = "UnaryExpression",
        ["assignment_expression"] = "AssignmentExpression",
        ["new_expression"] = "NewExpression",
        ["array"] = "ArrayExpression",
        ["object"] = "ObjectExpression",

        // Java
        ["method_declaration"] = "FunctionDeclaration",
        ["interface_declaration"] = "TypeDeclaration",
        ["method_invocation"] = "CallExpression",
        ["field_access"] = "MemberExpression",
        ["array_access"] = "IndexExpression",
        ["object_creation_expression"] = "NewExpression",

        // Go
        ["method_declaration"] = "FunctionDeclaration",
        ["type_declaration"] = "TypeDeclaration",
        ["selector_expression"] = "MemberExpression",
        ["index_expression"] = "IndexExpression",

        // Rust
        ["function_item"] = "FunctionDeclaration",
        ["impl_item"] = "TypeDeclaration",
        ["struct_item"] = "TypeDeclaration",
        ["enum_item"] = "TypeDeclaration",
        ["field_expression"] = "MemberExpression",

        // C/C++
        ["struct_specifier"] = "TypeDeclaration",
        ["class_specifier"] = "TypeDeclaration"
    }.ToFrozenDictionary(StringComparer.Ordinal);

    // Cache for interned unknown kinds
    private static readonly Dictionary<string, string> _internedKinds = new(StringComparer.Ordinal);
    private static readonly object _internLock = new();

    /// <summary>
    /// Gets the interned NodeKind string for a given kind.
    /// Returns an interned version of the string, avoiding duplicate allocations.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string InternNodeKind(string kind)
    {
        // Fast path: check if it's a known kind (already interned in the set)
        if (KnownNodeKinds.Contains(kind))
        {
            // The FrozenSet contains the interned version
            foreach (var known in KnownNodeKinds)
            {
                if (string.Equals(known, kind, StringComparison.Ordinal))
                    return known;
            }
        }

        // Check cache for previously interned unknowns
        lock (_internLock)
        {
            if (_internedKinds.TryGetValue(kind, out var cached))
                return cached;

            // Intern and cache
            var interned = string.Intern(kind);
            _internedKinds[interned] = interned;
            return interned;
        }
    }

    /// <summary>
    /// Maps a native tree-sitter node type to the corresponding UAST NodeKind.
    /// Returns null if no mapping exists.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string? MapNativeToUastKind(string nativeType)
    {
        return NativeToUastKind.GetValueOrDefault(nativeType);
    }

    #endregion

    #region ArrayPool for Children Collections

    /// <summary>
    /// Shared ArrayPool for UastNode children collections.
    /// Dramatically reduces allocations for temporary child lists during conversion.
    /// </summary>
    public static readonly ArrayPool<Schema.UastNode> NodeArrayPool = ArrayPool<Schema.UastNode>.Create(
        maxArrayLength: 1024,  // Most nodes have < 1024 children
        maxArraysPerBucket: 50 // Keep 50 arrays per size bucket
    );

    /// <summary>
    /// Rents a node array from the pool.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Schema.UastNode[] RentNodeArray(int minimumLength)
    {
        return NodeArrayPool.Rent(minimumLength);
    }

    /// <summary>
    /// Returns a node array to the pool.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void ReturnNodeArray(Schema.UastNode[] array, bool clearArray = true)
    {
        NodeArrayPool.Return(array, clearArray);
    }

    #endregion
}

/// <summary>
/// A pooled list wrapper for UastNode children that uses ArrayPool.
/// Provides List-like semantics with dramatically reduced allocations.
/// </summary>
public sealed class PooledChildrenList : IDisposable
{
    private Schema.UastNode[] _items;
    private int _count;
    private bool _disposed;

    public PooledChildrenList(int initialCapacity = 8)
    {
        _items = NodePooling.RentNodeArray(Math.Max(4, initialCapacity));
        _count = 0;
    }

    public int Count => _count;

    public Schema.UastNode this[int index]
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if ((uint)index >= (uint)_count)
                ThrowIndexOutOfRange();
            return _items[index];
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Add(Schema.UastNode node)
    {
        if (_count == _items.Length)
            Grow();
        _items[_count++] = node;
    }

    private void Grow()
    {
        var newArray = NodePooling.RentNodeArray(_items.Length * 2);
        Array.Copy(_items, newArray, _count);
        NodePooling.ReturnNodeArray(_items, clearArray: true);
        _items = newArray;
    }

    /// <summary>
    /// Creates a copy of the items as a regular List for permanent storage.
    /// Call this before disposing if you need to keep the items.
    /// </summary>
    public List<Schema.UastNode> ToList()
    {
        var list = new List<Schema.UastNode>(_count);
        for (int i = 0; i < _count; i++)
            list.Add(_items[i]);
        return list;
    }

    /// <summary>
    /// Creates a read-only list wrapper around the items.
    /// Only valid until Dispose is called.
    /// </summary>
    public IReadOnlyList<Schema.UastNode> AsReadOnly() => new ArraySegment<Schema.UastNode>(_items, 0, _count);

    public void Dispose()
    {
        if (!_disposed)
        {
            _disposed = true;
            NodePooling.ReturnNodeArray(_items, clearArray: true);
            _items = null!;
        }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void ThrowIndexOutOfRange() => throw new IndexOutOfRangeException();
}
