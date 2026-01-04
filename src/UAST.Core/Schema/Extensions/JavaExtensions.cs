using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Java Record Types

/// <summary>
/// Java record declaration: record Point(int x, int y) { }
/// Records are immutable data carriers with auto-generated equals, hashCode, toString.
/// </summary>
public class JavaRecordDeclaration : TypeDeclaration
{
    /// <summary>Record components (the canonical constructor parameters)</summary>
    public IReadOnlyList<JavaRecordComponent> Components { get; init; } = [];

    /// <summary>Implemented interfaces</summary>
    public IReadOnlyList<TypeReference> Implements { get; init; } = [];

    /// <summary>Whether this record has a compact canonical constructor</summary>
    public bool HasCompactConstructor { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Components);
        children.AddRange(Implements);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// A component in a Java record declaration: record Point(int x, int y)
/// </summary>
public class JavaRecordComponent : UastNode
{
    /// <summary>The component name</summary>
    public required string Name { get; init; }

    /// <summary>The component type</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Annotations on this component</summary>
    public IReadOnlyList<AttributeNode> Annotations { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Annotations);
        children.Add(Type);
        return children;
    }
}

#endregion

#region Java Sealed Classes

/// <summary>
/// Java sealed class/interface declaration with permits clause.
/// </summary>
public class JavaSealedTypeDeclaration : TypeDeclaration
{
    /// <summary>Whether this type is sealed</summary>
    public bool IsSealed { get; init; }

    /// <summary>Whether this type is non-sealed</summary>
    public bool IsNonSealed { get; init; }

    /// <summary>Whether this type is final (leaf of sealed hierarchy)</summary>
    public bool IsFinal { get; init; }

    /// <summary>Permitted subtypes (from permits clause)</summary>
    public IReadOnlyList<TypeReference> PermittedSubtypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        children.AddRange(PermittedSubtypes);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Java Pattern Matching

/// <summary>
/// Java instanceof pattern expression: if (obj instanceof String s)
/// </summary>
public class JavaInstanceofPatternExpression : ExpressionNode
{
    /// <summary>The expression being checked</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The pattern to match against</summary>
    public required JavaPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Pattern];
}

/// <summary>
/// Base class for Java patterns (used in instanceof, switch).
/// </summary>
public abstract class JavaPattern : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Java type pattern: String s, Integer i
/// </summary>
public class JavaTypePattern : JavaPattern
{
    /// <summary>The type to match</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The binding variable name</summary>
    public required string VariableName { get; init; }

    /// <summary>Whether this is a final binding</summary>
    public bool IsFinal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Java record pattern: Point(int x, int y)
/// </summary>
public class JavaRecordPattern : JavaPattern
{
    /// <summary>The record type</summary>
    public required TypeReference RecordType { get; init; }

    /// <summary>Nested patterns for record components</summary>
    public IReadOnlyList<JavaPattern> ComponentPatterns { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { RecordType };
        children.AddRange(ComponentPatterns);
        return children;
    }
}

/// <summary>
/// Java guarded pattern: pattern when condition
/// </summary>
public class JavaGuardedPattern : JavaPattern
{
    /// <summary>The inner pattern</summary>
    public required JavaPattern Pattern { get; init; }

    /// <summary>The guard condition</summary>
    public required ExpressionNode Guard { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Guard];
}

/// <summary>
/// Java null pattern in switch: case null
/// </summary>
public class JavaNullPattern : JavaPattern
{
}

/// <summary>
/// Java default pattern in switch: case default
/// </summary>
public class JavaDefaultPattern : JavaPattern
{
}

#endregion

#region Java Switch Expressions

/// <summary>
/// Java switch expression (yields a value): var x = switch(y) { case 1 -> "one"; }
/// </summary>
public class JavaSwitchExpression : ExpressionNode
{
    /// <summary>The selector expression</summary>
    public required ExpressionNode Selector { get; init; }

    /// <summary>The switch arms/rules</summary>
    public IReadOnlyList<JavaSwitchRule> Rules { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Selector };
        children.AddRange(Rules);
        return children;
    }
}

/// <summary>
/// A single rule in a Java switch expression: case PATTERN -> EXPR
/// </summary>
public class JavaSwitchRule : UastNode
{
    /// <summary>The case labels/patterns</summary>
    public IReadOnlyList<JavaSwitchLabel> Labels { get; init; } = [];

    /// <summary>The body (expression, block, or throw)</summary>
    public required UastNode Body { get; init; }

    /// <summary>Whether this uses arrow syntax (->) vs colon (:)</summary>
    public bool IsArrowRule { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Labels);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// A switch case label in Java (pattern or constant).
/// </summary>
public class JavaSwitchLabel : UastNode
{
    /// <summary>The pattern if this is a pattern case</summary>
    public JavaPattern? Pattern { get; init; }

    /// <summary>The constant expression if this is a constant case</summary>
    public ExpressionNode? ConstantExpression { get; init; }

    /// <summary>Whether this is a default label</summary>
    public bool IsDefault { get; init; }

    /// <summary>Whether this is a null case</summary>
    public bool IsNull { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        if (Pattern != null) return [Pattern];
        if (ConstantExpression != null) return [ConstantExpression];
        return [];
    }
}

/// <summary>
/// Java yield statement in switch expression: yield value;
/// </summary>
public class JavaYieldStatement : StatementNode
{
    /// <summary>The value to yield</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion

#region Java Annotations

/// <summary>
/// Java annotation: @Override, @SuppressWarnings("unchecked")
/// </summary>
public class JavaAnnotation : AttributeNode
{
    /// <summary>Whether this is a marker annotation (no arguments)</summary>
    public bool IsMarker { get; init; }

    /// <summary>Whether this is a single-element annotation: @Anno("value")</summary>
    public bool IsSingleElement { get; init; }

    /// <summary>Element-value pairs for normal annotation</summary>
    public IReadOnlyList<JavaAnnotationElementPair> ElementPairs { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Arguments);
        children.AddRange(ElementPairs);
        return children;
    }
}

/// <summary>
/// An element-value pair in a Java annotation: @Anno(name = "value")
/// </summary>
public class JavaAnnotationElementPair : UastNode
{
    /// <summary>The element name</summary>
    public required string ElementName { get; init; }

    /// <summary>The element value</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Well-known Java annotation types for semantic understanding.
/// </summary>
public enum JavaAnnotationType
{
    /// <summary>Unknown annotation type</summary>
    Unknown,
    /// <summary>@Override</summary>
    Override,
    /// <summary>@Deprecated</summary>
    Deprecated,
    /// <summary>@SuppressWarnings</summary>
    SuppressWarnings,
    /// <summary>@FunctionalInterface</summary>
    FunctionalInterface,
    /// <summary>@SafeVarargs</summary>
    SafeVarargs,
    /// <summary>@Retention</summary>
    Retention,
    /// <summary>@Target</summary>
    Target,
    /// <summary>@Documented</summary>
    Documented,
    /// <summary>@Inherited</summary>
    Inherited,
    /// <summary>@Repeatable</summary>
    Repeatable,
    /// <summary>@Native</summary>
    Native
}

#endregion

#region Java Try-With-Resources

/// <summary>
/// Java try-with-resources statement: try (var r = new Resource()) { }
/// </summary>
public class JavaTryWithResourcesStatement : TryStatement
{
    /// <summary>Resource specifications</summary>
    public IReadOnlyList<JavaResourceSpecification> Resources { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Resources);
        children.Add(TryBlock);
        children.AddRange(CatchClauses);
        if (FinallyBlock != null) children.Add(FinallyBlock);
        return children;
    }
}

/// <summary>
/// A resource in try-with-resources.
/// </summary>
public class JavaResourceSpecification : UastNode
{
    /// <summary>The variable name (null if using existing variable)</summary>
    public string? VariableName { get; init; }

    /// <summary>The variable type (null if using var or existing variable)</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The initializer expression or existing variable reference</summary>
    public required ExpressionNode Initializer { get; init; }

    /// <summary>Whether this uses var for type inference</summary>
    public bool UsesVar { get; init; }

    /// <summary>Whether this is final (default is implicitly final)</summary>
    public bool IsFinal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        children.Add(Initializer);
        return children;
    }
}

#endregion

#region Java Streams

/// <summary>
/// Represents a Java Stream pipeline for semantic analysis.
/// </summary>
public class JavaStreamPipeline : ExpressionNode
{
    /// <summary>The source of the stream (collection, array, generator)</summary>
    public required ExpressionNode Source { get; init; }

    /// <summary>Intermediate operations (map, filter, etc.)</summary>
    public IReadOnlyList<JavaStreamOperation> IntermediateOperations { get; init; } = [];

    /// <summary>Terminal operation (collect, forEach, reduce, etc.)</summary>
    public JavaStreamOperation? TerminalOperation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Source };
        children.AddRange(IntermediateOperations);
        if (TerminalOperation != null) children.Add(TerminalOperation);
        return children;
    }
}

/// <summary>
/// A stream operation in a Java Stream pipeline.
/// </summary>
public class JavaStreamOperation : UastNode
{
    /// <summary>The operation name (map, filter, collect, etc.)</summary>
    public required string OperationName { get; init; }

    /// <summary>The operation kind</summary>
    public JavaStreamOperationKind Kind { get; init; }

    /// <summary>Arguments to the operation</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    /// <summary>Whether this is a parallel stream operation</summary>
    public bool IsParallel { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Arguments.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Kind of Java Stream operation.
/// </summary>
public enum JavaStreamOperationKind
{
    /// <summary>Source operation (stream, of, iterate, generate)</summary>
    Source,
    /// <summary>Intermediate operation (map, filter, flatMap, etc.)</summary>
    Intermediate,
    /// <summary>Terminal operation (collect, forEach, reduce, etc.)</summary>
    Terminal,
    /// <summary>Short-circuiting operation (findFirst, anyMatch, etc.)</summary>
    ShortCircuiting
}

#endregion

#region Java Text Blocks

/// <summary>
/// Java text block (multi-line string literal): """..."""
/// </summary>
public class JavaTextBlockExpression : ExpressionNode
{
    /// <summary>The raw content of the text block</summary>
    public required string Content { get; init; }

    /// <summary>The processed content (with incidental whitespace stripped)</summary>
    public required string ProcessedContent { get; init; }

    /// <summary>Whether this text block contains embedded expressions (future)</summary>
    public bool HasInterpolation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Java Var (Local Variable Type Inference)

/// <summary>
/// Java var declaration for local variable type inference.
/// </summary>
public class JavaVarDeclaration : VariableDeclaration
{
    /// <summary>The inferred type (resolved at compile time)</summary>
    public TypeReference? InferredType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        if (Initializer != null) children.Add(Initializer);
        return children;
    }
}

#endregion

#region Java Enhanced For-Each

/// <summary>
/// Java enhanced for loop with additional metadata: for (var x : collection)
/// </summary>
public class JavaEnhancedForStatement : ForEachStatement
{
    /// <summary>Whether the loop variable uses var</summary>
    public bool UsesVar { get; init; }

    /// <summary>Whether the loop variable is final</summary>
    public bool IsFinal { get; init; }

    /// <summary>The inferred type when using var</summary>
    public TypeReference? InferredVariableType { get; init; }
}

#endregion

#region Java Method References

/// <summary>
/// Java method reference: Class::method, obj::method, Class::new
/// </summary>
public class JavaMethodReference : ExpressionNode
{
    /// <summary>The qualifier (type or expression)</summary>
    public required UastNode Qualifier { get; init; }

    /// <summary>The method name or "new" for constructor reference</summary>
    public required string MethodName { get; init; }

    /// <summary>Type arguments for generic method references</summary>
    public IReadOnlyList<TypeReference> TypeArguments { get; init; } = [];

    /// <summary>The kind of method reference</summary>
    public JavaMethodReferenceKind Kind { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Qualifier };
        children.AddRange(TypeArguments);
        return children;
    }
}

/// <summary>
/// Kind of Java method reference.
/// </summary>
public enum JavaMethodReferenceKind
{
    /// <summary>Reference to a static method: ContainingClass::staticMethodName</summary>
    StaticMethod,
    /// <summary>Reference to an instance method: containingObject::instanceMethodName</summary>
    BoundInstanceMethod,
    /// <summary>Reference to an instance method of arbitrary object: ContainingType::methodName</summary>
    UnboundInstanceMethod,
    /// <summary>Reference to a constructor: ClassName::new</summary>
    Constructor,
    /// <summary>Reference to an array constructor: int[]::new</summary>
    ArrayConstructor
}

#endregion

#region Java Lambda Expressions

/// <summary>
/// Java lambda expression with additional metadata.
/// </summary>
public class JavaLambdaExpression : LambdaExpression
{
    /// <summary>Whether this lambda uses expression form (no braces)</summary>
    public bool IsExpressionBody { get; init; }

    /// <summary>Whether parameters are implicitly typed</summary>
    public bool HasImplicitTypes { get; init; }

    /// <summary>Whether this is a single-parameter lambda without parentheses</summary>
    public bool HasImplicitParentheses { get; init; }

    /// <summary>The functional interface this lambda implements</summary>
    public TypeReference? TargetType { get; init; }
}

#endregion

#region Java Module System

/// <summary>
/// Java module declaration: module com.example.mymodule { }
/// </summary>
public class JavaModuleDeclaration : DeclarationNode
{
    /// <summary>Whether this is an open module</summary>
    public bool IsOpen { get; init; }

    /// <summary>Module directives</summary>
    public IReadOnlyList<JavaModuleDirective> Directives { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Directives);
        return children;
    }
}

/// <summary>
/// Base class for Java module directives.
/// </summary>
public abstract class JavaModuleDirective : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Java requires directive: requires transitive java.base;
/// </summary>
public class JavaRequiresDirective : JavaModuleDirective
{
    /// <summary>The required module name</summary>
    public required string ModuleName { get; init; }

    /// <summary>Whether this is a transitive requires</summary>
    public bool IsTransitive { get; init; }

    /// <summary>Whether this is a static requires</summary>
    public bool IsStatic { get; init; }
}

/// <summary>
/// Java exports directive: exports com.example.pkg to module1, module2;
/// </summary>
public class JavaExportsDirective : JavaModuleDirective
{
    /// <summary>The exported package name</summary>
    public required string PackageName { get; init; }

    /// <summary>Target modules (empty = exports to all)</summary>
    public IReadOnlyList<string> ToModules { get; init; } = [];
}

/// <summary>
/// Java opens directive: opens com.example.pkg to module1;
/// </summary>
public class JavaOpensDirective : JavaModuleDirective
{
    /// <summary>The opened package name</summary>
    public required string PackageName { get; init; }

    /// <summary>Target modules (empty = opens to all)</summary>
    public IReadOnlyList<string> ToModules { get; init; } = [];
}

/// <summary>
/// Java uses directive: uses com.example.Service;
/// </summary>
public class JavaUsesDirective : JavaModuleDirective
{
    /// <summary>The service type</summary>
    public required TypeReference ServiceType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [ServiceType];
}

/// <summary>
/// Java provides directive: provides Service with Impl1, Impl2;
/// </summary>
public class JavaProvidesDirective : JavaModuleDirective
{
    /// <summary>The service type</summary>
    public required TypeReference ServiceType { get; init; }

    /// <summary>Implementation types</summary>
    public IReadOnlyList<TypeReference> ImplementationTypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { ServiceType };
        children.AddRange(ImplementationTypes);
        return children;
    }
}

#endregion
