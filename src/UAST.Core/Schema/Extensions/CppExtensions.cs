using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Pointers and References

/// <summary>
/// C/C++ pointer expression: *ptr (dereference) or &amp;value (address-of)
/// </summary>
public class CppPointerExpression : ExpressionNode
{
    /// <summary>
    /// The type of pointer operation.
    /// </summary>
    public PointerOperationType Operation { get; init; }

    /// <summary>
    /// The expression being operated on.
    /// </summary>
    public required ExpressionNode Inner { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Inner];
}

/// <summary>
/// Type of pointer operation in C/C++.
/// </summary>
public enum PointerOperationType
{
    /// <summary>Dereference: *ptr</summary>
    Dereference,
    /// <summary>Address-of: &amp;value</summary>
    AddressOf
}

/// <summary>
/// C++ reference expression: used for lvalue references (&amp;) and rvalue references (&amp;&amp;).
/// </summary>
public class CppReferenceExpression : ExpressionNode
{
    /// <summary>
    /// Whether this is an rvalue reference (T&amp;&amp;).
    /// </summary>
    public bool IsRvalueReference { get; init; }

    /// <summary>
    /// The expression being referenced.
    /// </summary>
    public required ExpressionNode Inner { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Inner];
}

/// <summary>
/// C/C++ pointer type: T* or T**
/// </summary>
public class CppPointerType : TypeReference
{
    /// <summary>
    /// The pointed-to type.
    /// </summary>
    public required TypeReference PointeeType { get; init; }

    /// <summary>
    /// Whether this is a const pointer (int* const).
    /// </summary>
    public bool IsConstPointer { get; init; }

    /// <summary>
    /// Whether the pointed-to value is const (const int*).
    /// </summary>
    public bool IsConstPointee { get; init; }

    /// <summary>
    /// Whether this is a volatile pointer.
    /// </summary>
    public bool IsVolatile { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [PointeeType];
}

/// <summary>
/// C++ reference type: T&amp; or T&amp;&amp;
/// </summary>
public class CppReferenceType : TypeReference
{
    /// <summary>
    /// The referenced type.
    /// </summary>
    public required TypeReference ReferencedType { get; init; }

    /// <summary>
    /// Whether this is an rvalue reference (T&amp;&amp;).
    /// </summary>
    public bool IsRvalueReference { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [ReferencedType];
}

#endregion

#region Preprocessor Directives

/// <summary>
/// C/C++ preprocessor directive base class.
/// </summary>
public abstract class CppPreprocessorDirective : StatementNode
{
    /// <summary>
    /// The directive name (#define, #include, #ifdef, etc.).
    /// </summary>
    public required string DirectiveName { get; init; }
}

/// <summary>
/// C/C++ #include directive.
/// </summary>
public class CppIncludeDirective : CppPreprocessorDirective
{
    /// <summary>
    /// The included file path.
    /// </summary>
    public required string Path { get; init; }

    /// <summary>
    /// Whether this uses angle brackets (&lt;file&gt;) vs quotes ("file").
    /// </summary>
    public bool IsSystemInclude { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// C/C++ #define directive for macro definitions.
/// </summary>
public class CppDefineDirective : CppPreprocessorDirective
{
    /// <summary>
    /// The macro name.
    /// </summary>
    public required string MacroName { get; init; }

    /// <summary>
    /// The macro parameters (for function-like macros).
    /// </summary>
    public IReadOnlyList<string> Parameters { get; init; } = [];

    /// <summary>
    /// Whether this is a function-like macro.
    /// </summary>
    public bool IsFunctionLike { get; init; }

    /// <summary>
    /// The macro replacement body.
    /// </summary>
    public string? Body { get; init; }

    /// <summary>
    /// Whether this is a variadic macro (...).
    /// </summary>
    public bool IsVariadic { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// C/C++ conditional preprocessor directive (#ifdef, #ifndef, #if, #elif, #else, #endif).
/// </summary>
public class CppConditionalDirective : CppPreprocessorDirective
{
    /// <summary>
    /// The condition expression (for #if and #elif).
    /// </summary>
    public string? Condition { get; init; }

    /// <summary>
    /// The macro name (for #ifdef and #ifndef).
    /// </summary>
    public string? MacroName { get; init; }

    /// <summary>
    /// The type of conditional directive.
    /// </summary>
    public ConditionalDirectiveType Type { get; init; }

    /// <summary>
    /// The body of the conditional block.
    /// </summary>
    public IReadOnlyList<UastNode> Body { get; init; } = [];

    /// <summary>
    /// The else branch (another CppConditionalDirective or statements).
    /// </summary>
    public IReadOnlyList<UastNode> ElseBranch { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Body);
        children.AddRange(ElseBranch);
        return children;
    }
}

/// <summary>
/// Type of conditional preprocessor directive.
/// </summary>
public enum ConditionalDirectiveType
{
    /// <summary>#ifdef MACRO</summary>
    Ifdef,
    /// <summary>#ifndef MACRO</summary>
    Ifndef,
    /// <summary>#if expression</summary>
    If,
    /// <summary>#elif expression</summary>
    Elif,
    /// <summary>#else</summary>
    Else,
    /// <summary>#endif</summary>
    Endif
}

#endregion

#region Templates (C++)

/// <summary>
/// C++ template declaration: template&lt;...&gt; class/struct/function
/// </summary>
public class CppTemplateDeclaration : DeclarationNode
{
    /// <summary>
    /// Template parameters.
    /// </summary>
    public required IReadOnlyList<CppTemplateParameter> TemplateParameters { get; init; }

    /// <summary>
    /// The templated declaration (class, struct, function, etc.).
    /// </summary>
    public required DeclarationNode Declaration { get; init; }

    /// <summary>
    /// Whether this is a variadic template (template&lt;typename... Args&gt;).
    /// </summary>
    public bool IsVariadic { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TemplateParameters);
        children.Add(Declaration);
        return children;
    }
}

/// <summary>
/// C++ template parameter.
/// </summary>
public class CppTemplateParameter : UastNode
{
    /// <summary>
    /// The parameter name.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The parameter kind (type, non-type, or template).
    /// </summary>
    public TemplateParameterKind Kind { get; init; }

    /// <summary>
    /// The type for non-type template parameters.
    /// </summary>
    public TypeReference? Type { get; init; }

    /// <summary>
    /// The default value or type.
    /// </summary>
    public UastNode? DefaultValue { get; init; }

    /// <summary>
    /// Whether this is a parameter pack (...).
    /// </summary>
    public bool IsParameterPack { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

/// <summary>
/// Kind of C++ template parameter.
/// </summary>
public enum TemplateParameterKind
{
    /// <summary>Type parameter: typename T or class T</summary>
    Type,
    /// <summary>Non-type parameter: int N</summary>
    NonType,
    /// <summary>Template template parameter: template&lt;...&gt; class T</summary>
    Template
}

/// <summary>
/// C++ template instantiation: Type&lt;Args...&gt;
/// </summary>
public class CppTemplateInstantiation : ExpressionNode
{
    /// <summary>
    /// The template name.
    /// </summary>
    public required string TemplateName { get; init; }

    /// <summary>
    /// The template arguments.
    /// </summary>
    public required IReadOnlyList<CppTemplateArgument> Arguments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arguments.Cast<UastNode>().ToList();
}

/// <summary>
/// C++ template argument (type or value).
/// </summary>
public class CppTemplateArgument : UastNode
{
    /// <summary>
    /// The type argument (for type template parameters).
    /// </summary>
    public TypeReference? TypeArgument { get; init; }

    /// <summary>
    /// The expression argument (for non-type template parameters).
    /// </summary>
    public ExpressionNode? ExpressionArgument { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        if (TypeArgument != null) return [TypeArgument];
        if (ExpressionArgument != null) return [ExpressionArgument];
        return [];
    }
}

#endregion

#region Concepts (C++20)

/// <summary>
/// C++20 concept definition: concept Name = constraint-expression
/// </summary>
public class CppConceptDeclaration : DeclarationNode
{
    /// <summary>
    /// Template parameters for the concept.
    /// </summary>
    public required IReadOnlyList<CppTemplateParameter> TemplateParameters { get; init; }

    /// <summary>
    /// The constraint expression defining the concept.
    /// </summary>
    public required ExpressionNode Constraint { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TemplateParameters);
        children.Add(Constraint);
        return children;
    }
}

/// <summary>
/// C++20 requires clause: requires (params) { requirements }
/// </summary>
public class CppRequiresClause : UastNode
{
    /// <summary>
    /// Optional parameter list for the requires expression.
    /// </summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>
    /// The requirements in the requires expression.
    /// </summary>
    public required IReadOnlyList<CppRequirement> Requirements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.AddRange(Requirements);
        return children;
    }
}

/// <summary>
/// A requirement in a C++20 requires expression.
/// </summary>
public class CppRequirement : UastNode
{
    /// <summary>
    /// The kind of requirement.
    /// </summary>
    public RequirementKind Kind { get; init; }

    /// <summary>
    /// The expression for simple, compound, or nested requirements.
    /// </summary>
    public ExpressionNode? Expression { get; init; }

    /// <summary>
    /// The type for type requirements.
    /// </summary>
    public TypeReference? Type { get; init; }

    /// <summary>
    /// The return type constraint for compound requirements.
    /// </summary>
    public TypeReference? ReturnTypeConstraint { get; init; }

    /// <summary>
    /// Whether the expression must not throw (noexcept).
    /// </summary>
    public bool IsNoexcept { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Expression != null) children.Add(Expression);
        if (Type != null) children.Add(Type);
        if (ReturnTypeConstraint != null) children.Add(ReturnTypeConstraint);
        return children;
    }
}

/// <summary>
/// Kind of C++20 requirement.
/// </summary>
public enum RequirementKind
{
    /// <summary>Simple requirement: expr;</summary>
    Simple,
    /// <summary>Type requirement: typename T;</summary>
    Type,
    /// <summary>Compound requirement: { expr } noexcept -&gt; type;</summary>
    Compound,
    /// <summary>Nested requirement: requires constraint-expr;</summary>
    Nested
}

#endregion

#region Coroutines (C++20)

/// <summary>
/// C++20 co_await expression.
/// </summary>
public class CppCoAwaitExpression : ExpressionNode
{
    /// <summary>
    /// The awaitable expression.
    /// </summary>
    public required ExpressionNode Operand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// C++20 co_yield expression.
/// </summary>
public class CppCoYieldExpression : ExpressionNode
{
    /// <summary>
    /// The yielded expression.
    /// </summary>
    public required ExpressionNode Operand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// C++20 co_return statement.
/// </summary>
public class CppCoReturnStatement : StatementNode
{
    /// <summary>
    /// The returned expression (optional).
    /// </summary>
    public ExpressionNode? Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Value != null ? [Value] : [];
    }
}

#endregion

#region Structured Bindings (C++17)

/// <summary>
/// C++17 structured binding declaration: auto [a, b, c] = expr;
/// </summary>
public class CppStructuredBindingDeclaration : DeclarationNode
{
    /// <summary>
    /// The binding names.
    /// </summary>
    public required IReadOnlyList<string> Bindings { get; init; }

    /// <summary>
    /// The initializer expression.
    /// </summary>
    public required ExpressionNode Initializer { get; init; }

    /// <summary>
    /// Whether this is a reference binding (auto&amp;).
    /// </summary>
    public bool IsReference { get; init; }

    /// <summary>
    /// Whether this is an rvalue reference binding (auto&amp;&amp;).
    /// </summary>
    public bool IsRvalueReference { get; init; }

    /// <summary>
    /// Whether this is const qualified.
    /// </summary>
    public bool IsConst { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Initializer);
        return children;
    }
}

#endregion

#region Constexpr and Consteval

/// <summary>
/// C++ constexpr/consteval declaration or expression.
/// </summary>
public class CppConstexprDeclaration : DeclarationNode
{
    /// <summary>
    /// Whether this is consteval (C++20) rather than constexpr.
    /// </summary>
    public bool IsConsteval { get; init; }

    /// <summary>
    /// Whether this is constinit (C++20).
    /// </summary>
    public bool IsConstinit { get; init; }

    /// <summary>
    /// The underlying declaration.
    /// </summary>
    public required DeclarationNode Declaration { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Declaration);
        return children;
    }
}

/// <summary>
/// C++17 if constexpr statement.
/// </summary>
public class CppIfConstexprStatement : StatementNode
{
    /// <summary>
    /// The condition (must be a constant expression).
    /// </summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>
    /// The then branch.
    /// </summary>
    public required StatementNode ThenBranch { get; init; }

    /// <summary>
    /// The optional else branch.
    /// </summary>
    public StatementNode? ElseBranch { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Condition, ThenBranch };
        if (ElseBranch != null) children.Add(ElseBranch);
        return children;
    }
}

#endregion

#region Auto Keyword

/// <summary>
/// C++ auto type placeholder.
/// </summary>
public class CppAutoType : TypeReference
{
    /// <summary>
    /// Whether this is decltype(auto).
    /// </summary>
    public bool IsDecltype { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// C++ decltype expression: decltype(expr)
/// </summary>
public class CppDecltypeExpression : TypeReference
{
    /// <summary>
    /// The expression whose type is being deduced.
    /// </summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Lambda Expressions

/// <summary>
/// C++ lambda expression with capture clause.
/// </summary>
public class CppLambdaExpression : ExpressionNode
{
    /// <summary>
    /// The capture clause.
    /// </summary>
    public required CppLambdaCapture Capture { get; init; }

    /// <summary>
    /// Lambda parameters.
    /// </summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>
    /// Optional explicit return type.
    /// </summary>
    public TypeReference? ReturnType { get; init; }

    /// <summary>
    /// The lambda body.
    /// </summary>
    public required UastNode Body { get; init; }

    /// <summary>
    /// Whether this lambda is mutable.
    /// </summary>
    public bool IsMutable { get; init; }

    /// <summary>
    /// Whether this lambda is constexpr.
    /// </summary>
    public bool IsConstexpr { get; init; }

    /// <summary>
    /// Whether this lambda is noexcept.
    /// </summary>
    public bool IsNoexcept { get; init; }

    /// <summary>
    /// Template parameters for generic lambdas (C++20).
    /// </summary>
    public IReadOnlyList<CppTemplateParameter> TemplateParameters { get; init; } = [];

    /// <summary>
    /// Requires clause for constrained generic lambdas (C++20).
    /// </summary>
    public ExpressionNode? RequiresClause { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Capture };
        children.AddRange(TemplateParameters);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        if (RequiresClause != null) children.Add(RequiresClause);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// C++ lambda capture clause.
/// </summary>
public class CppLambdaCapture : UastNode
{
    /// <summary>
    /// The default capture mode.
    /// </summary>
    public LambdaCaptureDefault Default { get; init; }

    /// <summary>
    /// Individual captures.
    /// </summary>
    public IReadOnlyList<CppCaptureItem> Captures { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Captures.Cast<UastNode>().ToList();
}

/// <summary>
/// Default capture mode for C++ lambdas.
/// </summary>
public enum LambdaCaptureDefault
{
    /// <summary>No default capture</summary>
    None,
    /// <summary>Default capture by value [=]</summary>
    ByValue,
    /// <summary>Default capture by reference [&amp;]</summary>
    ByReference
}

/// <summary>
/// Individual capture in a C++ lambda.
/// </summary>
public class CppCaptureItem : UastNode
{
    /// <summary>
    /// The captured variable name (null for this capture).
    /// </summary>
    public string? Name { get; init; }

    /// <summary>
    /// Whether this captures 'this'.
    /// </summary>
    public bool IsThis { get; init; }

    /// <summary>
    /// Whether this is a '*this' capture (copy of this, C++17).
    /// </summary>
    public bool IsThisCopy { get; init; }

    /// <summary>
    /// Whether captured by reference.
    /// </summary>
    public bool IsByReference { get; init; }

    /// <summary>
    /// Init-capture expression for init captures (C++14).
    /// </summary>
    public ExpressionNode? Initializer { get; init; }

    /// <summary>
    /// Whether this is a pack expansion (...).
    /// </summary>
    public bool IsPackExpansion { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Initializer != null ? [Initializer] : [];
    }
}

#endregion

#region Ranges (C++20)

/// <summary>
/// C++20 range-based constructs: views, pipelines, etc.
/// </summary>
public class CppRangeExpression : ExpressionNode
{
    /// <summary>
    /// The source range expression.
    /// </summary>
    public required ExpressionNode Source { get; init; }

    /// <summary>
    /// The range adapter/view being applied.
    /// </summary>
    public required string AdapterName { get; init; }

    /// <summary>
    /// Arguments to the range adapter.
    /// </summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Source };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// C++20 range pipe expression: range | view
/// </summary>
public class CppRangePipeExpression : ExpressionNode
{
    /// <summary>
    /// The left-hand side (source range).
    /// </summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>
    /// The right-hand side (view/adaptor).
    /// </summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

#endregion

#region Modules (C++20)

/// <summary>
/// C++20 module declaration.
/// </summary>
public class CppModuleDeclaration : DeclarationNode
{
    /// <summary>
    /// The module name (e.g., "mymodule.submodule").
    /// </summary>
    public required string ModuleName { get; init; }

    /// <summary>
    /// Whether this is an export declaration.
    /// </summary>
    public bool IsExport { get; init; }

    /// <summary>
    /// Whether this is a module partition.
    /// </summary>
    public bool IsPartition { get; init; }

    /// <summary>
    /// The partition name (if this is a partition).
    /// </summary>
    public string? PartitionName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// C++20 import declaration.
/// </summary>
public class CppImportDeclaration : DeclarationNode
{
    /// <summary>
    /// The imported module name.
    /// </summary>
    public required string ModuleName { get; init; }

    /// <summary>
    /// Whether this is a header unit import.
    /// </summary>
    public bool IsHeaderUnit { get; init; }

    /// <summary>
    /// The header name (for header unit imports).
    /// </summary>
    public string? HeaderName { get; init; }

    /// <summary>
    /// Whether this is a partition import.
    /// </summary>
    public bool IsPartition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// C++20 export declaration.
/// </summary>
public class CppExportDeclaration : DeclarationNode
{
    /// <summary>
    /// The exported declaration(s).
    /// </summary>
    public required IReadOnlyList<DeclarationNode> Declarations { get; init; }

    /// <summary>
    /// Whether this is an export block (export { ... }).
    /// </summary>
    public bool IsBlock { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Declarations);
        return children;
    }
}

#endregion

#region Designated Initializers (C++20)

/// <summary>
/// C++20 designated initializer: .member = value
/// </summary>
public class CppDesignatedInitializer : ExpressionNode
{
    /// <summary>
    /// The designators (member names or array indices).
    /// </summary>
    public required IReadOnlyList<CppDesignator> Designators { get; init; }

    /// <summary>
    /// The initializer value.
    /// </summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Designators);
        children.Add(Value);
        return children;
    }
}

/// <summary>
/// A designator in a designated initializer.
/// </summary>
public class CppDesignator : UastNode
{
    /// <summary>
    /// The member name (for .member designators).
    /// </summary>
    public string? MemberName { get; init; }

    /// <summary>
    /// The array index (for [index] designators - C only).
    /// </summary>
    public ExpressionNode? ArrayIndex { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return ArrayIndex != null ? [ArrayIndex] : [];
    }
}

#endregion

#region C++ Specific Statements and Expressions

/// <summary>
/// C++ namespace declaration.
/// </summary>
public class CppNamespaceDeclaration : DeclarationNode
{
    /// <summary>
    /// The namespace name (null for anonymous namespaces).
    /// </summary>
    public string? NamespaceName { get; init; }

    /// <summary>
    /// Whether this is an inline namespace.
    /// </summary>
    public bool IsInline { get; init; }

    /// <summary>
    /// Whether this is a nested namespace declaration (namespace A::B::C).
    /// </summary>
    public bool IsNested { get; init; }

    /// <summary>
    /// The namespace body declarations.
    /// </summary>
    public required IReadOnlyList<UastNode> Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Body);
        return children;
    }
}

/// <summary>
/// C++ using declaration: using name; or using namespace ns;
/// </summary>
public class CppUsingDeclaration : DeclarationNode
{
    /// <summary>
    /// The name being imported.
    /// </summary>
    public required string Target { get; init; }

    /// <summary>
    /// Whether this is "using namespace".
    /// </summary>
    public bool IsNamespaceUsing { get; init; }

    /// <summary>
    /// Whether this is a type alias (using Type = ...;).
    /// </summary>
    public bool IsTypeAlias { get; init; }

    /// <summary>
    /// The aliased type (for type aliases).
    /// </summary>
    public TypeReference? AliasedType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (AliasedType != null) children.Add(AliasedType);
        return children;
    }
}

/// <summary>
/// C++ scope resolution expression: ns::name or Type::member
/// </summary>
public class CppScopeResolutionExpression : ExpressionNode
{
    /// <summary>
    /// The scope (namespace or type) - null for global scope (::name).
    /// </summary>
    public ExpressionNode? Scope { get; init; }

    /// <summary>
    /// The name being accessed.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// Whether this is global scope resolution (::name).
    /// </summary>
    public bool IsGlobalScope { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Scope != null ? [Scope] : [];
    }
}

/// <summary>
/// C++ new expression: new Type or new Type[size] or new (placement) Type
/// </summary>
public class CppNewExpression : ExpressionNode
{
    /// <summary>
    /// The type being allocated.
    /// </summary>
    public required TypeReference Type { get; init; }

    /// <summary>
    /// Whether this is an array allocation (new[]).
    /// </summary>
    public bool IsArray { get; init; }

    /// <summary>
    /// The array size expression (for array allocations).
    /// </summary>
    public ExpressionNode? ArraySize { get; init; }

    /// <summary>
    /// Placement arguments (for placement new).
    /// </summary>
    public IReadOnlyList<ExpressionNode> PlacementArgs { get; init; } = [];

    /// <summary>
    /// Constructor arguments.
    /// </summary>
    public IReadOnlyList<ExpressionNode> ConstructorArgs { get; init; } = [];

    /// <summary>
    /// Initializer list (for new Type{...}).
    /// </summary>
    public IReadOnlyList<ExpressionNode> InitializerList { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        if (ArraySize != null) children.Add(ArraySize);
        children.AddRange(PlacementArgs);
        children.AddRange(ConstructorArgs);
        children.AddRange(InitializerList);
        return children;
    }
}

/// <summary>
/// C++ delete expression: delete ptr or delete[] arr
/// </summary>
public class CppDeleteExpression : ExpressionNode
{
    /// <summary>
    /// The expression being deleted.
    /// </summary>
    public required ExpressionNode Operand { get; init; }

    /// <summary>
    /// Whether this is an array delete (delete[]).
    /// </summary>
    public bool IsArray { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// C++ static_cast, dynamic_cast, const_cast, or reinterpret_cast expression.
/// </summary>
public class CppNamedCastExpression : ExpressionNode
{
    /// <summary>
    /// The kind of cast.
    /// </summary>
    public CppCastKind CastKind { get; init; }

    /// <summary>
    /// The target type.
    /// </summary>
    public required TypeReference TargetType { get; init; }

    /// <summary>
    /// The expression being cast.
    /// </summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [TargetType, Expression];
}

/// <summary>
/// Kind of C++ named cast.
/// </summary>
public enum CppCastKind
{
    /// <summary>static_cast&lt;T&gt;(expr)</summary>
    Static,
    /// <summary>dynamic_cast&lt;T&gt;(expr)</summary>
    Dynamic,
    /// <summary>const_cast&lt;T&gt;(expr)</summary>
    Const,
    /// <summary>reinterpret_cast&lt;T&gt;(expr)</summary>
    Reinterpret
}

/// <summary>
/// C++ throw expression: throw expr or throw;
/// </summary>
public class CppThrowExpression : ExpressionNode
{
    /// <summary>
    /// The expression being thrown (null for rethrow: throw;).
    /// </summary>
    public ExpressionNode? Operand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Operand != null ? [Operand] : [];
    }
}

/// <summary>
/// C++ sizeof expression: sizeof(type) or sizeof expr or sizeof...(pack)
/// </summary>
public class CppSizeofExpression : ExpressionNode
{
    /// <summary>
    /// The type operand (for sizeof(type)).
    /// </summary>
    public TypeReference? TypeOperand { get; init; }

    /// <summary>
    /// The expression operand (for sizeof expr).
    /// </summary>
    public ExpressionNode? ExpressionOperand { get; init; }

    /// <summary>
    /// Whether this is sizeof...(pack).
    /// </summary>
    public bool IsParameterPack { get; init; }

    /// <summary>
    /// The pack name (for sizeof...).
    /// </summary>
    public string? PackName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        if (TypeOperand != null) return [TypeOperand];
        if (ExpressionOperand != null) return [ExpressionOperand];
        return [];
    }
}

/// <summary>
/// C++ alignof expression: alignof(type)
/// </summary>
public class CppAlignofExpression : ExpressionNode
{
    /// <summary>
    /// The type whose alignment is being queried.
    /// </summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// C++ noexcept expression: noexcept(expr)
/// </summary>
public class CppNoexceptExpression : ExpressionNode
{
    /// <summary>
    /// The expression to check.
    /// </summary>
    public required ExpressionNode Operand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// C++ typeid expression: typeid(type) or typeid(expr)
/// </summary>
public class CppTypeidExpression : ExpressionNode
{
    /// <summary>
    /// The type operand (for typeid(type)).
    /// </summary>
    public TypeReference? TypeOperand { get; init; }

    /// <summary>
    /// The expression operand (for typeid(expr)).
    /// </summary>
    public ExpressionNode? ExpressionOperand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        if (TypeOperand != null) return [TypeOperand];
        if (ExpressionOperand != null) return [ExpressionOperand];
        return [];
    }
}

#endregion

#region C++20 Spaceship Operator

/// <summary>
/// C++20 three-way comparison (spaceship) operator: a &lt;=&gt; b
/// </summary>
public class CppSpaceshipExpression : ExpressionNode
{
    /// <summary>
    /// The left-hand side.
    /// </summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>
    /// The right-hand side.
    /// </summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

#endregion

#region Fold Expressions (C++17)

/// <summary>
/// C++17 fold expression: (pack op ...) or (... op pack) or (pack op ... op init)
/// </summary>
public class CppFoldExpression : ExpressionNode
{
    /// <summary>
    /// The pack expression.
    /// </summary>
    public required ExpressionNode Pack { get; init; }

    /// <summary>
    /// The operator used in the fold.
    /// </summary>
    public required string Operator { get; init; }

    /// <summary>
    /// The kind of fold (left, right, or binary).
    /// </summary>
    public FoldExpressionKind Kind { get; init; }

    /// <summary>
    /// The initial value (for binary folds).
    /// </summary>
    public ExpressionNode? InitialValue { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pack };
        if (InitialValue != null) children.Add(InitialValue);
        return children;
    }
}

/// <summary>
/// Kind of C++17 fold expression.
/// </summary>
public enum FoldExpressionKind
{
    /// <summary>Left fold: (... op pack)</summary>
    Left,
    /// <summary>Right fold: (pack op ...)</summary>
    Right,
    /// <summary>Binary left fold: (init op ... op pack)</summary>
    BinaryLeft,
    /// <summary>Binary right fold: (pack op ... op init)</summary>
    BinaryRight
}

#endregion
