namespace UAST.Core.Schema;

/// <summary>
/// Visibility modifiers for declarations.
/// </summary>
public enum Visibility
{
    Default,
    Public,
    Private,
    Protected,
    Internal
}

/// <summary>
/// Kind of type declaration.
/// </summary>
public enum TypeDeclarationKind
{
    Class,
    Struct,
    Interface,
    Enum,
    Record,
    Trait
}

/// <summary>
/// Kind of variable declaration.
/// </summary>
public enum VariableKind
{
    Local,
    Field,
    Property,
    Parameter,
    Constant
}

/// <summary>
/// Kind of literal value.
/// </summary>
public enum LiteralKind
{
    Integer,
    Float,
    String,
    Boolean,
    Null,
    Regex,
    Char
}

/// <summary>
/// Binary operators.
/// </summary>
public enum BinaryOperator
{
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,

    // Comparison
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,

    // Logical
    And,
    Or,
    Xor,

    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,

    // String/Pattern
    Like,
    NotLike,
    Match,
    NotMatch,
    Replace,
    Split,
    Join,
    Contains,
    NotContains,
    In,
    NotIn,

    // Type
    Is,
    IsNot,
    As,

    // Range
    Range,
    RangeInclusive,

    // Null
    Coalesce
}

/// <summary>
/// Unary operators.
/// </summary>
public enum UnaryOperator
{
    Negate,
    Not,
    BitwiseNot,
    Increment,
    Decrement,
    Reference,
    Dereference,
    Spread,
    Await,
    Typeof,
    Sizeof,
    Splat
}

/// <summary>
/// Assignment operators.
/// </summary>
public enum AssignmentOperator
{
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    CoalesceAssign
}
