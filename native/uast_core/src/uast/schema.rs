//! UAST Schema - Rust implementation of the Unified Abstract Syntax Tree schema.
//!
//! This module provides Rust types that mirror the C# UAST schema, enabling
//! cross-language pattern matching and code analysis.
//!
//! # Design Decisions
//!
//! - `UastKind` is a flat enum covering all node kinds from the C# schema
//! - `UastNode` is a single struct with optional properties (simpler than class hierarchy)
//! - Properties use `Option<T>` for optional fields instead of nullable references
//! - Collections use `Vec<T>` instead of `IReadOnlyList<T>`
//! - The schema is designed for JSON serialization via serde

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Node Kind Enum - All UAST node types
// ============================================================================

/// All possible kinds of UAST nodes.
///
/// This flat enum covers all node types from the C# schema hierarchy:
/// - Declarations (FunctionDeclaration, TypeDeclaration, etc.)
/// - Statements (IfStatement, ForStatement, etc.)
/// - Expressions (CallExpression, BinaryExpression, etc.)
/// - Types (NamedTypeReference, ArrayTypeReference, etc.)
/// - Special (Unknown, Comment, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum UastKind {
    // ========== Source/Program Root ==========
    SourceFile,
    Module,
    ScriptBlock,
    Block,
    StatementList,

    // ========== Declarations ==========
    FunctionDeclaration,
    MethodDeclaration,
    ConstructorDeclaration,
    DestructorDeclaration,
    TypeDeclaration,
    ClassDeclaration,
    StructDeclaration,
    InterfaceDeclaration,
    TraitDeclaration,
    EnumDeclaration,
    EnumMemberDeclaration,
    RecordDeclaration,
    DelegateDeclaration,
    VariableDeclaration,
    FieldDeclaration,
    PropertyDeclaration,
    EventDeclaration,
    IndexerDeclaration,
    OperatorDeclaration,
    NamespaceDeclaration,
    ImportDeclaration,
    ExportDeclaration,
    TypeAlias,
    ModuleDeclaration,

    // ========== Parameters ==========
    Parameter,
    ParameterList,
    TypeParameter,
    TypeParameterList,
    RestParameter,

    // ========== Attributes/Modifiers ==========
    Attribute,
    AttributeList,
    Modifier,
    Decorator,
    Annotation,

    // ========== Statements ==========
    ExpressionStatement,
    ReturnStatement,
    BreakStatement,
    ContinueStatement,
    ThrowStatement,
    ExitStatement,
    EmptyStatement,
    LabeledStatement,
    GotoStatement,

    // Control Flow
    IfStatement,
    ElseClause,
    ElseIfClause,
    SwitchStatement,
    SwitchCase,
    SwitchSection,
    SwitchExpression,
    MatchExpression,
    MatchArm,

    // Loops
    ForStatement,
    ForEachStatement,
    ForInStatement,
    ForOfStatement,
    WhileStatement,
    DoWhileStatement,
    DoStatement,
    LoopStatement,

    // Exception Handling
    TryStatement,
    CatchClause,
    FinallyClause,
    TrapStatement,
    UsingStatement,

    // ========== Expressions ==========
    // Literals
    LiteralExpression,
    StringLiteral,
    NumberLiteral,
    BooleanLiteral,
    NullLiteral,
    CharacterLiteral,
    RegexLiteral,
    TemplateLiteral,

    // Identifiers
    Identifier,
    IdentifierExpression,
    VariableExpression,
    TypeIdentifier,
    QualifiedName,

    // Operators
    BinaryExpression,
    UnaryExpression,
    AssignmentExpression,
    ConditionalExpression,
    UpdateExpression,

    // Access
    MemberExpression,
    IndexExpression,
    CallExpression,
    NewExpression,
    InvokeMemberExpression,

    // Type Operations
    CastExpression,
    TypeCheckExpression,
    AsExpression,
    IsExpression,
    TypeofExpression,
    SizeofExpression,

    // Collections
    ArrayExpression,
    ObjectExpression,
    PropertyNode,
    TupleExpression,
    RangeExpression,

    // Functions
    LambdaExpression,
    ArrowFunction,
    ClosureExpression,

    // Interpolated Strings
    InterpolatedStringExpression,
    InterpolatedStringPart,

    // Other Expressions
    ParenthesizedExpression,
    AwaitExpression,
    YieldExpression,
    SpreadElement,
    ThisExpression,
    SuperExpression,
    BaseExpression,
    DefaultExpression,

    // ========== Type References ==========
    TypeReference,
    NamedTypeReference,
    ArrayTypeReference,
    NullableTypeReference,
    UnionTypeReference,
    IntersectionTypeReference,
    FunctionTypeReference,
    TupleTypeReference,
    GenericTypeReference,
    LiteralTypeReference,

    // ========== Arguments ==========
    Argument,
    ArgumentList,

    // ========== Comments ==========
    Comment,
    LineComment,
    BlockComment,
    DocComment,

    // ========== Language-Specific Extensions ==========
    // PowerShell
    CommandExpression,
    PipelineExpression,
    HashtableExpression,
    ScriptBlockExpression,
    CommandParameter,
    ExpandableStringLiteral,
    HereStringLiteral,
    Variable,
    FlowControlStatement,
    DataStatement,

    // Rust
    ImplDeclaration,
    UseDeclaration,
    MacroDefinition,
    MacroInvocation,
    UnsafeBlock,
    AsyncBlock,
    Lifetime,
    WhereClause,

    // Go
    DeferStatement,
    GoStatement,
    SelectStatement,
    ChannelType,
    SliceType,
    MapType,

    // C/C++
    IncludeDirective,
    DefineDirective,
    PreprocessorIf,
    PreprocessorElse,
    PointerType,
    TemplateDeclaration,

    // Java
    PackageDeclaration,
    SynchronizedStatement,
    AssertStatement,
    AnnotationDeclaration,

    // JavaScript/TypeScript
    JsxElement,
    JsxSelfClosingElement,
    JsxAttribute,
    GeneratorFunction,
    TypeAnnotation,

    // Python
    ListComprehension,
    DictionaryComprehension,
    SetComprehension,
    GeneratorExpression,
    WithStatement,
    PassStatement,
    RaiseStatement,
    FormattedString,

    // Ruby
    SymbolLiteral,
    DoBlock,
    RescueClause,
    EnsureClause,
    RequireStatement,
    UnlessStatement,
    UntilStatement,

    // C# Specific
    LocalDeclarationStatement,
    LocalFunctionStatement,
    LockStatement,
    FixedStatement,
    CheckedStatement,
    UncheckedStatement,
    QueryExpression,
    WithExpression,
    PatternMatching,

    // ========== Patterns (C# and others) ==========
    DeclarationPattern,
    ConstantPattern,
    TypePattern,
    VarPattern,
    DiscardPattern,
    ListPattern,
    PropertyPattern,
    PositionalPattern,
    RelationalPattern,
    AndPattern,
    OrPattern,
    NotPattern,

    // ========== Fallback ==========
    Unknown,
}

impl UastKind {
    /// Get the kind name as a static string.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::SourceFile => "SourceFile",
            Self::Module => "Module",
            Self::ScriptBlock => "ScriptBlock",
            Self::Block => "Block",
            Self::StatementList => "StatementList",
            Self::FunctionDeclaration => "FunctionDeclaration",
            Self::MethodDeclaration => "MethodDeclaration",
            Self::ConstructorDeclaration => "ConstructorDeclaration",
            Self::DestructorDeclaration => "DestructorDeclaration",
            Self::TypeDeclaration => "TypeDeclaration",
            Self::ClassDeclaration => "ClassDeclaration",
            Self::StructDeclaration => "StructDeclaration",
            Self::InterfaceDeclaration => "InterfaceDeclaration",
            Self::TraitDeclaration => "TraitDeclaration",
            Self::EnumDeclaration => "EnumDeclaration",
            Self::EnumMemberDeclaration => "EnumMemberDeclaration",
            Self::RecordDeclaration => "RecordDeclaration",
            Self::DelegateDeclaration => "DelegateDeclaration",
            Self::VariableDeclaration => "VariableDeclaration",
            Self::FieldDeclaration => "FieldDeclaration",
            Self::PropertyDeclaration => "PropertyDeclaration",
            Self::EventDeclaration => "EventDeclaration",
            Self::IndexerDeclaration => "IndexerDeclaration",
            Self::OperatorDeclaration => "OperatorDeclaration",
            Self::NamespaceDeclaration => "NamespaceDeclaration",
            Self::ImportDeclaration => "ImportDeclaration",
            Self::ExportDeclaration => "ExportDeclaration",
            Self::TypeAlias => "TypeAlias",
            Self::ModuleDeclaration => "ModuleDeclaration",
            Self::Parameter => "Parameter",
            Self::ParameterList => "ParameterList",
            Self::TypeParameter => "TypeParameter",
            Self::TypeParameterList => "TypeParameterList",
            Self::RestParameter => "RestParameter",
            Self::Attribute => "Attribute",
            Self::AttributeList => "AttributeList",
            Self::Modifier => "Modifier",
            Self::Decorator => "Decorator",
            Self::Annotation => "Annotation",
            Self::ExpressionStatement => "ExpressionStatement",
            Self::ReturnStatement => "ReturnStatement",
            Self::BreakStatement => "BreakStatement",
            Self::ContinueStatement => "ContinueStatement",
            Self::ThrowStatement => "ThrowStatement",
            Self::ExitStatement => "ExitStatement",
            Self::EmptyStatement => "EmptyStatement",
            Self::LabeledStatement => "LabeledStatement",
            Self::GotoStatement => "GotoStatement",
            Self::IfStatement => "IfStatement",
            Self::ElseClause => "ElseClause",
            Self::ElseIfClause => "ElseIfClause",
            Self::SwitchStatement => "SwitchStatement",
            Self::SwitchCase => "SwitchCase",
            Self::SwitchSection => "SwitchSection",
            Self::SwitchExpression => "SwitchExpression",
            Self::MatchExpression => "MatchExpression",
            Self::MatchArm => "MatchArm",
            Self::ForStatement => "ForStatement",
            Self::ForEachStatement => "ForEachStatement",
            Self::ForInStatement => "ForInStatement",
            Self::ForOfStatement => "ForOfStatement",
            Self::WhileStatement => "WhileStatement",
            Self::DoWhileStatement => "DoWhileStatement",
            Self::DoStatement => "DoStatement",
            Self::LoopStatement => "LoopStatement",
            Self::TryStatement => "TryStatement",
            Self::CatchClause => "CatchClause",
            Self::FinallyClause => "FinallyClause",
            Self::TrapStatement => "TrapStatement",
            Self::UsingStatement => "UsingStatement",
            Self::LiteralExpression => "LiteralExpression",
            Self::StringLiteral => "StringLiteral",
            Self::NumberLiteral => "NumberLiteral",
            Self::BooleanLiteral => "BooleanLiteral",
            Self::NullLiteral => "NullLiteral",
            Self::CharacterLiteral => "CharacterLiteral",
            Self::RegexLiteral => "RegexLiteral",
            Self::TemplateLiteral => "TemplateLiteral",
            Self::Identifier => "Identifier",
            Self::IdentifierExpression => "IdentifierExpression",
            Self::VariableExpression => "VariableExpression",
            Self::TypeIdentifier => "TypeIdentifier",
            Self::QualifiedName => "QualifiedName",
            Self::BinaryExpression => "BinaryExpression",
            Self::UnaryExpression => "UnaryExpression",
            Self::AssignmentExpression => "AssignmentExpression",
            Self::ConditionalExpression => "ConditionalExpression",
            Self::UpdateExpression => "UpdateExpression",
            Self::MemberExpression => "MemberExpression",
            Self::IndexExpression => "IndexExpression",
            Self::CallExpression => "CallExpression",
            Self::NewExpression => "NewExpression",
            Self::InvokeMemberExpression => "InvokeMemberExpression",
            Self::CastExpression => "CastExpression",
            Self::TypeCheckExpression => "TypeCheckExpression",
            Self::AsExpression => "AsExpression",
            Self::IsExpression => "IsExpression",
            Self::TypeofExpression => "TypeofExpression",
            Self::SizeofExpression => "SizeofExpression",
            Self::ArrayExpression => "ArrayExpression",
            Self::ObjectExpression => "ObjectExpression",
            Self::PropertyNode => "PropertyNode",
            Self::TupleExpression => "TupleExpression",
            Self::RangeExpression => "RangeExpression",
            Self::LambdaExpression => "LambdaExpression",
            Self::ArrowFunction => "ArrowFunction",
            Self::ClosureExpression => "ClosureExpression",
            Self::InterpolatedStringExpression => "InterpolatedStringExpression",
            Self::InterpolatedStringPart => "InterpolatedStringPart",
            Self::ParenthesizedExpression => "ParenthesizedExpression",
            Self::AwaitExpression => "AwaitExpression",
            Self::YieldExpression => "YieldExpression",
            Self::SpreadElement => "SpreadElement",
            Self::ThisExpression => "ThisExpression",
            Self::SuperExpression => "SuperExpression",
            Self::BaseExpression => "BaseExpression",
            Self::DefaultExpression => "DefaultExpression",
            Self::TypeReference => "TypeReference",
            Self::NamedTypeReference => "NamedTypeReference",
            Self::ArrayTypeReference => "ArrayTypeReference",
            Self::NullableTypeReference => "NullableTypeReference",
            Self::UnionTypeReference => "UnionTypeReference",
            Self::IntersectionTypeReference => "IntersectionTypeReference",
            Self::FunctionTypeReference => "FunctionTypeReference",
            Self::TupleTypeReference => "TupleTypeReference",
            Self::GenericTypeReference => "GenericTypeReference",
            Self::LiteralTypeReference => "LiteralTypeReference",
            Self::Argument => "Argument",
            Self::ArgumentList => "ArgumentList",
            Self::Comment => "Comment",
            Self::LineComment => "LineComment",
            Self::BlockComment => "BlockComment",
            Self::DocComment => "DocComment",
            Self::CommandExpression => "CommandExpression",
            Self::PipelineExpression => "PipelineExpression",
            Self::HashtableExpression => "HashtableExpression",
            Self::ScriptBlockExpression => "ScriptBlockExpression",
            Self::CommandParameter => "CommandParameter",
            Self::ExpandableStringLiteral => "ExpandableStringLiteral",
            Self::HereStringLiteral => "HereStringLiteral",
            Self::Variable => "Variable",
            Self::FlowControlStatement => "FlowControlStatement",
            Self::DataStatement => "DataStatement",
            Self::ImplDeclaration => "ImplDeclaration",
            Self::UseDeclaration => "UseDeclaration",
            Self::MacroDefinition => "MacroDefinition",
            Self::MacroInvocation => "MacroInvocation",
            Self::UnsafeBlock => "UnsafeBlock",
            Self::AsyncBlock => "AsyncBlock",
            Self::Lifetime => "Lifetime",
            Self::WhereClause => "WhereClause",
            Self::DeferStatement => "DeferStatement",
            Self::GoStatement => "GoStatement",
            Self::SelectStatement => "SelectStatement",
            Self::ChannelType => "ChannelType",
            Self::SliceType => "SliceType",
            Self::MapType => "MapType",
            Self::IncludeDirective => "IncludeDirective",
            Self::DefineDirective => "DefineDirective",
            Self::PreprocessorIf => "PreprocessorIf",
            Self::PreprocessorElse => "PreprocessorElse",
            Self::PointerType => "PointerType",
            Self::TemplateDeclaration => "TemplateDeclaration",
            Self::PackageDeclaration => "PackageDeclaration",
            Self::SynchronizedStatement => "SynchronizedStatement",
            Self::AssertStatement => "AssertStatement",
            Self::AnnotationDeclaration => "AnnotationDeclaration",
            Self::JsxElement => "JsxElement",
            Self::JsxSelfClosingElement => "JsxSelfClosingElement",
            Self::JsxAttribute => "JsxAttribute",
            Self::GeneratorFunction => "GeneratorFunction",
            Self::TypeAnnotation => "TypeAnnotation",
            Self::ListComprehension => "ListComprehension",
            Self::DictionaryComprehension => "DictionaryComprehension",
            Self::SetComprehension => "SetComprehension",
            Self::GeneratorExpression => "GeneratorExpression",
            Self::WithStatement => "WithStatement",
            Self::PassStatement => "PassStatement",
            Self::RaiseStatement => "RaiseStatement",
            Self::FormattedString => "FormattedString",
            Self::SymbolLiteral => "SymbolLiteral",
            Self::DoBlock => "DoBlock",
            Self::RescueClause => "RescueClause",
            Self::EnsureClause => "EnsureClause",
            Self::RequireStatement => "RequireStatement",
            Self::UnlessStatement => "UnlessStatement",
            Self::UntilStatement => "UntilStatement",
            Self::LocalDeclarationStatement => "LocalDeclarationStatement",
            Self::LocalFunctionStatement => "LocalFunctionStatement",
            Self::LockStatement => "LockStatement",
            Self::FixedStatement => "FixedStatement",
            Self::CheckedStatement => "CheckedStatement",
            Self::UncheckedStatement => "UncheckedStatement",
            Self::QueryExpression => "QueryExpression",
            Self::WithExpression => "WithExpression",
            Self::PatternMatching => "PatternMatching",
            Self::DeclarationPattern => "DeclarationPattern",
            Self::ConstantPattern => "ConstantPattern",
            Self::TypePattern => "TypePattern",
            Self::VarPattern => "VarPattern",
            Self::DiscardPattern => "DiscardPattern",
            Self::ListPattern => "ListPattern",
            Self::PropertyPattern => "PropertyPattern",
            Self::PositionalPattern => "PositionalPattern",
            Self::RelationalPattern => "RelationalPattern",
            Self::AndPattern => "AndPattern",
            Self::OrPattern => "OrPattern",
            Self::NotPattern => "NotPattern",
            Self::Unknown => "Unknown",
        }
    }

    /// Parse a kind from a string.
    pub fn from_str(s: &str) -> Self {
        match s {
            "SourceFile" => Self::SourceFile,
            "Module" => Self::Module,
            "ScriptBlock" => Self::ScriptBlock,
            "Block" => Self::Block,
            "StatementList" => Self::StatementList,
            "FunctionDeclaration" => Self::FunctionDeclaration,
            "MethodDeclaration" => Self::MethodDeclaration,
            "ConstructorDeclaration" => Self::ConstructorDeclaration,
            "DestructorDeclaration" => Self::DestructorDeclaration,
            "TypeDeclaration" => Self::TypeDeclaration,
            "ClassDeclaration" => Self::ClassDeclaration,
            "StructDeclaration" => Self::StructDeclaration,
            "InterfaceDeclaration" => Self::InterfaceDeclaration,
            "TraitDeclaration" => Self::TraitDeclaration,
            "EnumDeclaration" => Self::EnumDeclaration,
            "EnumMemberDeclaration" => Self::EnumMemberDeclaration,
            "RecordDeclaration" => Self::RecordDeclaration,
            "DelegateDeclaration" => Self::DelegateDeclaration,
            "VariableDeclaration" => Self::VariableDeclaration,
            "FieldDeclaration" => Self::FieldDeclaration,
            "PropertyDeclaration" => Self::PropertyDeclaration,
            "EventDeclaration" => Self::EventDeclaration,
            "IndexerDeclaration" => Self::IndexerDeclaration,
            "OperatorDeclaration" => Self::OperatorDeclaration,
            "NamespaceDeclaration" => Self::NamespaceDeclaration,
            "ImportDeclaration" => Self::ImportDeclaration,
            "ExportDeclaration" => Self::ExportDeclaration,
            "TypeAlias" => Self::TypeAlias,
            "ModuleDeclaration" => Self::ModuleDeclaration,
            "Parameter" => Self::Parameter,
            "ParameterList" => Self::ParameterList,
            "TypeParameter" => Self::TypeParameter,
            "TypeParameterList" => Self::TypeParameterList,
            "RestParameter" => Self::RestParameter,
            "Attribute" => Self::Attribute,
            "AttributeList" => Self::AttributeList,
            "Modifier" => Self::Modifier,
            "Decorator" => Self::Decorator,
            "Annotation" => Self::Annotation,
            "ExpressionStatement" => Self::ExpressionStatement,
            "ReturnStatement" => Self::ReturnStatement,
            "BreakStatement" => Self::BreakStatement,
            "ContinueStatement" => Self::ContinueStatement,
            "ThrowStatement" => Self::ThrowStatement,
            "ExitStatement" => Self::ExitStatement,
            "EmptyStatement" => Self::EmptyStatement,
            "LabeledStatement" => Self::LabeledStatement,
            "GotoStatement" => Self::GotoStatement,
            "IfStatement" => Self::IfStatement,
            "ElseClause" => Self::ElseClause,
            "ElseIfClause" => Self::ElseIfClause,
            "SwitchStatement" => Self::SwitchStatement,
            "SwitchCase" => Self::SwitchCase,
            "SwitchSection" => Self::SwitchSection,
            "SwitchExpression" => Self::SwitchExpression,
            "MatchExpression" => Self::MatchExpression,
            "MatchArm" => Self::MatchArm,
            "ForStatement" => Self::ForStatement,
            "ForEachStatement" => Self::ForEachStatement,
            "ForInStatement" => Self::ForInStatement,
            "ForOfStatement" => Self::ForOfStatement,
            "WhileStatement" => Self::WhileStatement,
            "DoWhileStatement" => Self::DoWhileStatement,
            "DoStatement" => Self::DoStatement,
            "LoopStatement" => Self::LoopStatement,
            "TryStatement" => Self::TryStatement,
            "CatchClause" => Self::CatchClause,
            "FinallyClause" => Self::FinallyClause,
            "TrapStatement" => Self::TrapStatement,
            "UsingStatement" => Self::UsingStatement,
            "LiteralExpression" => Self::LiteralExpression,
            "StringLiteral" => Self::StringLiteral,
            "NumberLiteral" => Self::NumberLiteral,
            "BooleanLiteral" => Self::BooleanLiteral,
            "NullLiteral" => Self::NullLiteral,
            "CharacterLiteral" => Self::CharacterLiteral,
            "RegexLiteral" => Self::RegexLiteral,
            "TemplateLiteral" => Self::TemplateLiteral,
            "Identifier" => Self::Identifier,
            "IdentifierExpression" => Self::IdentifierExpression,
            "VariableExpression" => Self::VariableExpression,
            "TypeIdentifier" => Self::TypeIdentifier,
            "QualifiedName" => Self::QualifiedName,
            "BinaryExpression" => Self::BinaryExpression,
            "UnaryExpression" => Self::UnaryExpression,
            "AssignmentExpression" => Self::AssignmentExpression,
            "ConditionalExpression" => Self::ConditionalExpression,
            "UpdateExpression" => Self::UpdateExpression,
            "MemberExpression" => Self::MemberExpression,
            "IndexExpression" => Self::IndexExpression,
            "CallExpression" => Self::CallExpression,
            "NewExpression" => Self::NewExpression,
            "InvokeMemberExpression" => Self::InvokeMemberExpression,
            "CastExpression" => Self::CastExpression,
            "TypeCheckExpression" => Self::TypeCheckExpression,
            "AsExpression" => Self::AsExpression,
            "IsExpression" => Self::IsExpression,
            "TypeofExpression" => Self::TypeofExpression,
            "SizeofExpression" => Self::SizeofExpression,
            "ArrayExpression" => Self::ArrayExpression,
            "ObjectExpression" => Self::ObjectExpression,
            "PropertyNode" => Self::PropertyNode,
            "TupleExpression" => Self::TupleExpression,
            "RangeExpression" => Self::RangeExpression,
            "LambdaExpression" => Self::LambdaExpression,
            "ArrowFunction" => Self::ArrowFunction,
            "ClosureExpression" => Self::ClosureExpression,
            "InterpolatedStringExpression" => Self::InterpolatedStringExpression,
            "InterpolatedStringPart" => Self::InterpolatedStringPart,
            "ParenthesizedExpression" => Self::ParenthesizedExpression,
            "AwaitExpression" => Self::AwaitExpression,
            "YieldExpression" => Self::YieldExpression,
            "SpreadElement" => Self::SpreadElement,
            "ThisExpression" => Self::ThisExpression,
            "SuperExpression" => Self::SuperExpression,
            "BaseExpression" => Self::BaseExpression,
            "DefaultExpression" => Self::DefaultExpression,
            "TypeReference" => Self::TypeReference,
            "NamedTypeReference" => Self::NamedTypeReference,
            "ArrayTypeReference" => Self::ArrayTypeReference,
            "NullableTypeReference" => Self::NullableTypeReference,
            "UnionTypeReference" => Self::UnionTypeReference,
            "IntersectionTypeReference" => Self::IntersectionTypeReference,
            "FunctionTypeReference" => Self::FunctionTypeReference,
            "TupleTypeReference" => Self::TupleTypeReference,
            "GenericTypeReference" => Self::GenericTypeReference,
            "LiteralTypeReference" => Self::LiteralTypeReference,
            "Argument" => Self::Argument,
            "ArgumentList" => Self::ArgumentList,
            "Comment" => Self::Comment,
            "LineComment" => Self::LineComment,
            "BlockComment" => Self::BlockComment,
            "DocComment" => Self::DocComment,
            "CommandExpression" => Self::CommandExpression,
            "PipelineExpression" => Self::PipelineExpression,
            "HashtableExpression" => Self::HashtableExpression,
            "ScriptBlockExpression" => Self::ScriptBlockExpression,
            "CommandParameter" => Self::CommandParameter,
            "ExpandableStringLiteral" => Self::ExpandableStringLiteral,
            "HereStringLiteral" => Self::HereStringLiteral,
            "Variable" => Self::Variable,
            "FlowControlStatement" => Self::FlowControlStatement,
            "DataStatement" => Self::DataStatement,
            "ImplDeclaration" => Self::ImplDeclaration,
            "UseDeclaration" => Self::UseDeclaration,
            "MacroDefinition" => Self::MacroDefinition,
            "MacroInvocation" => Self::MacroInvocation,
            "UnsafeBlock" => Self::UnsafeBlock,
            "AsyncBlock" => Self::AsyncBlock,
            "Lifetime" => Self::Lifetime,
            "WhereClause" => Self::WhereClause,
            "DeferStatement" => Self::DeferStatement,
            "GoStatement" => Self::GoStatement,
            "SelectStatement" => Self::SelectStatement,
            "ChannelType" => Self::ChannelType,
            "SliceType" => Self::SliceType,
            "MapType" => Self::MapType,
            "IncludeDirective" => Self::IncludeDirective,
            "DefineDirective" => Self::DefineDirective,
            "PreprocessorIf" => Self::PreprocessorIf,
            "PreprocessorElse" => Self::PreprocessorElse,
            "PointerType" => Self::PointerType,
            "TemplateDeclaration" => Self::TemplateDeclaration,
            "PackageDeclaration" => Self::PackageDeclaration,
            "SynchronizedStatement" => Self::SynchronizedStatement,
            "AssertStatement" => Self::AssertStatement,
            "AnnotationDeclaration" => Self::AnnotationDeclaration,
            "JsxElement" => Self::JsxElement,
            "JsxSelfClosingElement" => Self::JsxSelfClosingElement,
            "JsxAttribute" => Self::JsxAttribute,
            "GeneratorFunction" => Self::GeneratorFunction,
            "TypeAnnotation" => Self::TypeAnnotation,
            "ListComprehension" => Self::ListComprehension,
            "DictionaryComprehension" => Self::DictionaryComprehension,
            "SetComprehension" => Self::SetComprehension,
            "GeneratorExpression" => Self::GeneratorExpression,
            "WithStatement" => Self::WithStatement,
            "PassStatement" => Self::PassStatement,
            "RaiseStatement" => Self::RaiseStatement,
            "FormattedString" => Self::FormattedString,
            "SymbolLiteral" => Self::SymbolLiteral,
            "DoBlock" => Self::DoBlock,
            "RescueClause" => Self::RescueClause,
            "EnsureClause" => Self::EnsureClause,
            "RequireStatement" => Self::RequireStatement,
            "UnlessStatement" => Self::UnlessStatement,
            "UntilStatement" => Self::UntilStatement,
            "LocalDeclarationStatement" => Self::LocalDeclarationStatement,
            "LocalFunctionStatement" => Self::LocalFunctionStatement,
            "LockStatement" => Self::LockStatement,
            "FixedStatement" => Self::FixedStatement,
            "CheckedStatement" => Self::CheckedStatement,
            "UncheckedStatement" => Self::UncheckedStatement,
            "QueryExpression" => Self::QueryExpression,
            "WithExpression" => Self::WithExpression,
            "PatternMatching" => Self::PatternMatching,
            "DeclarationPattern" => Self::DeclarationPattern,
            "ConstantPattern" => Self::ConstantPattern,
            "TypePattern" => Self::TypePattern,
            "VarPattern" => Self::VarPattern,
            "DiscardPattern" => Self::DiscardPattern,
            "ListPattern" => Self::ListPattern,
            "PropertyPattern" => Self::PropertyPattern,
            "PositionalPattern" => Self::PositionalPattern,
            "RelationalPattern" => Self::RelationalPattern,
            "AndPattern" => Self::AndPattern,
            "OrPattern" => Self::OrPattern,
            "NotPattern" => Self::NotPattern,
            _ => Self::Unknown,
        }
    }
}

impl Default for UastKind {
    fn default() -> Self {
        Self::Unknown
    }
}

impl std::fmt::Display for UastKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// ============================================================================
// Source Span - Location information
// ============================================================================

/// Represents the source location of a UAST node.
///
/// All positions are preserved for bidirectional mapping to source.
/// Lines are 1-indexed, columns are 0-indexed (matching C# schema).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceSpan {
    /// 1-indexed line number of the start.
    pub start_line: u32,
    /// 0-indexed column of the start.
    pub start_column: u32,
    /// 1-indexed line number of the end.
    pub end_line: u32,
    /// 0-indexed column of the end.
    pub end_column: u32,
    /// Byte offset from start of file.
    pub start_offset: u32,
    /// Byte offset from start of file.
    pub end_offset: u32,
}

impl SourceSpan {
    /// Create a new source span.
    pub fn new(
        start_line: u32,
        start_column: u32,
        end_line: u32,
        end_column: u32,
        start_offset: u32,
        end_offset: u32,
    ) -> Self {
        Self {
            start_line,
            start_column,
            end_line,
            end_column,
            start_offset,
            end_offset,
        }
    }

    /// Create an empty/unknown span.
    pub const fn empty() -> Self {
        Self {
            start_line: 0,
            start_column: 0,
            end_line: 0,
            end_column: 0,
            start_offset: 0,
            end_offset: 0,
        }
    }

    /// The length in bytes of the span.
    pub fn length(&self) -> u32 {
        self.end_offset.saturating_sub(self.start_offset)
    }

    /// Whether this span is valid (non-empty).
    pub fn is_valid(&self) -> bool {
        self.start_line > 0
    }

    /// Create a span from tree-sitter node positions.
    pub fn from_tree_sitter(node: tree_sitter::Node) -> Self {
        let start = node.start_position();
        let end = node.end_position();
        Self {
            start_line: (start.row + 1) as u32,
            start_column: start.column as u32,
            end_line: (end.row + 1) as u32,
            end_column: end.column as u32,
            start_offset: node.start_byte() as u32,
            end_offset: node.end_byte() as u32,
        }
    }
}

impl std::fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.start_line, self.start_column, self.end_line, self.end_column
        )
    }
}

// ============================================================================
// UAST Node - The main AST node type
// ============================================================================

/// A unified AST node that can represent any language construct.
///
/// This is a flat structure with optional properties, rather than a class hierarchy
/// like the C# version. This makes serialization simpler and allows for dynamic
/// property access.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UastNode {
    /// The kind of node (e.g., FunctionDeclaration, BinaryExpression).
    pub kind: UastKind,

    /// The source language this node came from.
    pub language: String,

    /// The source location of this node.
    pub span: SourceSpan,

    /// The raw source text of this node (for leaf nodes).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,

    /// The name of the declared entity (for declarations).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// Child nodes.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<UastNode>,

    /// Language-specific properties that don't fit the unified schema.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub properties: HashMap<String, serde_json::Value>,

    /// The native tree-sitter node type (for debugging/extension).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub native_type: Option<String>,

    /// Whether this is a named node (vs anonymous like punctuation).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_named: Option<bool>,
}

impl UastNode {
    /// Create a new UAST node.
    pub fn new(kind: UastKind, language: impl Into<String>, span: SourceSpan) -> Self {
        Self {
            kind,
            language: language.into(),
            span,
            text: None,
            name: None,
            children: Vec::new(),
            properties: HashMap::new(),
            native_type: None,
            is_named: None,
        }
    }

    /// Create an unknown node.
    pub fn unknown(language: impl Into<String>, span: SourceSpan, native_type: &str) -> Self {
        Self {
            kind: UastKind::Unknown,
            language: language.into(),
            span,
            text: None,
            name: None,
            children: Vec::new(),
            properties: HashMap::new(),
            native_type: Some(native_type.to_string()),
            is_named: None,
        }
    }

    /// Set the raw source text.
    pub fn with_text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }

    /// Set the name.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Add a child node.
    pub fn with_child(mut self, child: UastNode) -> Self {
        self.children.push(child);
        self
    }

    /// Add multiple children.
    pub fn with_children(mut self, children: Vec<UastNode>) -> Self {
        self.children = children;
        self
    }

    /// Set a property.
    pub fn with_property(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.properties.insert(key.into(), value);
        self
    }

    /// Set the native type.
    pub fn with_native_type(mut self, native_type: impl Into<String>) -> Self {
        self.native_type = Some(native_type.into());
        self
    }

    /// Set whether this is a named node.
    pub fn with_is_named(mut self, is_named: bool) -> Self {
        self.is_named = Some(is_named);
        self
    }

    /// Check if this is a declaration node.
    pub fn is_declaration(&self) -> bool {
        matches!(
            self.kind,
            UastKind::FunctionDeclaration
                | UastKind::MethodDeclaration
                | UastKind::ConstructorDeclaration
                | UastKind::DestructorDeclaration
                | UastKind::TypeDeclaration
                | UastKind::ClassDeclaration
                | UastKind::StructDeclaration
                | UastKind::InterfaceDeclaration
                | UastKind::TraitDeclaration
                | UastKind::EnumDeclaration
                | UastKind::VariableDeclaration
                | UastKind::FieldDeclaration
                | UastKind::PropertyDeclaration
                | UastKind::NamespaceDeclaration
                | UastKind::ImportDeclaration
        )
    }

    /// Check if this is an expression node.
    pub fn is_expression(&self) -> bool {
        matches!(
            self.kind,
            UastKind::LiteralExpression
                | UastKind::StringLiteral
                | UastKind::NumberLiteral
                | UastKind::BooleanLiteral
                | UastKind::NullLiteral
                | UastKind::Identifier
                | UastKind::IdentifierExpression
                | UastKind::VariableExpression
                | UastKind::BinaryExpression
                | UastKind::UnaryExpression
                | UastKind::AssignmentExpression
                | UastKind::ConditionalExpression
                | UastKind::MemberExpression
                | UastKind::IndexExpression
                | UastKind::CallExpression
                | UastKind::NewExpression
                | UastKind::LambdaExpression
                | UastKind::ArrayExpression
                | UastKind::ObjectExpression
        )
    }

    /// Check if this is a statement node.
    pub fn is_statement(&self) -> bool {
        matches!(
            self.kind,
            UastKind::ExpressionStatement
                | UastKind::ReturnStatement
                | UastKind::BreakStatement
                | UastKind::ContinueStatement
                | UastKind::ThrowStatement
                | UastKind::IfStatement
                | UastKind::SwitchStatement
                | UastKind::ForStatement
                | UastKind::ForEachStatement
                | UastKind::WhileStatement
                | UastKind::DoWhileStatement
                | UastKind::TryStatement
                | UastKind::Block
        )
    }

    /// Get all descendants of this node (depth-first).
    pub fn descendants(&self) -> Vec<&UastNode> {
        let mut result = Vec::new();
        self.collect_descendants(&mut result);
        result
    }

    fn collect_descendants<'a>(&'a self, result: &mut Vec<&'a UastNode>) {
        for child in &self.children {
            result.push(child);
            child.collect_descendants(result);
        }
    }

    /// Find all descendants of a specific kind.
    pub fn descendants_of_kind(&self, kind: UastKind) -> Vec<&UastNode> {
        self.descendants()
            .into_iter()
            .filter(|n| n.kind == kind)
            .collect()
    }
}

// ============================================================================
// Enums from C# Schema
// ============================================================================

/// Visibility modifiers for declarations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Visibility {
    #[default]
    Default,
    Public,
    Private,
    Protected,
    Internal,
}

/// Kind of literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum LiteralKind {
    Integer,
    Float,
    String,
    Boolean,
    Null,
    Regex,
    Char,
    #[default]
    Unknown,
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BinaryOperator {
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
    Coalesce,

    #[default]
    Unknown,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum UnaryOperator {
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
    Splat,
    #[default]
    Unknown,
}

/// Assignment operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum AssignmentOperator {
    #[default]
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    CoalesceAssign,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uast_kind_as_str() {
        assert_eq!(UastKind::FunctionDeclaration.as_str(), "FunctionDeclaration");
        assert_eq!(UastKind::Unknown.as_str(), "Unknown");
        assert_eq!(UastKind::IfStatement.as_str(), "IfStatement");
    }

    #[test]
    fn test_uast_kind_from_str() {
        assert_eq!(
            UastKind::from_str("FunctionDeclaration"),
            UastKind::FunctionDeclaration
        );
        assert_eq!(UastKind::from_str("Unknown"), UastKind::Unknown);
        assert_eq!(UastKind::from_str("NonExistent"), UastKind::Unknown);
    }

    #[test]
    fn test_source_span() {
        let span = SourceSpan::new(1, 0, 5, 10, 0, 100);
        assert!(span.is_valid());
        assert_eq!(span.length(), 100);
        assert_eq!(span.to_string(), "1:0-5:10");

        let empty = SourceSpan::empty();
        assert!(!empty.is_valid());
    }

    #[test]
    fn test_uast_node_creation() {
        let span = SourceSpan::new(1, 0, 1, 10, 0, 10);
        let node = UastNode::new(UastKind::FunctionDeclaration, "rust", span)
            .with_name("my_function")
            .with_text("fn my_function()");

        assert_eq!(node.kind, UastKind::FunctionDeclaration);
        assert_eq!(node.language, "rust");
        assert_eq!(node.name, Some("my_function".to_string()));
        assert!(node.is_declaration());
        assert!(!node.is_expression());
    }

    #[test]
    fn test_uast_node_serialization() {
        let span = SourceSpan::new(1, 0, 1, 10, 0, 10);
        let node = UastNode::new(UastKind::Identifier, "rust", span).with_text("foo");

        let json = serde_json::to_string(&node).unwrap();
        assert!(json.contains("\"kind\":\"Identifier\""));
        assert!(json.contains("\"text\":\"foo\""));

        let parsed: UastNode = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.kind, UastKind::Identifier);
        assert_eq!(parsed.text, Some("foo".to_string()));
    }

    #[test]
    fn test_uast_node_descendants() {
        let span = SourceSpan::empty();
        let child1 = UastNode::new(UastKind::Identifier, "rust", span);
        let child2 = UastNode::new(UastKind::NumberLiteral, "rust", span);
        let grandchild = UastNode::new(UastKind::StringLiteral, "rust", span);

        let child1_with_grandchild = child1.with_child(grandchild);
        let parent = UastNode::new(UastKind::CallExpression, "rust", span)
            .with_child(child1_with_grandchild)
            .with_child(child2);

        let descendants = parent.descendants();
        assert_eq!(descendants.len(), 3);

        let identifiers = parent.descendants_of_kind(UastKind::Identifier);
        assert_eq!(identifiers.len(), 1);
    }

    #[test]
    fn test_visibility_serialization() {
        let vis = Visibility::Public;
        let json = serde_json::to_string(&vis).unwrap();
        assert_eq!(json, "\"public\"");

        let parsed: Visibility = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed, Visibility::Public);
    }

    #[test]
    fn test_binary_operator_serialization() {
        let op = BinaryOperator::Add;
        let json = serde_json::to_string(&op).unwrap();
        assert_eq!(json, "\"add\"");
    }
}
