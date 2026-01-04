//! Static mapping tables from tree-sitter node types to UAST NodeKind strings.
//!
//! This module provides language-specific mappings that convert tree-sitter's
//! concrete syntax tree node types into universal abstract syntax tree node kinds.

use once_cell::sync::Lazy;
use std::collections::HashMap;

/// A collection of node kind mappings for a specific language.
pub struct NodeKindMappings {
    mappings: HashMap<&'static str, &'static str>,
}

impl NodeKindMappings {
    /// Create a new NodeKindMappings from a static slice of tuples.
    fn new(pairs: &[(&'static str, &'static str)]) -> Self {
        let mut mappings = HashMap::with_capacity(pairs.len());
        for (ts_type, uast_kind) in pairs {
            mappings.insert(*ts_type, *uast_kind);
        }
        Self { mappings }
    }

    /// Look up the UAST NodeKind for a tree-sitter node type.
    /// Returns "Unknown" if no mapping exists.
    pub fn get(&self, ts_node_type: &str) -> &'static str {
        self.mappings.get(ts_node_type).copied().unwrap_or("Unknown")
    }

    /// Check if a mapping exists for the given tree-sitter node type.
    pub fn contains(&self, ts_node_type: &str) -> bool {
        self.mappings.contains_key(ts_node_type)
    }

    /// Reverse lookup: find all native tree-sitter types that map to a given UAST type.
    /// Returns a vector of native type names that map to the specified UAST kind.
    pub fn get_native_types_for_uast(&self, uast_kind: &str) -> Vec<&'static str> {
        self.mappings
            .iter()
            .filter(|(_, v)| **v == uast_kind)
            .map(|(k, _)| *k)
            .collect()
    }
}

/// Generic mappings that work across most languages.
static GENERIC_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    NodeKindMappings::new(&[
        // Source/Program root
        ("source_file", "SourceFile"),
        ("program", "SourceFile"),
        ("module", "Module"),
        ("translation_unit", "SourceFile"),

        // Functions
        ("function_declaration", "FunctionDeclaration"),
        ("function_definition", "FunctionDeclaration"),
        ("function_item", "FunctionDeclaration"),
        ("method_declaration", "MethodDeclaration"),
        ("method_definition", "MethodDeclaration"),
        ("arrow_function", "ArrowFunction"),
        ("lambda_expression", "LambdaExpression"),
        ("lambda", "LambdaExpression"),
        ("closure_expression", "LambdaExpression"),

        // Classes and Types
        ("class_declaration", "TypeDeclaration"),
        ("class_definition", "TypeDeclaration"),
        ("class_item", "TypeDeclaration"),
        ("struct_declaration", "TypeDeclaration"),
        ("struct_definition", "TypeDeclaration"),
        ("struct_item", "TypeDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("interface_definition", "InterfaceDeclaration"),
        ("trait_item", "InterfaceDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("enum_definition", "EnumDeclaration"),
        ("enum_item", "EnumDeclaration"),
        ("type_alias_declaration", "TypeAlias"),
        ("type_item", "TypeAlias"),

        // Variables and Declarations
        ("variable_declaration", "VariableDeclaration"),
        ("variable_declarator", "VariableDeclarator"),
        ("let_declaration", "VariableDeclaration"),
        ("const_declaration", "VariableDeclaration"),
        ("const_item", "VariableDeclaration"),
        ("static_item", "VariableDeclaration"),
        ("field_declaration", "FieldDeclaration"),
        ("property_declaration", "PropertyDeclaration"),

        // Expressions
        ("call_expression", "CallExpression"),
        ("function_call", "CallExpression"),
        ("method_call_expression", "CallExpression"),
        ("new_expression", "NewExpression"),
        ("assignment_expression", "AssignmentExpression"),
        ("assignment", "AssignmentExpression"),
        ("binary_expression", "BinaryExpression"),
        ("binary_operator", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("unary_operator", "UnaryExpression"),
        ("member_expression", "MemberExpression"),
        ("field_expression", "MemberExpression"),
        ("subscript_expression", "IndexExpression"),
        ("index_expression", "IndexExpression"),
        ("array_access", "IndexExpression"),
        ("conditional_expression", "ConditionalExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("await_expression", "AwaitExpression"),
        ("yield_expression", "YieldExpression"),

        // Identifiers and Literals
        ("identifier", "Identifier"),
        ("type_identifier", "TypeIdentifier"),
        ("property_identifier", "PropertyIdentifier"),
        ("field_identifier", "FieldIdentifier"),
        ("string", "StringLiteral"),
        ("string_literal", "StringLiteral"),
        ("template_string", "TemplateLiteral"),
        ("template_literal", "TemplateLiteral"),
        ("number", "NumberLiteral"),
        ("number_literal", "NumberLiteral"),
        ("integer_literal", "NumberLiteral"),
        ("float_literal", "NumberLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("boolean", "BooleanLiteral"),
        ("null", "NullLiteral"),
        ("nil", "NullLiteral"),
        ("none", "NullLiteral"),

        // Control Flow
        ("if_statement", "IfStatement"),
        ("if_expression", "IfExpression"),
        ("else_clause", "ElseClause"),
        ("else_if_clause", "ElseIfClause"),
        ("switch_statement", "SwitchStatement"),
        ("switch_expression", "SwitchExpression"),
        ("match_expression", "MatchExpression"),
        ("case_clause", "CaseClause"),
        ("match_arm", "MatchArm"),

        // Loops
        ("for_statement", "ForStatement"),
        ("for_expression", "ForExpression"),
        ("for_in_statement", "ForInStatement"),
        ("for_of_statement", "ForOfStatement"),
        ("foreach_statement", "ForEachStatement"),
        ("while_statement", "WhileStatement"),
        ("while_expression", "WhileExpression"),
        ("do_statement", "DoWhileStatement"),
        ("loop_expression", "LoopStatement"),

        // Control Keywords
        ("break_statement", "BreakStatement"),
        ("break_expression", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("continue_expression", "ContinueStatement"),
        ("return_statement", "ReturnStatement"),
        ("return_expression", "ReturnStatement"),
        ("throw_statement", "ThrowStatement"),

        // Error Handling
        ("try_statement", "TryStatement"),
        ("try_expression", "TryStatement"),
        ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"),

        // Blocks and Statements
        ("block", "Block"),
        ("statement_block", "Block"),
        ("compound_statement", "Block"),
        ("expression_statement", "ExpressionStatement"),
        ("empty_statement", "EmptyStatement"),

        // Imports and Exports
        ("import_declaration", "ImportDeclaration"),
        ("import_statement", "ImportDeclaration"),
        ("use_declaration", "ImportDeclaration"),
        ("export_statement", "ExportDeclaration"),
        ("export_declaration", "ExportDeclaration"),

        // Comments
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
        ("block_comment", "BlockComment"),
        ("doc_comment", "DocComment"),

        // Collections
        ("array", "ArrayExpression"),
        ("array_expression", "ArrayExpression"),
        ("array_literal", "ArrayExpression"),
        ("object", "ObjectExpression"),
        ("object_expression", "ObjectExpression"),
        ("hash", "ObjectExpression"),
        ("dictionary", "ObjectExpression"),
        ("tuple_expression", "TupleExpression"),

        // Parameters
        ("parameters", "ParameterList"),
        ("parameter_list", "ParameterList"),
        ("formal_parameters", "ParameterList"),
        ("parameter", "Parameter"),
        ("formal_parameter", "Parameter"),
        ("rest_parameter", "RestParameter"),
        ("spread_element", "SpreadElement"),

        // Arguments
        ("arguments", "ArgumentList"),
        ("argument_list", "ArgumentList"),
        ("argument", "Argument"),

        // Attributes and Decorators
        ("decorator", "Decorator"),
        ("attribute", "Attribute"),
        ("annotation", "Annotation"),

        // Async
        ("async", "AsyncModifier"),
        ("await", "AwaitKeyword"),

        // Operators
        ("+", "PlusOperator"),
        ("-", "MinusOperator"),
        ("*", "MultiplyOperator"),
        ("/", "DivideOperator"),
        ("%", "ModuloOperator"),
        ("==", "EqualsOperator"),
        ("!=", "NotEqualsOperator"),
        ("===", "StrictEqualsOperator"),
        ("!==", "StrictNotEqualsOperator"),
        ("<", "LessThanOperator"),
        (">", "GreaterThanOperator"),
        ("<=", "LessThanOrEqualOperator"),
        (">=", "GreaterThanOrEqualOperator"),
        ("&&", "LogicalAndOperator"),
        ("||", "LogicalOrOperator"),
        ("!", "LogicalNotOperator"),
        ("&", "BitwiseAndOperator"),
        ("|", "BitwiseOrOperator"),
        ("^", "BitwiseXorOperator"),
        ("~", "BitwiseNotOperator"),
        ("<<", "LeftShiftOperator"),
        (">>", "RightShiftOperator"),
        ("=", "AssignmentOperator"),
        ("+=", "AddAssignOperator"),
        ("-=", "SubAssignOperator"),
        ("*=", "MulAssignOperator"),
        ("/=", "DivAssignOperator"),
    ])
});

/// JavaScript/TypeScript specific mappings.
static JAVASCRIPT_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("lexical_declaration", "VariableDeclaration"),
        ("variable_declaration", "VariableDeclaration"),
        ("function", "FunctionDeclaration"),
        ("generator_function", "GeneratorFunction"),
        ("generator_function_declaration", "GeneratorFunction"),
        ("jsx_element", "JsxElement"),
        ("jsx_self_closing_element", "JsxSelfClosingElement"),
        ("jsx_opening_element", "JsxOpeningElement"),
        ("jsx_closing_element", "JsxClosingElement"),
        ("jsx_attribute", "JsxAttribute"),
        ("jsx_expression", "JsxExpression"),
        ("regex", "RegexLiteral"),
        ("regex_literal", "RegexLiteral"),
        ("this", "ThisExpression"),
        ("super", "SuperExpression"),
        ("undefined", "UndefinedLiteral"),
        ("labeled_statement", "LabeledStatement"),
        ("with_statement", "WithStatement"),
        ("debugger_statement", "DebuggerStatement"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// TypeScript specific mappings (extends JavaScript).
static TYPESCRIPT_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all JavaScript mappings first
    for (k, v) in &JAVASCRIPT_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add TypeScript-specific (these will override JavaScript when there's conflict)
    pairs.extend_from_slice(&[
        ("type_annotation", "TypeAnnotation"),
        ("type_parameter", "TypeParameter"),
        ("type_parameters", "TypeParameterList"),
        ("as_expression", "AsExpression"),
        ("satisfies_expression", "SatisfiesExpression"),
        ("non_null_expression", "NonNullExpression"),
        ("ambient_declaration", "AmbientDeclaration"),
        ("abstract_class_declaration", "AbstractClassDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("type_alias_declaration", "TypeAliasDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("namespace_declaration", "NamespaceDeclaration"),
        ("module_declaration", "ModuleDeclaration"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Python specific mappings.
static PYTHON_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    // Start with generic mappings, then override with language-specific
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("function_definition", "FunctionDeclaration"),
        ("async_function_definition", "AsyncFunctionDeclaration"),
        ("class_definition", "TypeDeclaration"),
        ("decorated_definition", "DecoratedDefinition"),
        ("with_statement", "WithStatement"),
        ("with_clause", "WithClause"),
        ("as_pattern", "AsPattern"),
        ("assert_statement", "AssertStatement"),
        ("global_statement", "GlobalStatement"),
        ("nonlocal_statement", "NonlocalStatement"),
        ("exec_statement", "ExecStatement"),
        ("print_statement", "PrintStatement"),
        ("expression_list", "ExpressionList"),
        ("list", "ListExpression"),
        ("list_comprehension", "ListComprehension"),
        ("dictionary", "DictionaryExpression"),
        ("dictionary_comprehension", "DictionaryComprehension"),
        ("set", "SetExpression"),
        ("set_comprehension", "SetComprehension"),
        ("generator_expression", "GeneratorExpression"),
        ("slice", "SliceExpression"),
        ("ellipsis", "EllipsisLiteral"),
        ("concatenated_string", "ConcatenatedString"),
        ("formatted_string", "FormattedString"),
        ("f_string", "FormattedString"),
        ("pass_statement", "PassStatement"),
        ("raise_statement", "RaiseStatement"),
        ("except_clause", "ExceptClause"),
        ("finally_clause", "FinallyClause"),
        ("import_from_statement", "ImportFromStatement"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Go specific mappings.
static GO_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("function_declaration", "FunctionDeclaration"),
        ("method_declaration", "MethodDeclaration"),
        ("type_declaration", "TypeDeclaration"),
        ("type_spec", "TypeSpec"),
        ("struct_type", "StructType"),
        ("interface_type", "InterfaceType"),
        ("map_type", "MapType"),
        ("channel_type", "ChannelType"),
        ("slice_type", "SliceType"),
        ("array_type", "ArrayType"),
        ("pointer_type", "PointerType"),
        ("func_literal", "FunctionLiteral"),
        ("composite_literal", "CompositeLiteral"),
        ("literal_value", "LiteralValue"),
        ("keyed_element", "KeyedElement"),
        ("selector_expression", "SelectorExpression"),
        ("type_assertion", "TypeAssertion"),
        ("type_switch_statement", "TypeSwitchStatement"),
        ("short_var_declaration", "ShortVarDeclaration"),
        ("var_declaration", "VarDeclaration"),
        ("const_declaration", "ConstDeclaration"),
        ("defer_statement", "DeferStatement"),
        ("go_statement", "GoStatement"),
        ("select_statement", "SelectStatement"),
        ("communication_case", "CommunicationCase"),
        ("send_statement", "SendStatement"),
        ("receive_statement", "ReceiveStatement"),
        ("range_clause", "RangeClause"),
        ("for_clause", "ForClause"),
        ("package_clause", "PackageClause"),
        ("import_spec", "ImportSpec"),
        ("blank_identifier", "BlankIdentifier"),
        ("iota", "IotaLiteral"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Rust specific mappings.
static RUST_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("function_item", "FunctionDeclaration"),
        ("struct_item", "TypeDeclaration"),
        ("enum_item", "EnumDeclaration"),
        ("trait_item", "TraitDeclaration"),
        ("impl_item", "ImplDeclaration"),
        ("mod_item", "ModuleDeclaration"),
        ("use_declaration", "UseDeclaration"),
        ("type_item", "TypeAlias"),
        ("const_item", "ConstDeclaration"),
        ("static_item", "StaticDeclaration"),
        ("macro_definition", "MacroDefinition"),
        ("macro_invocation", "MacroInvocation"),
        ("attribute_item", "Attribute"),
        ("inner_attribute_item", "InnerAttribute"),
        ("let_declaration", "LetDeclaration"),
        ("match_expression", "MatchExpression"),
        ("match_arm", "MatchArm"),
        ("if_let_expression", "IfLetExpression"),
        ("while_let_expression", "WhileLetExpression"),
        ("loop_expression", "LoopExpression"),
        ("for_expression", "ForExpression"),
        ("closure_expression", "ClosureExpression"),
        ("async_block", "AsyncBlock"),
        ("unsafe_block", "UnsafeBlock"),
        ("block", "Block"),
        ("reference_expression", "ReferenceExpression"),
        ("dereference_expression", "DereferenceExpression"),
        ("try_expression", "TryExpression"),
        ("range_expression", "RangeExpression"),
        ("tuple_expression", "TupleExpression"),
        ("unit_expression", "UnitExpression"),
        ("unit_type", "UnitType"),
        ("lifetime", "Lifetime"),
        ("where_clause", "WhereClause"),
        ("generic_type", "GenericType"),
        ("type_parameters", "TypeParameters"),
        ("type_arguments", "TypeArguments"),
        ("scoped_identifier", "ScopedIdentifier"),
        ("crate", "CrateKeyword"),
        ("self", "SelfKeyword"),
        ("super", "SuperKeyword"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// C/C++ specific mappings.
static C_CPP_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("function_definition", "FunctionDeclaration"),
        ("declaration", "Declaration"),
        ("struct_specifier", "StructDeclaration"),
        ("union_specifier", "UnionDeclaration"),
        ("enum_specifier", "EnumDeclaration"),
        ("class_specifier", "ClassDeclaration"),
        ("namespace_definition", "NamespaceDeclaration"),
        ("template_declaration", "TemplateDeclaration"),
        ("preproc_include", "IncludeDirective"),
        ("preproc_def", "DefineDirective"),
        ("preproc_ifdef", "IfdefDirective"),
        ("preproc_if", "IfDirective"),
        ("preproc_else", "ElseDirective"),
        ("preproc_elif", "ElifDirective"),
        ("preproc_endif", "EndifDirective"),
        ("preproc_call", "PreprocessorCall"),
        ("init_declarator", "InitDeclarator"),
        ("pointer_declarator", "PointerDeclarator"),
        ("array_declarator", "ArrayDeclarator"),
        ("function_declarator", "FunctionDeclarator"),
        ("sizeof_expression", "SizeofExpression"),
        ("alignof_expression", "AlignofExpression"),
        ("cast_expression", "CastExpression"),
        ("compound_literal_expression", "CompoundLiteralExpression"),
        ("initializer_list", "InitializerList"),
        ("designated_initializer", "DesignatedInitializer"),
        ("goto_statement", "GotoStatement"),
        ("labeled_statement", "LabeledStatement"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Java specific mappings.
static JAVA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("method_declaration", "MethodDeclaration"),
        ("constructor_declaration", "ConstructorDeclaration"),
        ("class_declaration", "ClassDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("annotation_type_declaration", "AnnotationDeclaration"),
        ("record_declaration", "RecordDeclaration"),
        ("field_declaration", "FieldDeclaration"),
        ("local_variable_declaration", "LocalVariableDeclaration"),
        ("package_declaration", "PackageDeclaration"),
        ("import_declaration", "ImportDeclaration"),
        ("method_invocation", "MethodInvocation"),
        ("object_creation_expression", "ObjectCreationExpression"),
        ("instanceof_expression", "InstanceofExpression"),
        ("cast_expression", "CastExpression"),
        ("lambda_expression", "LambdaExpression"),
        ("method_reference", "MethodReference"),
        ("synchronized_statement", "SynchronizedStatement"),
        ("assert_statement", "AssertStatement"),
        ("enhanced_for_statement", "EnhancedForStatement"),
        ("try_with_resources_statement", "TryWithResourcesStatement"),
        ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"),
        ("throws_clause", "ThrowsClause"),
        ("annotation", "Annotation"),
        ("marker_annotation", "MarkerAnnotation"),
        ("type_arguments", "TypeArguments"),
        ("type_parameters", "TypeParameters"),
        ("wildcard", "Wildcard"),
        ("super", "SuperExpression"),
        ("this", "ThisExpression"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Ruby specific mappings.
static RUBY_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        ("method", "MethodDeclaration"),
        ("singleton_method", "SingletonMethodDeclaration"),
        ("class", "ClassDeclaration"),
        ("module", "ModuleDeclaration"),
        ("block", "Block"),
        ("do_block", "DoBlock"),
        ("begin", "BeginStatement"),
        ("rescue", "RescueClause"),
        ("ensure", "EnsureClause"),
        ("lambda", "LambdaExpression"),
        ("proc", "ProcExpression"),
        ("symbol", "SymbolLiteral"),
        ("hash", "HashLiteral"),
        ("array", "ArrayLiteral"),
        ("regex", "RegexLiteral"),
        ("heredoc_body", "HeredocBody"),
        ("interpolation", "StringInterpolation"),
        ("require", "RequireStatement"),
        ("require_relative", "RequireRelativeStatement"),
        ("yield", "YieldStatement"),
        ("case", "CaseStatement"),
        ("when", "WhenClause"),
        ("unless", "UnlessStatement"),
        ("until", "UntilStatement"),
        ("range", "RangeExpression"),
        ("splat_argument", "SplatArgument"),
        ("block_argument", "BlockArgument"),
        ("keyword_parameter", "KeywordParameter"),
        ("self", "SelfExpression"),
        ("nil", "NilLiteral"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// PowerShell specific mappings.
static POWERSHELL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add PowerShell-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),
        ("script_block", "ScriptBlock"),
        ("script_block_body", "Block"),
        ("statement_block", "Block"),
        ("statement_list", "StatementList"),
        ("named_block", "NamedBlock"),
        ("named_block_list", "NamedBlockList"),

        // Functions and parameters
        ("function_statement", "FunctionDeclaration"),
        ("function_name", "Identifier"),
        ("function_parameter_declaration", "ParameterList"),
        ("param_block", "ParameterBlock"),
        ("parameter_list", "ParameterList"),
        ("script_parameter", "Parameter"),
        ("script_parameter_default", "DefaultValue"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"),
        ("elseif_clause", "ElseIfClause"),
        ("elseif_clauses", "ElseIfClauses"),
        ("else_clause", "ElseClause"),
        ("switch_statement", "SwitchStatement"),
        ("switch_body", "SwitchBody"),
        ("switch_clause", "SwitchCase"),
        ("switch_clause_condition", "SwitchCaseCondition"),
        ("switch_clauses", "SwitchCases"),

        // Control flow - loops
        ("for_statement", "ForStatement"),
        ("for_initializer", "ForInitializer"),
        ("for_condition", "ForCondition"),
        ("for_iterator", "ForIterator"),
        ("foreach_statement", "ForEachStatement"),
        ("while_statement", "WhileStatement"),
        ("while_condition", "WhileCondition"),
        ("do_statement", "DoStatement"),

        // Control flow - flow control
        ("flow_control_statement", "FlowControlStatement"),
        ("label_expression", "LabelExpression"),

        // Error handling
        ("try_statement", "TryStatement"),
        ("catch_clause", "CatchClause"),
        ("catch_clauses", "CatchClauses"),
        ("catch_type_list", "CatchTypeList"),
        ("finally_clause", "FinallyClause"),
        ("trap_statement", "TrapStatement"),

        // Classes and enums
        ("class_statement", "TypeDeclaration"),
        ("class_property_definition", "PropertyDeclaration"),
        ("class_method_definition", "MethodDeclaration"),
        ("class_method_parameter", "Parameter"),
        ("class_method_parameter_list", "ParameterList"),
        ("class_attribute", "Attribute"),
        ("enum_statement", "EnumDeclaration"),
        ("enum_member", "EnumMember"),

        // Commands and pipelines
        ("pipeline", "PipelineExpression"),
        ("pipeline_chain", "PipelineChain"),
        ("command", "CommandExpression"),
        ("command_name", "CommandName"),
        ("command_name_expr", "CommandNameExpression"),
        ("command_elements", "ArgumentList"),
        ("command_parameter", "CommandParameter"),
        ("command_invokation_operator", "InvocationOperator"),
        ("path_command_name", "PathCommandName"),

        // Expressions
        ("assignment_expression", "AssignmentExpression"),
        ("left_assignment_expression", "LeftAssignmentExpression"),
        ("logical_expression", "LogicalExpression"),
        ("bitwise_expression", "BitwiseExpression"),
        ("comparison_expression", "ComparisonExpression"),
        ("additive_expression", "BinaryExpression"),
        ("multiplicative_expression", "BinaryExpression"),
        ("format_expression", "FormatExpression"),
        ("range_expression", "RangeExpression"),
        ("array_literal_expression", "ArrayExpression"),
        ("unary_expression", "UnaryExpression"),
        ("expression_with_unary_operator", "UnaryExpression"),
        ("pre_increment_expression", "UpdateExpression"),
        ("pre_decrement_expression", "UpdateExpression"),
        ("post_increment_expression", "UpdateExpression"),
        ("post_decrement_expression", "UpdateExpression"),
        ("cast_expression", "CastExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("sub_expression", "SubExpression"),
        ("array_expression", "ArrayExpression"),
        ("hash_literal_expression", "HashtableExpression"),
        ("hash_literal_body", "HashtableBody"),
        ("hash_entry", "HashtableEntry"),
        ("key_expression", "KeyExpression"),
        ("member_access", "MemberExpression"),
        ("member_name", "PropertyIdentifier"),
        ("element_access", "IndexExpression"),
        ("invokation_expression", "CallExpression"),
        ("invokation_foreach_expression", "ForEachMethodExpression"),
        ("script_block_expression", "ScriptBlockExpression"),

        // Arguments
        ("argument_list", "ArgumentList"),
        ("argument_expression_list", "ArgumentList"),
        ("argument_expression", "Argument"),

        // Literals and identifiers
        ("integer_literal", "NumberLiteral"),
        ("decimal_integer_literal", "NumberLiteral"),
        ("hexadecimal_integer_literal", "NumberLiteral"),
        ("real_literal", "NumberLiteral"),
        ("string_literal", "StringLiteral"),
        ("expandable_string_literal", "ExpandableStringLiteral"),
        ("expandable_here_string_literal", "HereStringLiteral"),
        ("verbatim_string_characters", "StringLiteral"),
        ("verbatim_here_string_characters", "HereStringLiteral"),
        ("variable", "Variable"),
        ("braced_variable", "Variable"),
        ("simple_name", "Identifier"),

        // Types
        ("type_name", "TypeReference"),
        ("type_literal", "TypeLiteral"),
        ("type_spec", "TypeSpec"),
        ("array_type_name", "ArrayTypeReference"),
        ("generic_type_name", "GenericTypeReference"),
        ("generic_type_arguments", "TypeArguments"),
        ("type_identifier", "TypeIdentifier"),

        // Operators
        ("assignement_operator", "AssignmentOperator"),
        ("comparison_operator", "ComparisonOperator"),
        ("format_operator", "FormatOperator"),
        ("file_redirection_operator", "RedirectionOperator"),
        ("merging_redirection_operator", "RedirectionOperator"),

        // Attributes
        ("attribute_list", "AttributeList"),
        ("attribute", "Attribute"),
        ("attribute_name", "AttributeName"),
        ("attribute_arguments", "AttributeArguments"),
        ("attribute_argument", "AttributeArgument"),

        // Other statements
        ("data_statement", "DataStatement"),
        ("inlinescript_statement", "InlineScriptStatement"),
        ("parallel_statement", "ParallelStatement"),
        ("sequence_statement", "SequenceStatement"),
        ("empty_statement", "EmptyStatement"),

        // Redirections
        ("redirections", "Redirections"),
        ("redirection", "Redirection"),
        ("redirected_file_name", "RedirectedFileName"),

        // Comments
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// C# specific mappings.
static CSHARP_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add C#-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("compilation_unit", "SourceFile"),
        ("global_statement", "GlobalStatement"),
        ("namespace_declaration", "NamespaceDeclaration"),
        ("file_scoped_namespace_declaration", "NamespaceDeclaration"),
        ("using_directive", "ImportDeclaration"),

        // Type declarations
        ("type_declaration", "TypeDeclaration"),
        ("class_declaration", "ClassDeclaration"),
        ("struct_declaration", "StructDeclaration"),
        ("record_declaration", "RecordDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("enum_member_declaration", "EnumMember"),
        ("enum_member_declaration_list", "EnumMemberList"),
        ("delegate_declaration", "DelegateDeclaration"),

        // Members
        ("method_declaration", "MethodDeclaration"),
        ("constructor_declaration", "ConstructorDeclaration"),
        ("destructor_declaration", "DestructorDeclaration"),
        ("field_declaration", "FieldDeclaration"),
        ("property_declaration", "PropertyDeclaration"),
        ("event_declaration", "EventDeclaration"),
        ("event_field_declaration", "EventFieldDeclaration"),
        ("indexer_declaration", "IndexerDeclaration"),
        ("operator_declaration", "OperatorDeclaration"),
        ("conversion_operator_declaration", "ConversionOperatorDeclaration"),
        ("accessor_declaration", "AccessorDeclaration"),

        // Variables
        ("variable_declaration", "VariableDeclaration"),
        ("variable_declarator", "VariableDeclarator"),
        ("local_declaration_statement", "LocalDeclarationStatement"),
        ("local_function_statement", "LocalFunctionStatement"),

        // Statements
        ("block", "Block"),
        ("statement", "Statement"),
        ("expression_statement", "ExpressionStatement"),
        ("empty_statement", "EmptyStatement"),
        ("if_statement", "IfStatement"),
        ("switch_statement", "SwitchStatement"),
        ("switch_section", "SwitchSection"),
        ("switch_expression", "SwitchExpression"),
        ("for_statement", "ForStatement"),
        ("foreach_statement", "ForEachStatement"),
        ("while_statement", "WhileStatement"),
        ("do_statement", "DoWhileStatement"),
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("return_statement", "ReturnStatement"),
        ("throw_statement", "ThrowStatement"),
        ("yield_statement", "YieldStatement"),
        ("goto_statement", "GotoStatement"),
        ("labeled_statement", "LabeledStatement"),
        ("try_statement", "TryStatement"),
        ("catch_clause", "CatchClause"),
        ("catch_declaration", "CatchDeclaration"),
        ("finally_clause", "FinallyClause"),
        ("lock_statement", "LockStatement"),
        ("using_statement", "UsingStatement"),
        ("fixed_statement", "FixedStatement"),
        ("checked_statement", "CheckedStatement"),
        ("unsafe_statement", "UnsafeStatement"),

        // Expressions
        ("assignment_expression", "AssignmentExpression"),
        ("binary_expression", "BinaryExpression"),
        ("prefix_unary_expression", "UnaryExpression"),
        ("postfix_unary_expression", "UpdateExpression"),
        ("conditional_expression", "ConditionalExpression"),
        ("conditional_access_expression", "ConditionalAccessExpression"),
        ("invocation_expression", "CallExpression"),
        ("element_access_expression", "IndexExpression"),
        ("member_access_expression", "MemberExpression"),
        ("member_binding_expression", "MemberBindingExpression"),
        ("object_creation_expression", "NewExpression"),
        ("array_creation_expression", "ArrayCreationExpression"),
        ("anonymous_object_creation_expression", "AnonymousObjectExpression"),
        ("implicit_object_creation_expression", "ImplicitNewExpression"),
        ("anonymous_method_expression", "AnonymousMethodExpression"),
        ("lambda_expression", "LambdaExpression"),
        ("parenthesized_lambda_expression", "LambdaExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("cast_expression", "CastExpression"),
        ("as_expression", "AsExpression"),
        ("is_expression", "IsExpression"),
        ("is_pattern_expression", "IsPatternExpression"),
        ("typeof_expression", "TypeofExpression"),
        ("sizeof_expression", "SizeofExpression"),
        ("default_expression", "DefaultExpression"),
        ("checked_expression", "CheckedExpression"),
        ("await_expression", "AwaitExpression"),
        ("throw_expression", "ThrowExpression"),
        ("query_expression", "QueryExpression"),
        ("interpolated_string_expression", "TemplateLiteral"),
        ("range_expression", "RangeExpression"),
        ("with_expression", "WithExpression"),
        ("switch_expression_arm", "SwitchExpressionArm"),

        // Patterns
        ("declaration_pattern", "DeclarationPattern"),
        ("constant_pattern", "ConstantPattern"),
        ("type_pattern", "TypePattern"),
        ("var_pattern", "VarPattern"),
        ("discard_pattern", "DiscardPattern"),
        ("parenthesized_pattern", "ParenthesizedPattern"),
        ("relational_pattern", "RelationalPattern"),
        ("and_pattern", "AndPattern"),
        ("or_pattern", "OrPattern"),
        ("not_pattern", "NotPattern"),
        ("list_pattern", "ListPattern"),
        ("property_pattern", "PropertyPattern"),
        ("positional_pattern", "PositionalPattern"),
        ("recursive_pattern", "RecursivePattern"),

        // Literals
        ("integer_literal", "NumberLiteral"),
        ("real_literal", "NumberLiteral"),
        ("character_literal", "CharacterLiteral"),
        ("string_literal", "StringLiteral"),
        ("verbatim_string_literal", "VerbatimStringLiteral"),
        ("raw_string_literal", "RawStringLiteral"),
        ("interpolated_string_text", "InterpolatedStringText"),
        ("null_literal", "NullLiteral"),
        ("boolean_literal", "BooleanLiteral"),

        // Identifiers and names
        ("identifier", "Identifier"),
        ("qualified_name", "QualifiedName"),
        ("alias_qualified_name", "AliasQualifiedName"),
        ("simple_name", "Identifier"),
        ("generic_name", "GenericName"),
        ("predefined_type", "PredefinedType"),

        // Types
        ("type", "TypeReference"),
        ("array_type", "ArrayTypeReference"),
        ("nullable_type", "NullableTypeReference"),
        ("pointer_type", "PointerTypeReference"),
        ("function_pointer_type", "FunctionPointerType"),
        ("tuple_type", "TupleType"),
        ("ref_type", "RefType"),
        ("scoped_type", "ScopedType"),
        ("type_parameter", "TypeParameter"),
        ("type_parameter_list", "TypeParameterList"),
        ("type_argument_list", "TypeArgumentList"),
        ("type_parameter_constraints_clause", "TypeParameterConstraint"),

        // Parameters and arguments
        ("parameter_list", "ParameterList"),
        ("parameter", "Parameter"),
        ("argument_list", "ArgumentList"),
        ("argument", "Argument"),
        ("bracketed_argument_list", "BracketedArgumentList"),

        // Attributes
        ("attribute_list", "AttributeList"),
        ("attribute", "Attribute"),
        ("attribute_argument_list", "AttributeArgumentList"),
        ("attribute_argument", "AttributeArgument"),

        // Modifiers
        ("modifier", "Modifier"),
        ("accessibility_modifier", "AccessibilityModifier"),

        // Other
        ("arrow_expression_clause", "ArrowExpressionClause"),
        ("base_list", "BaseList"),
        ("initializer_expression", "InitializerExpression"),
        ("collection_expression", "CollectionExpression"),
        ("object_initializer", "ObjectInitializer"),
        ("array_initializer", "ArrayInitializer"),
        ("equals_value_clause", "EqualsValueClause"),

        // LINQ
        ("from_clause", "FromClause"),
        ("where_clause", "WhereClause"),
        ("select_clause", "SelectClause"),
        ("group_clause", "GroupClause"),
        ("orderby_clause", "OrderByClause"),
        ("join_clause", "JoinClause"),
        ("let_clause", "LetClause"),
        ("query_body", "QueryBody"),
        ("query_continuation", "QueryContinuation"),

        // Preprocessor
        ("preproc_if", "PreprocessorIf"),
        ("preproc_else", "PreprocessorElse"),
        ("preproc_elif", "PreprocessorElif"),
        ("preproc_region", "PreprocessorRegion"),
        ("preproc_endregion", "PreprocessorEndRegion"),
        ("preproc_line", "PreprocessorLine"),
        ("preproc_pragma", "PreprocessorPragma"),
        ("preproc_nullable", "PreprocessorNullable"),

        // Keywords that map to literals
        ("this_expression", "ThisExpression"),
        ("base_expression", "BaseExpression"),

        // Comments
        ("comment", "Comment"),
        ("documentation_comment", "DocComment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Get the appropriate mappings for a language.
///
/// Returns language-specific mappings if available, otherwise returns generic mappings.
pub fn get_mappings(language: &str) -> &'static NodeKindMappings {
    match language.to_lowercase().as_str() {
        "javascript" | "js" => &JAVASCRIPT_MAPPINGS,
        "typescript" | "ts" | "tsx" | "jsx" => &TYPESCRIPT_MAPPINGS,
        "python" | "py" => &PYTHON_MAPPINGS,
        "go" | "golang" => &GO_MAPPINGS,
        "rust" | "rs" => &RUST_MAPPINGS,
        "c" | "cpp" | "c++" | "cxx" | "cc" | "h" | "hpp" => &C_CPP_MAPPINGS,
        "java" => &JAVA_MAPPINGS,
        "ruby" | "rb" => &RUBY_MAPPINGS,
        "powershell" | "ps1" | "psm1" | "psd1" | "pwsh" => &POWERSHELL_MAPPINGS,
        "c-sharp" | "csharp" | "cs" | "c_sharp" => &CSHARP_MAPPINGS,
        _ => &GENERIC_MAPPINGS,
    }
}

/// Get native tree-sitter types that map to a given UAST type for a specific language.
///
/// This performs a reverse lookup from UAST schema types to native tree-sitter node types.
///
/// # Arguments
/// * `uast_type` - The UAST type to look up (e.g., "FunctionDeclaration")
/// * `language` - The target language (e.g., "rust", "c", "python")
///
/// # Returns
/// A vector of native tree-sitter node types that map to the UAST type.
///
/// # Examples
/// ```
/// use uast_core::uast::mappings::get_native_types_for_uast;
///
/// // Rust's function_item maps to FunctionDeclaration
/// let rust_types = get_native_types_for_uast("FunctionDeclaration", "rust");
/// assert!(rust_types.contains(&"function_item"));
///
/// // C's function_definition maps to FunctionDeclaration
/// let c_types = get_native_types_for_uast("FunctionDeclaration", "c");
/// assert!(c_types.contains(&"function_definition"));
/// ```
pub fn get_native_types_for_uast(uast_type: &str, language: &str) -> Vec<&'static str> {
    let mappings = get_mappings(language);
    mappings.get_native_types_for_uast(uast_type)
}

/// Check if a pattern looks like a UAST type (PascalCase) vs native type (snake_case).
///
/// UAST types use PascalCase (e.g., FunctionDeclaration, ClassDeclaration).
/// Native tree-sitter types use snake_case (e.g., function_item, class_definition).
///
/// # Arguments
/// * `pattern` - The pattern to check
///
/// # Returns
/// `true` if the pattern appears to be a UAST type (PascalCase), `false` otherwise.
pub fn is_uast_pattern(pattern: &str) -> bool {
    if pattern.is_empty() {
        return false;
    }

    // UAST types are PascalCase: start with uppercase, contain at least one lowercase
    let first_char = pattern.chars().next().unwrap();
    if !first_char.is_ascii_uppercase() {
        return false;
    }

    // Must contain at least one lowercase letter (to distinguish from constants)
    let has_lowercase = pattern.chars().any(|c| c.is_ascii_lowercase());

    // Should not contain underscores (snake_case indicator)
    let has_underscore = pattern.contains('_');

    has_lowercase && !has_underscore
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generic_mappings() {
        let mappings = get_mappings("unknown_language");
        assert_eq!(mappings.get("function_declaration"), "FunctionDeclaration");
        assert_eq!(mappings.get("class_declaration"), "TypeDeclaration");
        assert_eq!(mappings.get("identifier"), "Identifier");
        assert_eq!(mappings.get("nonexistent_type"), "Unknown");
    }

    #[test]
    fn test_javascript_mappings() {
        let mappings = get_mappings("javascript");
        assert_eq!(mappings.get("lexical_declaration"), "VariableDeclaration");
        assert_eq!(mappings.get("jsx_element"), "JsxElement");
        // Should also have generic mappings
        assert_eq!(mappings.get("identifier"), "Identifier");
    }

    #[test]
    fn test_python_mappings() {
        let mappings = get_mappings("python");
        assert_eq!(mappings.get("list_comprehension"), "ListComprehension");
        assert_eq!(mappings.get("dictionary"), "DictionaryExpression");
        // Should also have generic mappings
        assert_eq!(mappings.get("if_statement"), "IfStatement");
    }

    #[test]
    fn test_rust_mappings() {
        let mappings = get_mappings("rust");
        assert_eq!(mappings.get("function_item"), "FunctionDeclaration");
        assert_eq!(mappings.get("impl_item"), "ImplDeclaration");
        assert_eq!(mappings.get("match_expression"), "MatchExpression");
    }

    #[test]
    fn test_case_insensitive() {
        assert!(std::ptr::eq(get_mappings("JavaScript"), get_mappings("javascript")));
        assert!(std::ptr::eq(get_mappings("PYTHON"), get_mappings("python")));
    }

    #[test]
    fn test_reverse_lookup_rust() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "rust");
        assert!(native_types.contains(&"function_item"));

        let native_types = get_native_types_for_uast("TypeDeclaration", "rust");
        assert!(native_types.contains(&"struct_item"));
    }

    #[test]
    fn test_reverse_lookup_c() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "c");
        assert!(native_types.contains(&"function_definition"));

        let native_types = get_native_types_for_uast("EnumDeclaration", "c");
        assert!(native_types.contains(&"enum_specifier"));
    }

    #[test]
    fn test_reverse_lookup_python() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "python");
        assert!(native_types.contains(&"function_definition"));

        let native_types = get_native_types_for_uast("TypeDeclaration", "python");
        assert!(native_types.contains(&"class_definition"));
    }

    #[test]
    fn test_reverse_lookup_unknown_uast() {
        let native_types = get_native_types_for_uast("NonExistentType", "rust");
        assert!(native_types.is_empty());
    }

    #[test]
    fn test_is_uast_pattern() {
        // UAST patterns (PascalCase)
        assert!(is_uast_pattern("FunctionDeclaration"));
        assert!(is_uast_pattern("ClassDeclaration"));
        assert!(is_uast_pattern("TypeDeclaration"));
        assert!(is_uast_pattern("IfStatement"));
        assert!(is_uast_pattern("Block"));

        // Native patterns (snake_case)
        assert!(!is_uast_pattern("function_item"));
        assert!(!is_uast_pattern("class_declaration"));
        assert!(!is_uast_pattern("if_statement"));

        // Edge cases
        assert!(!is_uast_pattern("")); // empty
        assert!(!is_uast_pattern("lowercase")); // all lowercase
        assert!(!is_uast_pattern("ALLCAPS")); // no lowercase
        assert!(!is_uast_pattern("Pascal_Case")); // underscore
    }

    #[test]
    fn test_powershell_mappings() {
        let mappings = get_mappings("powershell");
        // PowerShell-specific mappings
        assert_eq!(mappings.get("function_statement"), "FunctionDeclaration");
        assert_eq!(mappings.get("class_statement"), "TypeDeclaration");
        assert_eq!(mappings.get("pipeline"), "PipelineExpression");
        assert_eq!(mappings.get("command"), "CommandExpression");
        assert_eq!(mappings.get("hash_literal_expression"), "HashtableExpression");
        assert_eq!(mappings.get("foreach_statement"), "ForEachStatement");
        assert_eq!(mappings.get("try_statement"), "TryStatement");
        assert_eq!(mappings.get("catch_clause"), "CatchClause");
        // Should also have generic mappings
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_powershell_aliases() {
        // Test all PowerShell language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("powershell"), get_mappings("ps1")));
        assert!(std::ptr::eq(get_mappings("powershell"), get_mappings("psm1")));
        assert!(std::ptr::eq(get_mappings("powershell"), get_mappings("psd1")));
        assert!(std::ptr::eq(get_mappings("powershell"), get_mappings("pwsh")));
    }

    #[test]
    fn test_reverse_lookup_powershell() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "powershell");
        assert!(native_types.contains(&"function_statement"));

        let native_types = get_native_types_for_uast("TypeDeclaration", "powershell");
        assert!(native_types.contains(&"class_statement"));

        let native_types = get_native_types_for_uast("PipelineExpression", "powershell");
        assert!(native_types.contains(&"pipeline"));

        let native_types = get_native_types_for_uast("CommandExpression", "powershell");
        assert!(native_types.contains(&"command"));
    }

    #[test]
    fn test_csharp_mappings() {
        let mappings = get_mappings("c-sharp");
        // C#-specific mappings
        assert_eq!(mappings.get("method_declaration"), "MethodDeclaration");
        assert_eq!(mappings.get("class_declaration"), "ClassDeclaration");
        assert_eq!(mappings.get("namespace_declaration"), "NamespaceDeclaration");
        assert_eq!(mappings.get("interface_declaration"), "InterfaceDeclaration");
        assert_eq!(mappings.get("property_declaration"), "PropertyDeclaration");
        assert_eq!(mappings.get("lambda_expression"), "LambdaExpression");
        assert_eq!(mappings.get("invocation_expression"), "CallExpression");
        assert_eq!(mappings.get("await_expression"), "AwaitExpression");
        assert_eq!(mappings.get("query_expression"), "QueryExpression");
        // Should also have generic mappings
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("for_statement"), "ForStatement");
    }

    #[test]
    fn test_csharp_aliases() {
        // Test all C# language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("c-sharp"), get_mappings("csharp")));
        assert!(std::ptr::eq(get_mappings("c-sharp"), get_mappings("cs")));
        assert!(std::ptr::eq(get_mappings("c-sharp"), get_mappings("c_sharp")));
    }

    #[test]
    fn test_reverse_lookup_csharp() {
        let native_types = get_native_types_for_uast("MethodDeclaration", "c-sharp");
        assert!(native_types.contains(&"method_declaration"));

        let native_types = get_native_types_for_uast("ClassDeclaration", "c-sharp");
        assert!(native_types.contains(&"class_declaration"));

        let native_types = get_native_types_for_uast("NamespaceDeclaration", "c-sharp");
        assert!(native_types.contains(&"namespace_declaration"));

        let native_types = get_native_types_for_uast("LambdaExpression", "c-sharp");
        assert!(native_types.contains(&"lambda_expression"));
    }
}
