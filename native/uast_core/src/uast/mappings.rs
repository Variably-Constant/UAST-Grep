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
    /// First checks explicit mappings, then falls back to heuristic inference.
    /// Returns "Unknown" only if no mapping or heuristic match exists.
    pub fn get(&self, ts_node_type: &str) -> &'static str {
        self.mappings
            .get(ts_node_type)
            .copied()
            .unwrap_or_else(|| infer_uast_kind(ts_node_type))
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

/// Heuristically infer a UAST kind from a tree-sitter node type name.
/// This provides automatic coverage for unknown languages/grammars by using
/// common naming patterns in tree-sitter grammars.
///
/// Pattern matching priority (first match wins):
/// 1. Exact suffix matches (most specific)
/// 2. Contains patterns (less specific)
/// 3. Prefix matches (least specific)
///
/// This function is public to allow external use and testing.
pub fn infer_uast_kind(ts_node_type: &str) -> &'static str {
    let lower = ts_node_type.to_lowercase();

    // === DECLARATIONS ===
    // Function-like declarations
    if lower.ends_with("_declaration") || lower.ends_with("_definition") {
        if lower.contains("function") || lower.contains("method") || lower.contains("proc")
            || lower.contains("sub") || lower.contains("def") {
            return "FunctionDeclaration";
        }
        if lower.contains("class") || lower.contains("struct") || lower.contains("type")
            || lower.contains("record") || lower.contains("message") {
            return "TypeDeclaration";
        }
        if lower.contains("interface") || lower.contains("protocol") || lower.contains("trait") {
            return "InterfaceDeclaration";
        }
        if lower.contains("enum") {
            return "EnumDeclaration";
        }
        if lower.contains("variable") || lower.contains("const") || lower.contains("let")
            || lower.contains("var") || lower.contains("field") {
            return "VariableDeclaration";
        }
        if lower.contains("module") || lower.contains("package") || lower.contains("namespace") {
            return "ModuleDeclaration";
        }
        if lower.contains("import") {
            return "ImportDeclaration";
        }
        return "Declaration";
    }

    // === STATEMENTS ===
    if lower.ends_with("_statement") || lower.ends_with("_stmt") {
        if lower.contains("if") {
            return "IfStatement";
        }
        if lower.contains("for") || lower.contains("foreach") || lower.contains("each") {
            if lower.contains("each") || lower.contains("in") {
                return "ForEachStatement";
            }
            return "ForStatement";
        }
        if lower.contains("while") {
            return "WhileStatement";
        }
        if lower.contains("do") {
            return "DoWhileStatement";
        }
        if lower.contains("switch") || lower.contains("case") || lower.contains("match") {
            return "SwitchStatement";
        }
        if lower.contains("try") {
            return "TryStatement";
        }
        if lower.contains("return") {
            return "ReturnStatement";
        }
        if lower.contains("break") {
            return "BreakStatement";
        }
        if lower.contains("continue") {
            return "ContinueStatement";
        }
        if lower.contains("throw") || lower.contains("raise") {
            return "ThrowStatement";
        }
        if lower.contains("import") {
            return "ImportStatement";
        }
        if lower.contains("export") {
            return "ExportStatement";
        }
        if lower.contains("assert") {
            return "AssertStatement";
        }
        return "Statement";
    }

    // === EXPRESSIONS ===
    if lower.ends_with("_expression") || lower.ends_with("_expr") {
        if lower.contains("call") || lower.contains("invoc") || lower.contains("apply") {
            return "CallExpression";
        }
        if lower.contains("binary") || lower.contains("infix") {
            return "BinaryExpression";
        }
        if lower.contains("unary") || lower.contains("prefix") || lower.contains("postfix") {
            return "UnaryExpression";
        }
        if lower.contains("assign") {
            return "AssignmentExpression";
        }
        if lower.contains("member") || lower.contains("access") || lower.contains("field") {
            return "MemberExpression";
        }
        if lower.contains("index") || lower.contains("subscript") {
            return "IndexExpression";
        }
        if lower.contains("lambda") || lower.contains("anonymous") || lower.contains("closure") {
            return "LambdaExpression";
        }
        if lower.contains("conditional") || lower.contains("ternary") {
            return "ConditionalExpression";
        }
        if lower.contains("if") {
            return "IfExpression";
        }
        if lower.contains("match") || lower.contains("case") || lower.contains("switch") {
            return "MatchExpression";
        }
        if lower.contains("for") {
            return "ForExpression";
        }
        if lower.contains("array") || lower.contains("list") || lower.contains("vec") {
            return "ArrayExpression";
        }
        if lower.contains("object") || lower.contains("dict") || lower.contains("map") || lower.contains("hash") {
            return "ObjectExpression";
        }
        if lower.contains("new") {
            return "NewExpression";
        }
        if lower.contains("await") {
            return "AwaitExpression";
        }
        if lower.contains("yield") {
            return "YieldExpression";
        }
        return "Expression";
    }

    // === LITERALS ===
    if lower.ends_with("_literal") || lower.ends_with("_lit") {
        if lower.contains("string") || lower.contains("str") {
            return "StringLiteral";
        }
        if lower.contains("number") || lower.contains("int") || lower.contains("float")
            || lower.contains("decimal") || lower.contains("numeric") {
            return "NumberLiteral";
        }
        if lower.contains("bool") || lower.contains("true") || lower.contains("false") {
            return "BooleanLiteral";
        }
        if lower.contains("null") || lower.contains("nil") || lower.contains("none") {
            return "NullLiteral";
        }
        if lower.contains("char") {
            return "CharacterLiteral";
        }
        if lower.contains("regex") || lower.contains("regexp") {
            return "RegexLiteral";
        }
        if lower.contains("array") || lower.contains("list") {
            return "ArrayLiteral";
        }
        if lower.contains("object") || lower.contains("dict") || lower.contains("map") {
            return "ObjectLiteral";
        }
        if lower.contains("symbol") || lower.contains("atom") {
            return "SymbolLiteral";
        }
        return "Literal";
    }

    // === CLAUSES ===
    if lower.ends_with("_clause") {
        if lower.contains("catch") || lower.contains("except") || lower.contains("rescue") {
            return "CatchClause";
        }
        if lower.contains("finally") || lower.contains("ensure") {
            return "FinallyClause";
        }
        if lower.contains("else") || lower.contains("elif") || lower.contains("elseif") {
            return "ElseClause";
        }
        if lower.contains("when") || lower.contains("guard") {
            return "GuardClause";
        }
        if lower.contains("where") {
            return "WhereClause";
        }
        if lower.contains("from") {
            return "FromClause";
        }
        return "Clause";
    }

    // === BLOCKS ===
    if lower.contains("block") || lower.contains("body") || lower == "compound_statement" {
        return "Block";
    }

    // === TYPES ===
    if lower.ends_with("_type") || lower.ends_with("type_") {
        if lower.contains("array") || lower.contains("list") {
            return "ArrayType";
        }
        if lower.contains("function") || lower.contains("callable") {
            return "FunctionType";
        }
        if lower.contains("generic") || lower.contains("parameterized") {
            return "GenericType";
        }
        if lower.contains("nullable") || lower.contains("optional") {
            return "NullableType";
        }
        if lower.contains("union") {
            return "UnionType";
        }
        if lower.contains("intersection") {
            return "IntersectionType";
        }
        return "TypeReference";
    }

    // === PATTERNS (for pattern matching) ===
    if lower.ends_with("_pattern") {
        if lower.contains("wildcard") || lower.contains("any") {
            return "WildcardPattern";
        }
        if lower.contains("tuple") {
            return "TuplePattern";
        }
        if lower.contains("array") || lower.contains("list") {
            return "ArrayPattern";
        }
        if lower.contains("object") || lower.contains("struct") {
            return "ObjectPattern";
        }
        return "Pattern";
    }

    // === SPECIFIC NODE TYPES (without suffix) ===
    // Control flow keywords
    if lower == "if" || lower == "if_expression" {
        return "IfStatement";
    }
    if lower == "else" || lower == "else_clause" {
        return "ElseClause";
    }
    if lower == "for" || lower == "for_in" {
        return "ForStatement";
    }
    if lower == "while" {
        return "WhileStatement";
    }
    if lower == "switch" || lower == "case" || lower == "match" {
        return "SwitchStatement";
    }
    if lower == "try" {
        return "TryStatement";
    }
    if lower == "catch" || lower == "except" {
        return "CatchClause";
    }
    if lower == "finally" {
        return "FinallyClause";
    }
    if lower == "return" {
        return "ReturnStatement";
    }
    if lower == "break" {
        return "BreakStatement";
    }
    if lower == "continue" {
        return "ContinueStatement";
    }

    // Literals
    if lower == "string" || lower == "string_content" || lower == "raw_string" {
        return "StringLiteral";
    }
    if lower == "number" || lower == "integer" || lower == "float" || lower == "decimal" {
        return "NumberLiteral";
    }
    if lower == "true" || lower == "false" || lower == "boolean" {
        return "BooleanLiteral";
    }
    if lower == "null" || lower == "nil" || lower == "none" {
        return "NullLiteral";
    }

    // Identifiers
    if lower == "identifier" || lower == "name" || lower == "symbol" || lower.ends_with("_name") {
        return "Identifier";
    }

    // Comments
    if lower.contains("comment") {
        return "Comment";
    }

    // Operators
    if lower.contains("operator") {
        if lower.contains("binary") {
            return "BinaryOperator";
        }
        if lower.contains("unary") {
            return "UnaryOperator";
        }
        return "Operator";
    }

    // Arguments and Parameters
    if lower.contains("argument") || lower.contains("arg") {
        return "Argument";
    }
    if lower.contains("parameter") || lower.contains("param") {
        return "Parameter";
    }

    // Common structures
    if lower == "array" || lower == "list" || lower == "sequence" {
        return "ArrayExpression";
    }
    if lower == "object" || lower == "dictionary" || lower == "map" || lower == "hash" {
        return "ObjectExpression";
    }
    if lower == "tuple" || lower == "pair" {
        return "TupleExpression";
    }

    // No heuristic match found
    "Unknown"
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
        ("for_range_loop", "ForEachStatement"),
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

/// Elixir-specific mappings
static ELIXIR_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific (these will override generic when there's conflict)
    pairs.extend_from_slice(&[
        // Functions and calls
        ("call", "CallExpression"),
        ("anonymous_function", "LambdaExpression"),
        ("stab_clause", "FunctionClause"),

        // Control flow
        ("do_block", "Block"),
        ("block", "Block"),
        ("rescue_block", "CatchClause"),
        ("catch_block", "CatchClause"),
        ("after_block", "FinallyClause"),
        ("else_block", "ElseClause"),

        // Literals
        ("atom", "SymbolLiteral"),
        ("boolean", "BooleanLiteral"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("string", "StringLiteral"),
        ("charlist", "StringLiteral"),
        ("char", "CharacterLiteral"),
        ("nil", "NullLiteral"),

        // Data structures
        ("list", "ArrayExpression"),
        ("tuple", "TupleExpression"),
        ("map", "DictionaryExpression"),
        ("struct", "ObjectExpression"),
        ("bitstring", "BinaryExpression"),
        ("keywords", "KeywordList"),
        ("pair", "KeyValuePair"),

        // Expressions
        ("binary_operator", "BinaryExpression"),
        ("unary_operator", "UnaryExpression"),
        ("identifier", "Identifier"),
        ("alias", "TypeReference"),
        ("dot", "MemberExpression"),
        ("access_call", "IndexExpression"),
        ("interpolation", "TemplateInterpolation"),
        ("sigil", "SigilExpression"),

        // Other
        ("comment", "Comment"),
        ("quoted_atom", "SymbolLiteral"),
        ("quoted_keyword", "Keyword"),
        ("operator_identifier", "OperatorIdentifier"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Erlang-specific mappings
static ERLANG_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Function definitions
        ("function_clause", "FunctionDeclaration"),
        ("fun_decl", "FunctionDeclaration"),
        ("anonymous_fun", "LambdaExpression"),
        ("fun_clause", "FunctionClause"),

        // Calls and expressions
        ("call", "CallExpression"),
        ("remote", "MemberExpression"),
        ("binary_op_expr", "BinaryExpression"),
        ("unary_op_expr", "UnaryExpression"),

        // Control flow
        ("case_expr", "SwitchExpression"),
        ("if_expr", "IfExpression"),
        ("try_expr", "TryStatement"),
        ("catch_expr", "CatchExpression"),
        ("receive_expr", "ReceiveExpression"),
        ("block_expr", "Block"),
        ("case_clause", "SwitchCase"),
        ("if_clause", "IfClause"),
        ("catch_clause", "CatchClause"),
        ("cr_clause", "MatchClause"),
        ("guard", "Guard"),
        ("guard_clause", "GuardClause"),
        ("clause_body", "Block"),

        // Data structures
        ("tuple", "TupleExpression"),
        ("list", "ArrayExpression"),
        ("binary", "BinaryExpression"),
        ("map_expr", "MapExpression"),
        ("record_expr", "RecordExpression"),

        // Literals
        ("atom", "SymbolLiteral"),
        ("string", "StringLiteral"),
        ("char", "CharacterLiteral"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("var", "Identifier"),

        // Type system
        ("type_alias", "TypeDeclaration"),
        ("type_sig", "TypeSignature"),
        ("spec", "TypeSpecification"),
        ("callback", "CallbackDeclaration"),
        ("record_decl", "RecordDeclaration"),

        // Module system
        ("module_attribute", "ModuleDeclaration"),
        ("export_attribute", "ExportDeclaration"),
        ("import_attribute", "ImportDeclaration"),
        ("behaviour_attribute", "BehaviourDeclaration"),

        // Comprehensions and generators
        ("list_comprehension", "ListComprehension"),
        ("binary_comprehension", "BinaryComprehension"),
        ("generator", "Generator"),
        ("b_generator", "BinaryGenerator"),

        // Other
        ("comment", "Comment"),
        ("arity", "Arity"),
        ("fa", "FunctionArity"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Elm-specific mappings
static ELM_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Function declarations
        ("function_declaration_left", "FunctionDeclaration"),
        ("anonymous_function_expr", "LambdaExpression"),

        // Function calls and expressions
        ("function_call_expr", "CallExpression"),
        ("bin_op_expr", "BinaryExpression"),
        ("negate_expr", "UnaryExpression"),
        ("parenthesized_expr", "ParenthesizedExpression"),
        ("field_access_expr", "MemberExpression"),
        ("field_accessor_function_expr", "FieldAccessor"),
        ("operator_as_function_expr", "OperatorFunction"),

        // Control flow
        ("if_else_expr", "IfExpression"),
        ("case_of_expr", "MatchExpression"),
        ("case_of_branch", "MatchArm"),
        ("let_in_expr", "LetExpression"),

        // Patterns
        ("lower_pattern", "IdentifierPattern"),
        ("anything_pattern", "WildcardPattern"),
        ("cons_pattern", "ConsPattern"),
        ("list_pattern", "ListPattern"),
        ("nullary_constructor_argument_pattern", "ConstructorPattern"),
        ("pattern", "Pattern"),

        // Types
        ("type_declaration", "TypeDeclaration"),
        ("type_alias_declaration", "TypeAliasDeclaration"),
        ("type_annotation", "TypeAnnotation"),
        ("field_type", "FieldType"),
        ("lower_type_name", "TypeVariable"),

        // Data structures
        ("list_expr", "ArrayExpression"),
        ("record_expr", "RecordExpression"),
        ("tuple_expr", "TupleExpression"),
        ("field", "Field"),

        // Literals
        ("number_literal", "NumberLiteral"),
        ("number_constant_expr", "NumberLiteral"),
        ("string_constant_expr", "StringLiteral"),
        ("char_constant_expr", "CharacterLiteral"),

        // Module system
        ("module_declaration", "ModuleDeclaration"),
        ("import_clause", "ImportDeclaration"),
        ("exposing_list", "ExposingList"),
        ("exposed_value", "ExposedValue"),
        ("exposed_type", "ExposedType"),

        // Identifiers
        ("lower_case_identifier", "Identifier"),
        ("upper_case_identifier", "TypeIdentifier"),
        ("operator_identifier", "OperatorIdentifier"),

        // Other
        ("line_comment", "Comment"),
        ("block_comment", "BlockComment"),
        ("infix_declaration", "InfixDeclaration"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Clojure-specific mappings
static CLOJURE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Lists and S-expressions
        ("list_lit", "ListExpression"),
        ("vec_lit", "ArrayExpression"),
        ("map_lit", "MapExpression"),
        ("set_lit", "SetExpression"),

        // Anonymous functions
        ("anon_fn_lit", "LambdaExpression"),

        // Literals
        ("sym_lit", "Identifier"),
        ("kwd_lit", "KeywordLiteral"),
        ("str_lit", "StringLiteral"),
        ("num_lit", "NumberLiteral"),
        ("bool_lit", "BooleanLiteral"),
        ("nil_lit", "NullLiteral"),
        ("char_lit", "CharacterLiteral"),
        ("regex_lit", "RegexLiteral"),

        // Quoting and macros
        ("quoting_lit", "QuoteExpression"),
        ("syn_quoting_lit", "SyntaxQuoteExpression"),
        ("unquoting_lit", "UnquoteExpression"),
        ("unquote_splicing_lit", "UnquoteSpliceExpression"),
        ("derefing_lit", "DerefExpression"),
        ("var_quoting_lit", "VarQuoteExpression"),
        ("evaling_lit", "EvalExpression"),

        // Metadata
        ("meta_lit", "MetadataExpression"),
        ("old_meta_lit", "MetadataExpression"),
        ("tagged_or_ctor_lit", "TaggedLiteral"),

        // Namespaces
        ("ns_map_lit", "NamespacedMapExpression"),

        // Reader conditionals
        ("read_cond_lit", "ReaderConditional"),
        ("splicing_read_cond_lit", "SplicingReaderConditional"),

        // Other
        ("comment", "Comment"),
        ("dis_expr", "DiscardExpression"),
        ("auto_res_mark", "AutoResolveMarker"),
        ("sym_val_lit", "SymbolValue"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// HCL (Terraform) specific mappings
static HCL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Core structure
        ("block", "Block"),
        ("body", "BlockBody"),
        ("attribute", "VariableDeclaration"),

        // Expressions
        ("expression", "Expression"),
        ("function_call", "CallExpression"),
        ("conditional", "ConditionalExpression"),
        ("binary_operation", "BinaryExpression"),
        ("operation", "BinaryExpression"),

        // Data structures
        ("object", "ObjectExpression"),
        ("object_elem", "ObjectProperty"),
        ("tuple", "ArrayExpression"),
        ("collection_value", "CollectionExpression"),

        // Literals
        ("string_lit", "StringLiteral"),
        ("numeric_lit", "NumberLiteral"),
        ("bool_lit", "BooleanLiteral"),
        ("null_lit", "NullLiteral"),
        ("literal_value", "Literal"),

        // Templates
        ("heredoc_template", "HeredocTemplate"),
        ("quoted_template", "StringTemplate"),
        ("template_expr", "TemplateExpression"),
        ("template_interpolation", "TemplateInterpolation"),
        ("template_directive", "TemplateDirective"),
        ("template_for", "TemplateForDirective"),

        // Access and traversal
        ("get_attr", "MemberExpression"),
        ("index", "IndexExpression"),
        ("splat", "SplatExpression"),
        ("attr_splat", "AttributeSplat"),
        ("full_splat", "FullSplat"),

        // For expressions
        ("for_expr", "ForExpression"),
        ("for_tuple_expr", "ForTupleExpression"),
        ("for_object_expr", "ForObjectExpression"),
        ("for_intro", "ForIntro"),
        ("for_cond", "ForCondition"),

        // Identifiers
        ("identifier", "Identifier"),

        // Other
        ("comment", "Comment"),
        ("ellipsis", "Ellipsis"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// GraphQL-specific mappings
static GRAPHQL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Type definitions
        ("object_type_definition", "TypeDeclaration"),
        ("interface_type_definition", "InterfaceDeclaration"),
        ("enum_type_definition", "EnumDeclaration"),
        ("union_type_definition", "UnionDeclaration"),
        ("scalar_type_definition", "ScalarDeclaration"),
        ("input_object_type_definition", "InputTypeDeclaration"),

        // Field definitions
        ("field_definition", "FieldDeclaration"),
        ("input_value_definition", "InputValueDeclaration"),
        ("argument", "Argument"),
        ("arguments", "ArgumentList"),
        ("arguments_definition", "ParameterList"),

        // Operations
        ("operation_definition", "OperationDefinition"),
        ("fragment_definition", "FragmentDeclaration"),
        ("directive_definition", "DirectiveDeclaration"),
        ("schema_definition", "SchemaDeclaration"),

        // Selections
        ("selection_set", "SelectionSet"),
        ("selection", "Selection"),
        ("field", "Field"),
        ("fragment_spread", "FragmentSpread"),
        ("inline_fragment", "InlineFragment"),

        // Types
        ("named_type", "TypeReference"),
        ("list_type", "ListType"),
        ("non_null_type", "NonNullType"),
        ("type", "TypeReference"),

        // Values and literals
        ("string_value", "StringLiteral"),
        ("int_value", "NumberLiteral"),
        ("float_value", "NumberLiteral"),
        ("boolean_value", "BooleanLiteral"),
        ("null_value", "NullLiteral"),
        ("enum_value", "EnumValue"),
        ("list_value", "ArrayExpression"),
        ("object_value", "ObjectExpression"),
        ("object_field", "ObjectField"),

        // Variables
        ("variable", "Variable"),
        ("variable_definition", "VariableDeclaration"),
        ("variable_definitions", "VariableDeclarationList"),
        ("default_value", "DefaultValue"),

        // Directives
        ("directive", "Directive"),
        ("directives", "DirectiveList"),
        ("directive_location", "DirectiveLocation"),
        ("directive_locations", "DirectiveLocationList"),

        // Extensions
        ("type_extension", "TypeExtension"),
        ("schema_extension", "SchemaExtension"),
        ("object_type_extension", "TypeExtension"),
        ("interface_type_extension", "InterfaceExtension"),
        ("enum_type_extension", "EnumExtension"),
        ("union_type_extension", "UnionExtension"),
        ("scalar_type_extension", "ScalarExtension"),
        ("input_object_type_extension", "InputTypeExtension"),

        // Other
        ("name", "Identifier"),
        ("alias", "Alias"),
        ("description", "Description"),
        ("comment", "Comment"),
        ("implements_interfaces", "ImplementsClause"),
        ("union_member_types", "UnionMemberTypes"),
        ("enum_values_definition", "EnumValueList"),
        ("enum_value_definition", "EnumMember"),
        ("fields_definition", "FieldList"),
        ("input_fields_definition", "InputFieldList"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Protobuf-specific mappings
static PROTOBUF_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Type definitions
        ("message", "TypeDeclaration"),
        ("enum", "EnumDeclaration"),
        ("service", "ServiceDeclaration"),

        // Field definitions
        ("field", "FieldDeclaration"),
        ("enum_field", "EnumMember"),
        ("map_field", "MapFieldDeclaration"),
        ("oneof", "OneofDeclaration"),
        ("oneof_field", "OneofFieldDeclaration"),

        // Methods and RPCs
        ("rpc", "MethodDeclaration"),

        // Package and imports
        ("package", "PackageDeclaration"),
        ("import", "ImportDeclaration"),
        ("syntax", "SyntaxDeclaration"),

        // Options
        ("option", "OptionDeclaration"),
        ("field_option", "FieldOption"),
        ("field_options", "FieldOptionList"),
        ("enum_value_option", "EnumValueOption"),

        // Bodies
        ("message_body", "Block"),
        ("enum_body", "EnumBody"),

        // Types and names
        ("type", "TypeReference"),
        ("message_or_enum_type", "TypeReference"),
        ("key_type", "KeyType"),
        ("message_name", "TypeIdentifier"),
        ("enum_name", "TypeIdentifier"),
        ("service_name", "TypeIdentifier"),
        ("rpc_name", "MethodIdentifier"),

        // Literals
        ("string", "StringLiteral"),
        ("int_lit", "NumberLiteral"),
        ("float_lit", "NumberLiteral"),
        ("decimal_lit", "NumberLiteral"),
        ("hex_lit", "NumberLiteral"),
        ("octal_lit", "NumberLiteral"),
        ("bool", "BooleanLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),

        // Other
        ("identifier", "Identifier"),
        ("full_ident", "QualifiedIdentifier"),
        ("constant", "Constant"),
        ("field_number", "FieldNumber"),
        ("reserved", "ReservedDeclaration"),
        ("range", "Range"),
        ("ranges", "RangeList"),
        ("field_names", "FieldNameList"),
        ("empty_statement", "EmptyStatement"),
        ("comment", "Comment"),
        ("block_lit", "BlockLiteral"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// HTML-specific mappings
static HTML_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add HTML-specific
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),
        ("doctype", "Doctype"),

        // Elements
        ("element", "Element"),
        ("start_tag", "StartTag"),
        ("end_tag", "EndTag"),
        ("self_closing_tag", "SelfClosingTag"),
        ("tag_name", "TagName"),
        ("script_element", "ScriptElement"),
        ("style_element", "StyleElement"),
        ("script_start_tag", "ScriptStartTag"),
        ("style_start_tag", "StyleStartTag"),

        // Attributes
        ("attribute", "Attribute"),
        ("attribute_name", "AttributeName"),
        ("attribute_value", "AttributeValue"),
        ("quoted_attribute_value", "QuotedAttributeValue"),

        // Content
        ("text", "Text"),
        ("raw_text", "RawText"),
        ("entity", "Entity"),

        // Errors (for recovery)
        ("erroneous_end_tag", "ErroneousEndTag"),
        ("erroneous_end_tag_name", "ErroneousEndTagName"),

        // Comments
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// XML-specific mappings
static XML_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add XML-specific (note: XML grammar uses different naming conventions)
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),
        ("prolog", "Prolog"),
        ("XMLDecl", "XmlDeclaration"),
        ("doctypedecl", "Doctype"),

        // Elements
        ("element", "Element"),
        ("STag", "StartTag"),
        ("ETag", "EndTag"),
        ("EmptyElemTag", "SelfClosingElement"),
        ("content", "Content"),
        ("children", "Children"),

        // Element declarations (DTD)
        ("elementdecl", "ElementDeclaration"),
        ("contentspec", "ContentSpec"),

        // Attributes (note: XML grammar uses PascalCase for some nodes)
        ("Attribute", "Attribute"),
        ("AttValue", "AttributeValue"),
        ("Name", "Name"),

        // CDATA and text
        ("CData", "CDataSection"),
        ("CDSect", "CDataSection"),
        ("CharData", "CharData"),

        // Comments
        ("Comment", "Comment"),
        ("comment", "Comment"),

        // Processing instructions
        ("PI", "ProcessingInstruction"),

        // Entity references
        ("EntityRef", "EntityReference"),
        ("CharRef", "CharacterReference"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// CSS-specific mappings
static CSS_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add CSS-specific
    pairs.extend_from_slice(&[
        // Stylesheet structure
        ("stylesheet", "Stylesheet"),
        ("rule_set", "RuleSet"),
        ("block", "Block"),

        // Selectors
        ("selectors", "Selectors"),
        ("class_selector", "ClassSelector"),
        ("id_selector", "IdSelector"),
        ("attribute_selector", "AttributeSelector"),
        ("pseudo_class_selector", "PseudoClassSelector"),
        ("pseudo_element_selector", "PseudoElementSelector"),
        ("universal_selector", "UniversalSelector"),
        ("nesting_selector", "NestingSelector"),
        ("namespace_selector", "NamespaceSelector"),

        // Selector combinators
        ("descendant_selector", "DescendantSelector"),
        ("child_selector", "ChildSelector"),
        ("sibling_selector", "SiblingSelector"),
        ("adjacent_sibling_selector", "AdjacentSiblingSelector"),

        // Selector arguments
        ("pseudo_class_arguments", "PseudoClassArguments"),
        ("pseudo_class_nth_child_arguments", "PseudoClassNthChildArguments"),
        ("pseudo_class_with_selector_arguments", "PseudoClassWithSelectorArguments"),
        ("pseudo_element_arguments", "PseudoElementArguments"),

        // Declarations
        ("declaration", "Declaration"),
        ("last_declaration", "Declaration"),
        ("property_name", "PropertyName"),
        ("important", "Important"),

        // At-rules
        ("at_rule", "AtRule"),
        ("at_keyword", "AtKeyword"),
        ("media_statement", "MediaQuery"),
        ("import_statement", "ImportDeclaration"),
        ("charset_statement", "CharsetDeclaration"),
        ("namespace_statement", "NamespaceDeclaration"),
        ("supports_statement", "SupportsRule"),
        ("keyframes_statement", "KeyframesRule"),
        ("scope_statement", "ScopeRule"),
        ("postcss_statement", "PostCssStatement"),

        // Keyframes
        ("keyframe_block_list", "KeyframeBlockList"),
        ("keyframe_block", "KeyframeBlock"),
        ("from", "FromKeyword"),
        ("to", "ToKeyword"),

        // Queries
        ("feature_query", "FeatureQuery"),
        ("binary_query", "BinaryQuery"),
        ("unary_query", "UnaryQuery"),
        ("parenthesized_query", "ParenthesizedQuery"),
        ("selector_query", "SelectorQuery"),

        // Values
        ("plain_value", "PlainValue"),
        ("color_value", "ColorValue"),
        ("integer_value", "IntegerValue"),
        ("float_value", "FloatValue"),
        ("string_value", "StringLiteral"),
        ("grid_value", "GridValue"),
        ("parenthesized_value", "ParenthesizedValue"),
        ("important_value", "ImportantValue"),
        ("unit", "Unit"),

        // Functions and expressions
        ("call_expression", "CallExpression"),
        ("binary_expression", "BinaryExpression"),
        ("arguments", "ArgumentList"),

        // Identifiers
        ("identifier", "Identifier"),
        ("class_name", "ClassName"),

        // Comments
        ("comment", "Comment"),
        ("js_comment", "JsComment"),

        // Other
        ("escape_sequence", "EscapeSequence"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Nix-specific mappings (functional package management language)
static NIX_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Function expressions (Nix is expression-based)
        ("function_expression", "LambdaExpression"),
        ("apply_expression", "CallExpression"),
        ("formal", "Parameter"),
        ("formals", "ParameterList"),

        // Expressions
        ("let_expression", "LetExpression"),
        ("if_expression", "IfExpression"),
        ("with_expression", "WithExpression"),
        ("assert_expression", "AssertExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("select_expression", "MemberExpression"),
        ("has_attr_expression", "HasAttributeExpression"),

        // Data structures
        ("attrset_expression", "ObjectExpression"),
        ("rec_attrset_expression", "ObjectExpression"),
        ("let_attrset_expression", "ObjectExpression"),
        ("list_expression", "ArrayExpression"),
        ("binding", "PropertyDeclaration"),
        ("binding_set", "PropertyList"),
        ("attrpath", "PropertyPath"),
        ("inherit", "InheritDeclaration"),
        ("inherit_from", "InheritFromDeclaration"),
        ("inherited_attrs", "InheritedAttributes"),

        // Literals
        ("string_expression", "StringLiteral"),
        ("indented_string_expression", "StringLiteral"),
        ("integer_expression", "NumberLiteral"),
        ("float_expression", "NumberLiteral"),
        ("path_expression", "PathLiteral"),
        ("hpath_expression", "HomePathLiteral"),
        ("spath_expression", "SearchPathLiteral"),
        ("uri_expression", "UriLiteral"),
        ("variable_expression", "Identifier"),

        // String parts
        ("string_fragment", "StringFragment"),
        ("path_fragment", "PathFragment"),
        ("interpolation", "TemplateInterpolation"),
        ("dollar_escape", "EscapeSequence"),
        ("escape_sequence", "EscapeSequence"),

        // Identifiers
        ("identifier", "Identifier"),
        ("ellipses", "RestParameter"),

        // Other
        ("source_code", "SourceFile"),
        ("comment", "Comment"),
        ("keyword", "Keyword"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Bicep-specific mappings (Azure infrastructure-as-code)
static BICEP_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Declarations (Bicep-specific infrastructure types)
        ("resource_declaration", "ResourceDeclaration"),
        ("module_declaration", "ModuleDeclaration"),
        ("parameter_declaration", "ParameterDeclaration"),
        ("output_declaration", "OutputDeclaration"),
        ("variable_declaration", "VariableDeclaration"),
        ("type_declaration", "TypeDeclaration"),
        ("metadata_declaration", "MetadataDeclaration"),
        ("target_scope_assignment", "TargetScopeDeclaration"),
        ("user_defined_function", "FunctionDeclaration"),

        // Control flow
        ("for_statement", "ForStatement"),
        ("for_loop_parameters", "ForLoopParameters"),
        ("if_statement", "IfStatement"),
        ("assert_statement", "AssertStatement"),

        // Imports
        ("import_statement", "ImportDeclaration"),
        ("import_with_statement", "ImportWithDeclaration"),
        ("import_functionality", "ImportFunctionality"),
        ("using_statement", "UsingDeclaration"),

        // Expressions
        ("call_expression", "CallExpression"),
        ("member_expression", "MemberExpression"),
        ("subscript_expression", "IndexExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("lambda_expression", "LambdaExpression"),
        ("resource_expression", "ResourceExpression"),
        ("assignment_expression", "AssignmentExpression"),

        // Data structures
        ("object", "ObjectExpression"),
        ("object_property", "ObjectProperty"),
        ("array", "ArrayExpression"),

        // Types
        ("type", "TypeReference"),
        ("primitive_type", "PrimitiveType"),
        ("array_type", "ArrayType"),
        ("nullable_type", "NullableType"),
        ("negated_type", "NegatedType"),
        ("parenthesized_type", "ParenthesizedType"),
        ("parameterized_type", "GenericType"),
        ("union_type", "UnionType"),
        ("type_arguments", "TypeArguments"),

        // Parameters and arguments
        ("parameters", "ParameterList"),
        ("parameter", "Parameter"),
        ("arguments", "ArgumentList"),

        // Literals
        ("string", "StringLiteral"),
        ("string_content", "StringContent"),
        ("multiline_string_content", "StringContent"),
        ("number", "NumberLiteral"),
        ("boolean", "BooleanLiteral"),
        ("null", "NullLiteral"),
        ("interpolation", "TemplateInterpolation"),
        ("escape_sequence", "EscapeSequence"),

        // Identifiers
        ("identifier", "Identifier"),
        ("compatible_identifier", "Identifier"),

        // Decorators
        ("decorator", "Decorator"),
        ("decorators", "DecoratorList"),

        // Test blocks
        ("test_block", "TestBlock"),

        // Other
        ("infrastructure", "SourceFile"),
        ("statement", "Statement"),
        ("declaration", "Declaration"),
        ("expression", "Expression"),
        ("primary_expression", "PrimaryExpression"),
        ("comment", "Comment"),
        ("diagnostic_comment", "DiagnosticComment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Dockerfile-specific mappings (container build instructions)
static DOCKERFILE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add language-specific
    pairs.extend_from_slice(&[
        // Core instructions
        ("from_instruction", "FromInstruction"),
        ("run_instruction", "RunInstruction"),
        ("cmd_instruction", "CmdInstruction"),
        ("entrypoint_instruction", "EntrypointInstruction"),
        ("copy_instruction", "CopyInstruction"),
        ("add_instruction", "AddInstruction"),
        ("env_instruction", "EnvInstruction"),
        ("arg_instruction", "ArgInstruction"),
        ("expose_instruction", "ExposeInstruction"),
        ("volume_instruction", "VolumeInstruction"),
        ("workdir_instruction", "WorkdirInstruction"),
        ("user_instruction", "UserInstruction"),
        ("label_instruction", "LabelInstruction"),
        ("stopsignal_instruction", "StopsignalInstruction"),
        ("healthcheck_instruction", "HealthcheckInstruction"),
        ("shell_instruction", "ShellInstruction"),
        ("maintainer_instruction", "MaintainerInstruction"),
        ("onbuild_instruction", "OnbuildInstruction"),
        ("cross_build_instruction", "CrossBuildInstruction"),

        // Image specification
        ("image_spec", "ImageSpec"),
        ("image_name", "ImageName"),
        ("image_tag", "ImageTag"),
        ("image_digest", "ImageDigest"),
        ("image_alias", "ImageAlias"),

        // Shell commands
        ("shell_command", "ShellCommand"),
        ("shell_fragment", "ShellFragment"),

        // String types
        ("double_quoted_string", "StringLiteral"),
        ("single_quoted_string", "StringLiteral"),
        ("unquoted_string", "StringLiteral"),
        ("json_string", "StringLiteral"),
        ("json_string_array", "ArrayExpression"),

        // Environment and arguments
        ("env_pair", "EnvironmentPair"),
        ("label_pair", "LabelPair"),
        ("param", "Parameter"),
        ("mount_param", "MountParameter"),
        ("mount_param_param", "MountParameterParam"),

        // Paths
        ("path", "PathExpression"),
        ("path_with_heredoc", "PathWithHeredoc"),

        // Variable expansion
        ("expansion", "VariableExpansion"),
        ("variable", "Variable"),

        // Port exposure
        ("expose_port", "ExposePort"),

        // Heredoc support
        ("heredoc_block", "HeredocBlock"),
        ("heredoc_line", "HeredocLine"),
        ("heredoc_marker", "HeredocMarker"),
        ("heredoc_end", "HeredocEnd"),
        ("heredoc_nl", "HeredocNewline"),

        // Escape sequences
        ("double_quoted_escape_sequence", "EscapeSequence"),
        ("single_quoted_escape_sequence", "EscapeSequence"),
        ("json_escape_sequence", "EscapeSequence"),

        // Line continuation
        ("line_continuation", "LineContinuation"),
        ("required_line_continuation", "LineContinuation"),

        // Other
        ("source_file", "SourceFile"),
        ("comment", "Comment"),
        ("error_sentinel", "ErrorSentinel"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Bash/Shell specific mappings
static BASH_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Bash-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),
        ("compound_statement", "Block"),
        ("do_group", "Block"),
        ("subshell", "SubshellExpression"),

        // Function definition
        ("function_definition", "FunctionDeclaration"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"),
        ("elif_clause", "ElseIfClause"),
        ("else_clause", "ElseClause"),
        ("case_statement", "SwitchStatement"),
        ("case_item", "SwitchCase"),
        ("last_case_item", "SwitchCase"),
        ("test_command", "TestExpression"),

        // Control flow - loops
        ("for_statement", "ForStatement"),
        ("c_style_for_statement", "ForStatement"),
        ("while_statement", "WhileStatement"),

        // Commands and pipelines
        ("command", "CallExpression"),
        ("command_name", "CommandName"),
        ("pipeline", "PipelineExpression"),
        ("redirected_statement", "RedirectedStatement"),
        ("negated_command", "UnaryExpression"),
        ("list", "CommandList"),

        // Expressions
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("postfix_expression", "UpdateExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("arithmetic_expansion", "ArithmeticExpression"),
        ("command_substitution", "CommandSubstitution"),
        ("process_substitution", "ProcessSubstitution"),
        ("brace_expression", "BraceExpression"),

        // Variables and assignments
        ("variable_assignment", "AssignmentExpression"),
        ("variable_assignments", "AssignmentList"),
        ("variable_name", "Identifier"),
        ("expansion", "VariableExpansion"),
        ("simple_expansion", "VariableExpansion"),
        ("subscript", "IndexExpression"),
        ("declaration_command", "VariableDeclaration"),
        ("unset_command", "UnsetStatement"),

        // Literals and strings
        ("word", "Identifier"),
        ("string", "StringLiteral"),
        ("raw_string", "StringLiteral"),
        ("ansi_c_string", "StringLiteral"),
        ("translated_string", "StringLiteral"),
        ("string_content", "StringContent"),
        ("concatenation", "ConcatenationExpression"),
        ("number", "NumberLiteral"),
        ("regex", "RegexLiteral"),
        ("extglob_pattern", "GlobPattern"),

        // Array
        ("array", "ArrayExpression"),

        // Heredoc
        ("heredoc_body", "HeredocBody"),
        ("heredoc_content", "HeredocContent"),
        ("heredoc_redirect", "HeredocRedirect"),
        ("heredoc_start", "HeredocStart"),
        ("heredoc_end", "HeredocEnd"),
        ("simple_heredoc_body", "HeredocBody"),
        ("herestring_redirect", "HerestringRedirect"),

        // File redirects
        ("file_redirect", "FileRedirect"),
        ("file_descriptor", "FileDescriptor"),

        // Other
        ("comment", "Comment"),
        ("test_operator", "TestOperator"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Lua specific mappings
static LUA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Lua-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("chunk", "SourceFile"),

        // Function definitions
        ("function_declaration", "FunctionDeclaration"),
        ("function_definition", "FunctionDeclaration"),
        ("function_call", "CallExpression"),
        ("parameters", "ParameterList"),
        ("arguments", "ArgumentList"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"),
        ("elseif_statement", "ElseIfClause"),
        ("else_statement", "ElseClause"),

        // Control flow - loops
        ("for_statement", "ForStatement"),
        ("for_generic_clause", "ForInClause"),
        ("for_numeric_clause", "ForNumericClause"),
        ("while_statement", "WhileStatement"),
        ("repeat_statement", "DoWhileStatement"),
        ("do_statement", "DoStatement"),

        // Control flow - jumps
        ("break_statement", "BreakStatement"),
        ("return_statement", "ReturnStatement"),
        ("goto_statement", "GotoStatement"),
        ("label_statement", "LabelStatement"),

        // Expressions
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("vararg_expression", "VarargExpression"),

        // Variables and assignments
        ("variable_declaration", "VariableDeclaration"),
        ("assignment_statement", "AssignmentExpression"),
        ("declaration", "Declaration"),
        ("variable", "Variable"),

        // Tables and indices
        ("table_constructor", "ObjectExpression"),
        ("field", "ObjectProperty"),
        ("bracket_index_expression", "IndexExpression"),
        ("dot_index_expression", "MemberExpression"),
        ("method_index_expression", "MethodExpression"),

        // Identifiers and literals
        ("identifier", "Identifier"),
        ("string", "StringLiteral"),
        ("number", "NumberLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("nil", "NullLiteral"),

        // Other
        ("comment", "Comment"),
        ("hash_bang_line", "ShebangLine"),
        ("expression", "Expression"),
        ("statement", "Statement"),
        ("empty_statement", "EmptyStatement"),
        ("escape_sequence", "EscapeSequence"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// PHP specific mappings
static PHP_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add PHP-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),
        ("php_tag", "PhpTag"),
        ("php_end_tag", "PhpEndTag"),
        ("text_interpolation", "TextInterpolation"),
        ("text", "Text"),
        ("compound_statement", "Block"),
        ("declaration_list", "DeclarationList"),
        ("colon_block", "ColonBlock"),

        // Function and method definitions
        ("function_definition", "FunctionDeclaration"),
        ("method_declaration", "MethodDeclaration"),
        ("anonymous_function", "LambdaExpression"),
        ("arrow_function", "ArrowFunction"),
        ("formal_parameters", "ParameterList"),
        ("simple_parameter", "Parameter"),
        ("variadic_parameter", "RestParameter"),
        ("property_promotion_parameter", "PropertyPromotionParameter"),
        ("anonymous_function_use_clause", "UseClause"),

        // Class and type declarations
        ("class_declaration", "TypeDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("trait_declaration", "TraitDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("enum_declaration_list", "EnumDeclarationList"),
        ("enum_case", "EnumCase"),
        ("anonymous_class", "AnonymousClassExpression"),
        ("base_clause", "ExtendsClause"),
        ("class_interface_clause", "ImplementsClause"),

        // Namespace and imports
        ("namespace_definition", "NamespaceDeclaration"),
        ("namespace_name", "NamespaceName"),
        ("namespace_use_declaration", "ImportDeclaration"),
        ("namespace_use_clause", "ImportClause"),
        ("namespace_use_group", "ImportGroup"),
        ("use_declaration", "UseDeclaration"),
        ("use_list", "UseList"),
        ("use_as_clause", "UseAsClause"),
        ("use_instead_of_clause", "UseInsteadOfClause"),
        ("qualified_name", "QualifiedName"),
        ("relative_name", "RelativeName"),

        // Properties and constants
        ("property_declaration", "PropertyDeclaration"),
        ("property_element", "PropertyElement"),
        ("property_hook", "PropertyHook"),
        ("property_hook_list", "PropertyHookList"),
        ("const_declaration", "ConstDeclaration"),
        ("static_variable_declaration", "StaticVariableDeclaration"),
        ("global_declaration", "GlobalDeclaration"),
        ("function_static_declaration", "FunctionStaticDeclaration"),

        // Attributes
        ("attribute_list", "AttributeList"),
        ("attribute_group", "AttributeGroup"),
        ("attribute", "Attribute"),

        // Modifiers
        ("visibility_modifier", "VisibilityModifier"),
        ("abstract_modifier", "AbstractModifier"),
        ("final_modifier", "FinalModifier"),
        ("static_modifier", "StaticModifier"),
        ("readonly_modifier", "ReadonlyModifier"),
        ("var_modifier", "VarModifier"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"),
        ("else_if_clause", "ElseIfClause"),
        ("else_clause", "ElseClause"),
        ("switch_statement", "SwitchStatement"),
        ("switch_block", "SwitchBlock"),
        ("case_statement", "CaseClause"),
        ("default_statement", "DefaultClause"),
        ("match_expression", "MatchExpression"),
        ("match_block", "MatchBlock"),
        ("match_conditional_expression", "MatchConditionalExpression"),
        ("match_default_expression", "MatchDefaultExpression"),
        ("match_condition_list", "MatchConditionList"),

        // Control flow - loops
        ("for_statement", "ForStatement"),
        ("foreach_statement", "ForEachStatement"),
        ("foreach_pair", "ForEachPair"),
        ("while_statement", "WhileStatement"),
        ("do_statement", "DoWhileStatement"),

        // Control flow - jumps
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("return_statement", "ReturnStatement"),
        ("goto_statement", "GotoStatement"),
        ("named_label_statement", "LabelStatement"),

        // Error handling
        ("try_statement", "TryStatement"),
        ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"),
        ("throw_expression", "ThrowExpression"),

        // Declare statement
        ("declare_statement", "DeclareStatement"),
        ("declare_directive", "DeclareDirective"),

        // Expressions
        ("assignment_expression", "AssignmentExpression"),
        ("augmented_assignment_expression", "AugmentedAssignmentExpression"),
        ("reference_assignment_expression", "ReferenceAssignmentExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_op_expression", "UnaryExpression"),
        ("conditional_expression", "ConditionalExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("cast_expression", "CastExpression"),
        ("cast_type", "CastType"),
        ("clone_expression", "CloneExpression"),
        ("error_suppression_expression", "ErrorSuppressionExpression"),
        ("update_expression", "UpdateExpression"),
        ("sequence_expression", "SequenceExpression"),

        // Call expressions
        ("function_call_expression", "CallExpression"),
        ("member_call_expression", "MethodCallExpression"),
        ("scoped_call_expression", "StaticMethodCallExpression"),
        ("nullsafe_member_call_expression", "NullsafeMemberCallExpression"),
        ("arguments", "ArgumentList"),
        ("argument", "Argument"),

        // Member access
        ("member_access_expression", "MemberExpression"),
        ("nullsafe_member_access_expression", "NullsafeMemberAccessExpression"),
        ("scoped_property_access_expression", "StaticPropertyAccessExpression"),
        ("class_constant_access_expression", "ClassConstantAccessExpression"),

        // Object creation
        ("object_creation_expression", "NewExpression"),

        // Array
        ("array_creation_expression", "ArrayExpression"),
        ("array_element_initializer", "ArrayElement"),
        ("list_literal", "ListLiteral"),

        // Variables
        ("variable_name", "Identifier"),
        ("dynamic_variable_name", "DynamicVariableName"),
        ("by_ref", "ByReference"),
        ("reference_modifier", "ReferenceModifier"),
        ("variadic_unpacking", "SpreadElement"),
        ("variadic_placeholder", "VariadicPlaceholder"),

        // Literals
        ("string", "StringLiteral"),
        ("string_content", "StringContent"),
        ("encapsed_string", "InterpolatedStringLiteral"),
        ("encapsed_string_chars", "StringChars"),
        ("encapsed_string_chars_after_variable", "StringChars"),
        ("encapsed_string_chars_heredoc", "StringChars"),
        ("encapsed_string_chars_after_variable_heredoc", "StringChars"),
        ("heredoc", "HeredocLiteral"),
        ("heredoc_body", "HeredocBody"),
        ("heredoc_start", "HeredocStart"),
        ("heredoc_end", "HeredocEnd"),
        ("nowdoc", "NowdocLiteral"),
        ("nowdoc_body", "NowdocBody"),
        ("nowdoc_string", "NowdocString"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("boolean", "BooleanLiteral"),
        ("null", "NullLiteral"),
        ("escape_sequence", "EscapeSequence"),

        // Types
        ("type", "TypeReference"),
        ("type_list", "TypeList"),
        ("named_type", "NamedType"),
        ("primitive_type", "PrimitiveType"),
        ("optional_type", "OptionalType"),
        ("union_type", "UnionType"),
        ("intersection_type", "IntersectionType"),
        ("disjunctive_normal_form_type", "DisjunctiveNormalFormType"),
        ("bottom_type", "NeverType"),
        ("relative_scope", "RelativeScope"),
        ("cast_variable", "CastVariable"),

        // Include/Require
        ("include_expression", "IncludeExpression"),
        ("include_once_expression", "IncludeOnceExpression"),
        ("require_expression", "RequireExpression"),
        ("require_once_expression", "RequireOnceExpression"),

        // Other statements
        ("expression_statement", "ExpressionStatement"),
        ("echo_statement", "EchoStatement"),
        ("exit_statement", "ExitStatement"),
        ("unset_statement", "UnsetStatement"),
        ("print_intrinsic", "PrintExpression"),
        ("yield_expression", "YieldExpression"),
        ("shell_command_expression", "ShellCommandExpression"),
        ("execution_string_chars", "ExecutionStringChars"),
        ("execution_string_chars_after_variable", "ExecutionStringChars"),
        ("empty_statement", "EmptyStatement"),
        ("sentinel_error", "ErrorNode"),

        // Identifiers
        ("name", "Identifier"),
        ("literal", "Literal"),
        ("expression", "Expression"),
        ("statement", "Statement"),
        ("primary_expression", "PrimaryExpression"),

        // Comments
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// JSON-specific mappings (data format language)
static JSON_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add JSON-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),

        // Data structures
        ("object", "ObjectExpression"),
        ("array", "ArrayExpression"),
        ("pair", "KeyValuePair"),

        // Literals
        ("string", "StringLiteral"),
        ("string_content", "StringContent"),
        ("number", "NumberLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("null", "NullLiteral"),

        // Other
        ("escape_sequence", "EscapeSequence"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// TOML-specific mappings (configuration file format)
static TOML_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add TOML-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),

        // Tables (TOML sections)
        ("table", "ObjectExpression"),
        ("table_array_element", "ArrayExpression"),
        ("inline_table", "ObjectExpression"),

        // Key-value pairs
        ("pair", "KeyValuePair"),
        ("bare_key", "Identifier"),
        ("dotted_key", "QualifiedIdentifier"),
        ("quoted_key", "StringLiteral"),

        // Data structures
        ("array", "ArrayExpression"),

        // Literals
        ("string", "StringLiteral"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("boolean", "BooleanLiteral"),

        // Date/time literals (TOML-specific)
        ("offset_date_time", "DateTimeLiteral"),
        ("local_date_time", "DateTimeLiteral"),
        ("local_date", "DateLiteral"),
        ("local_time", "TimeLiteral"),

        // Other
        ("escape_sequence", "EscapeSequence"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// SQL-specific mappings
static SQL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add SQL-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),
        ("statement", "Statement"),

        // DML Statements
        ("select", "SelectStatement"),
        ("select_expression", "SelectExpression"),
        ("insert", "InsertStatement"),
        ("update", "UpdateStatement"),
        ("delete", "DeleteStatement"),

        // DDL Statements
        ("create_table", "CreateTableStatement"),
        ("create_view", "CreateViewStatement"),
        ("create_index", "CreateIndexStatement"),
        ("create_function", "CreateFunctionStatement"),
        ("create_database", "CreateDatabaseStatement"),
        ("create_schema", "CreateSchemaStatement"),
        ("create_trigger", "CreateTriggerStatement"),
        ("create_type", "CreateTypeStatement"),
        ("create_sequence", "CreateSequenceStatement"),
        ("create_role", "CreateRoleStatement"),
        ("create_extension", "CreateExtensionStatement"),
        ("create_materialized_view", "CreateMaterializedViewStatement"),

        // Alter statements
        ("alter_table", "AlterTableStatement"),
        ("alter_column", "AlterColumnStatement"),
        ("alter_database", "AlterDatabaseStatement"),
        ("alter_index", "AlterIndexStatement"),
        ("alter_role", "AlterRoleStatement"),
        ("alter_schema", "AlterSchemaStatement"),
        ("alter_sequence", "AlterSequenceStatement"),
        ("alter_type", "AlterTypeStatement"),
        ("alter_view", "AlterViewStatement"),

        // Drop statements
        ("drop_table", "DropTableStatement"),
        ("drop_function", "DropFunctionStatement"),

        // Query clauses
        ("from", "FromClause"),
        ("where", "WhereClause"),
        ("join", "JoinClause"),
        ("cross_join", "CrossJoinClause"),
        ("lateral_join", "LateralJoinClause"),
        ("group_by", "GroupByClause"),
        ("having", "HavingClause"),
        ("order_by", "OrderByClause"),
        ("limit", "LimitClause"),
        ("window_clause", "WindowClause"),
        ("window_specification", "WindowSpecification"),
        ("window_frame", "WindowFrame"),
        ("window_function", "WindowFunction"),

        // Expressions
        ("expression", "Expression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("between_expression", "BetweenExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("case", "CaseExpression"),
        ("cast", "CastExpression"),
        ("filter_expression", "FilterExpression"),

        // Functions and calls
        ("function", "CallExpression"),
        ("function_declaration", "FunctionDeclaration"),
        ("function_body", "FunctionBody"),
        ("function_argument", "Argument"),
        ("function_arguments", "ArgumentList"),

        // Data types and columns
        ("column", "ColumnReference"),
        ("column_definition", "ColumnDefinition"),
        ("column_definitions", "ColumnDefinitionList"),
        ("col_name", "ColumnName"),
        ("alias", "Alias"),
        ("array", "ArrayExpression"),

        // Constraints
        ("constraint", "Constraint"),
        ("constraints", "ConstraintList"),
        ("add_constraint", "AddConstraint"),

        // Identifiers and literals
        ("identifier", "Identifier"),
        ("string", "StringLiteral"),
        ("number", "NumberLiteral"),

        // Set operations
        ("union", "UnionExpression"),

        // Control flow
        ("while_statement", "WhileStatement"),

        // Assignments
        ("assignment", "AssignmentExpression"),
        ("assignment_list", "AssignmentList"),

        // Comments
        ("comment", "Comment"),
        ("comment_statement", "CommentStatement"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Perl-specific mappings
static PERL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Perl-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("source_file", "SourceFile"),
        ("package_statement", "PackageDeclaration"),

        // Function/subroutine declarations
        ("anonymous_subroutine_expression", "LambdaExpression"),
        ("anonymous_method_expression", "MethodDeclaration"),
        ("function_call_expression", "CallExpression"),
        ("method_call_expression", "MethodCallExpression"),
        ("ambiguous_function_call_expression", "CallExpression"),
        ("coderef_call_expression", "CallExpression"),
        ("method_declaration_statement", "MethodDeclaration"),

        // Control flow - conditionals
        ("conditional_statement", "IfStatement"),
        ("conditional_expression", "ConditionalExpression"),
        ("postfix_conditional_expression", "PostfixConditionalExpression"),

        // Control flow - loops
        ("for_statement", "ForStatement"),
        ("cstyle_for_statement", "ForStatement"),
        ("loop_statement", "LoopStatement"),
        ("foreach_statement", "ForEachStatement"),

        // Statements
        ("block_statement", "Block"),
        ("expression_statement", "ExpressionStatement"),
        ("class_statement", "TypeDeclaration"),
        ("class_phaser_statement", "ClassPhaser"),
        ("phaser_statement", "PhaserStatement"),
        ("defer_statement", "DeferStatement"),

        // Expressions
        ("binary_expression", "BinaryExpression"),
        ("equality_expression", "BinaryExpression"),
        ("assignment_expression", "AssignmentExpression"),
        ("lowprec_logical_expression", "LogicalExpression"),
        ("keyval_expression", "KeyValueExpression"),
        ("list_expression", "ArrayExpression"),
        ("do_expression", "DoExpression"),
        ("eval_expression", "EvalExpression"),
        ("goto_expression", "GotoExpression"),
        ("loopex_expression", "LoopControlExpression"),
        ("await_expression", "AwaitExpression"),
        ("localization_expression", "LocalizationExpression"),
        ("fileglob_expression", "GlobExpression"),
        ("map_grep_expression", "MapGrepExpression"),

        // Data structures
        ("anonymous_array_expression", "ArrayExpression"),
        ("anonymous_hash_expression", "HashExpression"),
        ("anonymous_slice_expression", "SliceExpression"),

        // Dereferencing
        ("array_deref_expression", "ArrayDerefExpression"),
        ("array_element_expression", "IndexExpression"),
        ("arraylen_deref_expression", "ArrayLengthExpression"),
        ("hash_deref_expression", "HashDerefExpression"),
        ("hash_element_expression", "HashAccessExpression"),
        ("amper_deref_expression", "SubroutineDerefExpression"),
        ("glob_deref_expression", "GlobDerefExpression"),
        ("glob_slot_expression", "GlobSlotExpression"),

        // Regex
        ("regex", "RegexLiteral"),
        ("substitution", "SubstitutionExpression"),
        ("transliteration", "TransliterationExpression"),

        // Other
        ("comment", "Comment"),
        ("pod", "DocComment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Groovy-specific mappings
static GROOVY_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Groovy-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("source_file", "SourceFile"),
        ("package", "PackageDeclaration"),

        // Type declarations
        ("class_definition", "TypeDeclaration"),
        ("class", "TypeDeclaration"),

        // Function/method declarations
        ("function_definition", "FunctionDeclaration"),
        ("function_declaration", "FunctionDeclaration"),
        ("closure", "LambdaExpression"),

        // Control flow
        ("if_statement", "IfStatement"),
        ("switch_statement", "SwitchStatement"),
        ("switch_block", "SwitchBlock"),
        ("try_statement", "TryStatement"),
        ("catch", "CatchClause"),
        ("catch_body", "Block"),
        ("finally", "FinallyClause"),
        ("finally_body", "Block"),

        // Loops
        ("for_loop", "ForStatement"),
        ("for_in_loop", "ForInStatement"),
        ("while_loop", "WhileStatement"),
        ("do_while_loop", "DoWhileStatement"),

        // Expressions
        ("assignment", "AssignmentExpression"),
        ("ternary_op", "ConditionalExpression"),
        ("binary_op", "BinaryExpression"),
        ("unary_op", "UnaryExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("juxt_function_call", "CallExpression"),

        // Data structures
        ("map", "MapExpression"),
        ("map_item", "KeyValuePair"),
        ("list", "ArrayExpression"),
        ("collection", "CollectionExpression"),

        // Literals
        ("string", "StringLiteral"),
        ("number_literal", "NumberLiteral"),
        ("boolean_literal", "BooleanLiteral"),
        ("null", "NullLiteral"),

        // Parameters and arguments
        ("parameter", "Parameter"),
        ("parameter_list", "ParameterList"),
        ("parameters", "ParameterList"),
        ("args", "ArgumentList"),
        ("argument_list", "ArgumentList"),

        // Types
        ("type", "TypeReference"),
        ("type_with_generics", "GenericType"),
        ("array_type", "ArrayType"),
        ("qualified_name", "QualifiedName"),
        ("dotted_identifier", "QualifiedIdentifier"),

        // Modifiers and annotations
        ("annotation", "Annotation"),
        ("access_modifier", "AccessModifier"),
        ("modifier", "Modifier"),

        // Imports
        ("import", "ImportDeclaration"),
        ("wildcard_import", "WildcardImport"),

        // Other
        ("comment", "Comment"),
        ("identifier", "Identifier"),
        ("variable", "Variable"),
        ("assert", "AssertStatement"),
        ("assertion", "AssertStatement"),
        ("return", "ReturnStatement"),
        ("break", "BreakStatement"),
        ("continue", "ContinueStatement"),
        ("this", "ThisExpression"),
        ("new", "NewExpression"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// R-specific mappings (statistical computing language)
static R_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add R-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),

        // Functions
        ("function_definition", "FunctionDeclaration"),
        ("function", "FunctionDeclaration"),
        ("call", "CallExpression"),

        // Control flow
        ("if_statement", "IfStatement"),
        ("if", "IfStatement"),
        ("else", "ElseClause"),

        // Loops
        ("for_statement", "ForStatement"),
        ("for", "ForStatement"),
        ("while_statement", "WhileStatement"),
        ("while", "WhileStatement"),
        ("repeat_statement", "RepeatStatement"),
        ("repeat", "RepeatStatement"),

        // Control keywords
        ("break", "BreakStatement"),
        ("next", "ContinueStatement"),
        ("return", "ReturnStatement"),

        // Expressions
        ("binary_operator", "BinaryExpression"),
        ("unary_operator", "UnaryExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("braced_expression", "Block"),
        ("extract_operator", "IndexExpression"),
        ("subset", "SubsetExpression"),
        ("namespace_operator", "NamespaceOperator"),

        // Assignment (R uses <- and = for assignment)
        ("assignment", "AssignmentExpression"),

        // Parameters and arguments
        ("parameter", "Parameter"),
        ("parameters", "ParameterList"),
        ("argument", "Argument"),
        ("arguments", "ArgumentList"),
        ("default", "DefaultValue"),

        // Literals
        ("string", "StringLiteral"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("complex", "ComplexLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("null", "NullLiteral"),
        ("na", "NALiteral"),
        ("nan", "NaNLiteral"),
        ("inf", "InfinityLiteral"),

        // Identifiers
        ("identifier", "Identifier"),
        ("variable", "Variable"),
        ("name", "Identifier"),
        ("dots", "DotsParameter"),
        ("dot_dot_i", "DotDotIParameter"),

        // Other
        ("comment", "Comment"),
        ("sequence", "SequenceExpression"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// YAML-specific mappings (data serialization format)
static YAML_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add YAML-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("stream", "SourceFile"),
        ("document", "Document"),
        ("block_node", "BlockNode"),
        ("flow_node", "FlowNode"),

        // Block-style collections (YAML's default indented style)
        ("block_mapping", "ObjectExpression"),
        ("block_mapping_pair", "KeyValuePair"),
        ("block_sequence", "ArrayExpression"),
        ("block_sequence_item", "ArrayItem"),

        // Flow-style collections (YAML's inline JSON-like style)
        ("flow_mapping", "ObjectExpression"),
        ("flow_sequence", "ArrayExpression"),
        ("flow_pair", "KeyValuePair"),

        // Scalar types
        ("plain_scalar", "StringLiteral"),
        ("double_quote_scalar", "StringLiteral"),
        ("single_quote_scalar", "StringLiteral"),
        ("string_scalar", "StringLiteral"),
        ("integer_scalar", "NumberLiteral"),
        ("float_scalar", "NumberLiteral"),
        ("boolean_scalar", "BooleanLiteral"),
        ("null_scalar", "NullLiteral"),

        // YAML-specific features
        ("tag", "TypeAnnotation"),
        ("anchor", "AnchorDefinition"),
        ("alias", "AnchorReference"),

        // Other
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// CMake-specific mappings (build system generator)
static CMAKE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add CMake-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("source_file", "SourceFile"),

        // Function/Macro definitions
        ("function_def", "FunctionDeclaration"),
        ("macro_def", "MacroDeclaration"),
        ("function_command", "FunctionKeyword"),
        ("endfunction_command", "EndFunctionKeyword"),
        ("macro_command", "MacroKeyword"),
        ("endmacro_command", "EndMacroKeyword"),

        // Block definitions
        ("block_def", "BlockDeclaration"),
        ("block_command", "BlockKeyword"),
        ("endblock_command", "EndBlockKeyword"),

        // Control flow - If
        ("if_condition", "IfStatement"),
        ("if_command", "IfKeyword"),
        ("elseif_command", "ElseIfKeyword"),
        ("else_command", "ElseKeyword"),
        ("endif_command", "EndIfKeyword"),

        // Control flow - Loops
        ("foreach_loop", "ForEachStatement"),
        ("foreach_command", "ForEachKeyword"),
        ("endforeach_command", "EndForEachKeyword"),
        ("while_command", "WhileKeyword"),
        ("endwhile_command", "EndWhileKeyword"),

        // Commands and arguments
        ("normal_command", "CallExpression"),
        ("argument_list", "ArgumentList"),
        ("argument", "Argument"),
        ("bracket_argument", "BracketArgument"),
        ("quoted_argument", "StringLiteral"),

        // Identifiers and variables
        ("identifier", "Identifier"),
        ("normal_var", "VariableReference"),
        ("env_var", "EnvironmentVariable"),
        ("cache_var", "CacheVariable"),

        // Comments
        ("line_comment", "LineComment"),
        ("bracket_comment", "BlockComment"),

        // Body/block content
        ("body", "Block"),
        ("block", "Block"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Make-specific mappings (build automation)
static MAKE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Make-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("makefile", "SourceFile"),

        // Rules (target: prerequisites; recipe)
        ("rule", "Rule"),
        ("targets", "Targets"),
        ("prerequisites", "Prerequisites"),
        ("paths", "Paths"),
        ("recipe", "Recipe"),
        ("recipe_line", "RecipeLine"),

        // Variables
        ("variable_assignment", "AssignmentExpression"),
        ("variable_reference", "VariableReference"),
        ("substitution_reference", "SubstitutionReference"),
        ("automatic_variable", "AutomaticVariable"),

        // Functions
        ("function_call", "CallExpression"),
        ("shell_function", "ShellFunction"),
        ("shell_assignment", "ShellAssignment"),
        ("shell_text_with_split", "ShellText"),

        // Conditionals
        ("conditional", "ConditionalDirective"),
        ("ifdef_directive", "IfdefDirective"),
        ("ifndef_directive", "IfndefDirective"),
        ("ifeq_directive", "IfeqDirective"),
        ("ifneq_directive", "IfneqDirective"),
        ("else_directive", "ElseDirective"),
        ("endif_directive", "EndifDirective"),

        // Directives
        ("include_directive", "IncludeDirective"),
        ("define_directive", "DefineDirective"),
        ("undefine_directive", "UndefineDirective"),
        ("export_directive", "ExportDirective"),
        ("unexport_directive", "UnexportDirective"),
        ("override_directive", "OverrideDirective"),
        ("private_directive", "PrivateDirective"),
        ("vpath_directive", "VpathDirective"),

        // Other
        ("arguments", "ArgumentList"),
        ("list", "List"),
        ("word", "Word"),
        ("text", "Text"),
        ("string", "StringLiteral"),
        ("concatenation", "Concatenation"),
        ("archive", "Archive"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Vue-specific mappings (Single File Components)
static VUE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Vue-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),

        // Vue SFC sections
        ("template_element", "TemplateElement"),
        ("template_start_tag", "TemplateStartTag"),
        ("script_element", "ScriptElement"),
        ("script_start_tag", "ScriptStartTag"),
        ("style_element", "StyleElement"),
        ("style_start_tag", "StyleStartTag"),

        // HTML elements
        ("element", "Element"),
        ("start_tag", "StartTag"),
        ("end_tag", "EndTag"),
        ("self_closing_tag", "SelfClosingTag"),
        ("erroneous_end_tag", "ErroneousEndTag"),
        ("erroneous_end_tag_name", "ErroneousEndTagName"),

        // Attributes (standard and Vue directives)
        ("attribute", "Attribute"),
        ("attribute_name", "AttributeName"),
        ("attribute_value", "AttributeValue"),
        ("quoted_attribute_value", "QuotedAttributeValue"),
        ("directive_attribute", "DirectiveAttribute"),
        ("directive_name", "DirectiveName"),
        ("directive_value", "DirectiveValue"),
        ("directive_modifiers", "DirectiveModifiers"),
        ("directive_modifier", "DirectiveModifier"),
        ("dynamic_directive_inner_value", "DynamicDirectiveValue"),
        ("dynamic_directive_value", "DynamicDirectiveValue"),

        // Interpolation ({{ }})
        ("interpolation", "Interpolation"),

        // Content
        ("text", "Text"),
        ("raw_text", "RawText"),
        ("entity", "Entity"),

        // Other
        ("doctype", "Doctype"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Angular-specific mappings (Angular templates with control flow)
static ANGULAR_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Angular-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),

        // Angular SFC sections (similar to Vue)
        ("template_element", "TemplateElement"),
        ("script_element", "ScriptElement"),
        ("script_start_tag", "ScriptStartTag"),
        ("style_element", "StyleElement"),
        ("style_start_tag", "StyleStartTag"),

        // HTML elements
        ("element", "Element"),
        ("start_tag", "StartTag"),
        ("end_tag", "EndTag"),
        ("self_closing_tag", "SelfClosingTag"),
        ("erroneous_end_tag", "ErroneousEndTag"),
        ("erroneous_end_tag_name", "ErroneousEndTagName"),

        // Attributes and bindings
        ("attribute", "Attribute"),
        ("attribute_name", "AttributeName"),
        ("attribute_value", "AttributeValue"),
        ("quoted_attribute_value", "QuotedAttributeValue"),
        ("property_binding", "PropertyBinding"),
        ("event_binding", "EventBinding"),
        ("two_way_binding", "TwoWayBinding"),
        ("animation_binding", "AnimationBinding"),
        ("binding_name", "BindingName"),

        // Angular control flow (new @-syntax)
        ("if_statement", "IfStatement"),
        ("if_condition", "IfCondition"),
        ("if_reference", "IfReference"),
        ("else_statement", "ElseClause"),
        ("else_if_statement", "ElseIfClause"),
        ("for_statement", "ForStatement"),
        ("for_declaration", "ForDeclaration"),
        ("for_reference", "ForReference"),
        ("switch_statement", "SwitchStatement"),
        ("switch_body", "SwitchBody"),
        ("case_statement", "CaseClause"),
        ("default_statement", "DefaultClause"),
        ("defer_statement", "DeferStatement"),
        ("defer_trigger", "DeferTrigger"),
        ("defer_trigger_condition", "DeferTriggerCondition"),
        ("loading_statement", "LoadingStatement"),
        ("loading_condition", "LoadingCondition"),
        ("placeholder_statement", "PlaceholderStatement"),
        ("placeholder_minimum", "PlaceholderMinimum"),
        ("error_statement", "ErrorStatement"),
        ("empty_statement", "EmptyStatement"),
        ("let_statement", "LetStatement"),

        // Structural directives (legacy *ngIf, *ngFor syntax)
        ("structural_directive", "StructuralDirective"),
        ("structural_declaration", "StructuralDeclaration"),
        ("structural_expression", "StructuralExpression"),
        ("structural_assignment", "StructuralAssignment"),

        // Expressions
        ("expression", "Expression"),
        ("call_expression", "CallExpression"),
        ("member_expression", "MemberExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("conditional_expression", "ConditionalExpression"),
        ("nullish_coalescing_expression", "NullishCoalescingExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("bracket_expression", "IndexExpression"),
        ("concatenation_expression", "ConcatenationExpression"),
        ("assignment_expression", "AssignmentExpression"),

        // Pipes
        ("pipe_sequence", "PipeSequence"),
        ("pipe_call", "PipeCall"),
        ("pipe_arguments", "PipeArguments"),

        // ICU expressions (i18n)
        ("icu_expression", "IcuExpression"),
        ("icu_clause", "IcuClause"),
        ("icu_case", "IcuCase"),
        ("icu_category", "IcuCategory"),

        // Literals and values
        ("identifier", "Identifier"),
        ("string", "StringLiteral"),
        ("template_string", "TemplateLiteral"),
        ("template_chars", "TemplateChars"),
        ("template_substitution", "TemplateSubstitution"),
        ("number", "NumberLiteral"),
        ("array", "ArrayExpression"),
        ("object", "ObjectExpression"),
        ("pair", "KeyValuePair"),
        ("spread", "SpreadElement"),
        ("group", "ParenthesizedExpression"),
        ("regular_expression", "RegexLiteral"),
        ("regular_expression_pattern", "RegexPattern"),
        ("regular_expression_flags", "RegexFlags"),
        ("statement_block", "Block"),

        // Time expressions
        ("timed_expression", "TimedExpression"),
        ("style_unit", "StyleUnit"),
        ("unit", "Unit"),

        // Interpolation
        ("interpolation", "Interpolation"),

        // Content
        ("text", "Text"),
        ("raw_text", "RawText"),
        ("entity", "Entity"),

        // Other
        ("doctype", "Doctype"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Haskell-specific mappings (functional language)
static HASKELL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Haskell-specific mappings
    pairs.extend_from_slice(&[
        // Module structure
        ("module", "ModuleDeclaration"),
        ("haskell", "SourceFile"),

        // Function declarations and definitions
        ("function", "FunctionDeclaration"),
        ("function_head", "FunctionHead"),
        ("function_body", "FunctionBody"),
        ("signature", "TypeSignature"),
        ("bind", "VariableDeclaration"),
        ("decl", "Declaration"),

        // Type declarations
        ("data", "TypeDeclaration"),
        ("data_type", "TypeDeclaration"),
        ("newtype", "TypeDeclaration"),
        ("type_alias", "TypeAlias"),
        ("type_family", "TypeFamilyDeclaration"),
        ("class", "ClassDeclaration"),
        ("instance", "InstanceDeclaration"),
        ("deriving", "DerivingClause"),

        // Pattern matching and control flow
        ("case", "MatchExpression"),
        ("match", "MatchExpression"),
        ("alternative", "MatchArm"),
        ("guard", "Guard"),
        ("guards", "GuardList"),
        ("guard_equation", "GuardEquation"),
        ("if", "IfExpression"),
        ("conditional", "ConditionalExpression"),

        // Expressions
        ("do", "DoExpression"),
        ("do_statement", "DoStatement"),
        ("lambda", "LambdaExpression"),
        ("exp_lambda", "LambdaExpression"),
        ("application", "CallExpression"),
        ("exp_apply", "CallExpression"),
        ("infix", "BinaryExpression"),
        ("exp_infix", "BinaryExpression"),
        ("exp_negation", "UnaryExpression"),
        ("let", "LetExpression"),
        ("let_in", "LetInExpression"),
        ("exp_let_in", "LetInExpression"),
        ("where", "WhereClause"),
        ("exp_parens", "ParenthesizedExpression"),
        ("exp_tuple", "TupleExpression"),
        ("exp_list", "ArrayExpression"),
        ("exp_record", "RecordExpression"),
        ("exp_section_left", "SectionExpression"),
        ("exp_section_right", "SectionExpression"),
        ("exp_arithmetic_sequence", "ArithmeticSequence"),
        ("list_comprehension", "ListComprehension"),

        // Literals
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("char", "CharacterLiteral"),
        ("string", "StringLiteral"),
        ("con_unit", "UnitLiteral"),

        // Identifiers and names
        ("variable", "Identifier"),
        ("constructor", "TypeIdentifier"),
        ("qualified_variable", "QualifiedIdentifier"),
        ("qualified_constructor", "QualifiedTypeIdentifier"),
        ("operator", "OperatorIdentifier"),
        ("constructor_operator", "OperatorIdentifier"),

        // Types
        ("type", "TypeReference"),
        ("type_apply", "TypeApplication"),
        ("type_infix", "TypeInfixExpression"),
        ("type_name", "TypeIdentifier"),
        ("type_variable", "TypeVariable"),
        ("type_tuple", "TupleType"),
        ("type_list", "ListType"),
        ("type_fun", "FunctionType"),
        ("forall", "ForallType"),
        ("context", "TypeContext"),
        ("constraint", "TypeConstraint"),

        // Imports and exports
        ("import", "ImportDeclaration"),
        ("export", "ExportDeclaration"),
        ("import_item", "ImportSpecifier"),
        ("export_item", "ExportSpecifier"),
        ("hiding", "HidingClause"),

        // Records
        ("field", "FieldDeclaration"),
        ("field_update", "FieldUpdate"),
        ("field_pattern", "FieldPattern"),

        // Patterns
        ("pat_name", "IdentifierPattern"),
        ("pat_wildcard", "WildcardPattern"),
        ("pat_as", "AsPattern"),
        ("pat_tuple", "TuplePattern"),
        ("pat_list", "ListPattern"),
        ("pat_parens", "ParenthesizedPattern"),
        ("pat_constructor", "ConstructorPattern"),
        ("pat_record", "RecordPattern"),
        ("pat_negation", "NegationPattern"),

        // Other
        ("comment", "Comment"),
        ("pragma", "Pragma"),
        ("fixity", "FixityDeclaration"),
        ("default", "DefaultDeclaration"),
        ("foreign_import", "ForeignImportDeclaration"),
        ("foreign_export", "ForeignExportDeclaration"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// OCaml-specific mappings (ML-family functional language)
static OCAML_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add OCaml-specific mappings
    pairs.extend_from_slice(&[
        // Module structure
        ("compilation_unit", "SourceFile"),
        ("module_definition", "ModuleDeclaration"),
        ("module_binding", "ModuleBinding"),
        ("module_type_definition", "ModuleTypeDeclaration"),
        ("module_path", "ModulePath"),
        ("functor", "FunctorDeclaration"),
        ("functor_parameter", "FunctorParameter"),

        // Value bindings and definitions
        ("value_definition", "VariableDeclaration"),
        ("let_binding", "VariableDeclaration"),
        ("let_expression", "LetExpression"),
        ("let_and_expression", "LetExpression"),
        ("value_name", "Identifier"),
        ("value_path", "QualifiedIdentifier"),

        // Function definitions
        ("function_definition", "FunctionDeclaration"),
        ("fun_expression", "LambdaExpression"),
        ("function_expression", "LambdaExpression"),
        ("parameter", "Parameter"),
        ("labeled_argument", "LabeledArgument"),
        ("optional_argument", "OptionalArgument"),

        // Type definitions
        ("type_definition", "TypeDeclaration"),
        ("type_binding", "TypeBinding"),
        ("type_constructor_definition", "TypeConstructorDeclaration"),
        ("variant_declaration", "VariantDeclaration"),
        ("record_declaration", "RecordDeclaration"),
        ("field_declaration", "FieldDeclaration"),
        ("type_variable", "TypeVariable"),
        ("type_constraint", "TypeConstraint"),

        // Expressions
        ("application_expression", "CallExpression"),
        ("infix_expression", "BinaryExpression"),
        ("prefix_expression", "UnaryExpression"),
        ("if_expression", "IfExpression"),
        ("match_expression", "MatchExpression"),
        ("match_case", "MatchArm"),
        ("try_expression", "TryExpression"),
        ("while_expression", "WhileExpression"),
        ("for_expression", "ForExpression"),
        ("sequence_expression", "SequenceExpression"),
        ("begin_expression", "BeginExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("typed_expression", "TypedExpression"),
        ("coercion_expression", "CoercionExpression"),
        ("assert_expression", "AssertExpression"),
        ("lazy_expression", "LazyExpression"),

        // Data structures
        ("tuple_expression", "TupleExpression"),
        ("list_expression", "ArrayExpression"),
        ("array_expression", "ArrayExpression"),
        ("record_expression", "RecordExpression"),
        ("field_expression", "FieldExpression"),
        ("object_expression", "ObjectExpression"),

        // Patterns
        ("value_pattern", "IdentifierPattern"),
        ("any_pattern", "WildcardPattern"),
        ("constructor_pattern", "ConstructorPattern"),
        ("tuple_pattern", "TuplePattern"),
        ("list_pattern", "ListPattern"),
        ("record_pattern", "RecordPattern"),
        ("type_pattern", "TypePattern"),
        ("or_pattern", "OrPattern"),
        ("alias_pattern", "AsPattern"),
        ("lazy_pattern", "LazyPattern"),
        ("range_pattern", "RangePattern"),

        // Literals
        ("number", "NumberLiteral"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("character", "CharacterLiteral"),
        ("string", "StringLiteral"),
        ("quoted_string", "StringLiteral"),
        ("unit", "UnitLiteral"),
        ("boolean", "BooleanLiteral"),

        // Types
        ("type_constructor_path", "TypeReference"),
        ("constructed_type", "ConstructedType"),
        ("tuple_type", "TupleType"),
        ("function_type", "FunctionType"),
        ("polymorphic_variant_type", "PolymorphicVariantType"),
        ("object_type", "ObjectType"),
        ("class_type", "ClassType"),
        ("package_type", "PackageType"),

        // Classes and objects
        ("class_definition", "ClassDeclaration"),
        ("class_binding", "ClassBinding"),
        ("class_type_definition", "ClassTypeDeclaration"),
        ("method_definition", "MethodDeclaration"),
        ("instance_variable_definition", "InstanceVariableDeclaration"),
        ("inherit_definition", "InheritDeclaration"),
        ("initializer_definition", "InitializerDeclaration"),

        // Module system
        ("open_statement", "ImportDeclaration"),
        ("include_statement", "IncludeDeclaration"),
        ("external_declaration", "ExternalDeclaration"),
        ("exception_definition", "ExceptionDeclaration"),

        // Comments and attributes
        ("comment", "Comment"),
        ("attribute", "Attribute"),
        ("item_attribute", "ItemAttribute"),
        ("floating_attribute", "FloatingAttribute"),
        ("extension", "Extension"),

        // Operators
        ("infix_operator", "InfixOperator"),
        ("prefix_operator", "PrefixOperator"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// F#-specific mappings (ML-family .NET language)
static FSHARP_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add F#-specific mappings
    pairs.extend_from_slice(&[
        // Module structure
        ("file", "SourceFile"),
        ("module_defn", "ModuleDeclaration"),
        ("module_abbrev", "ModuleAlias"),
        ("namespace_defn", "NamespaceDeclaration"),

        // Value bindings and definitions
        ("value_defn", "VariableDeclaration"),
        ("function_defn", "FunctionDeclaration"),
        ("let_binding", "VariableDeclaration"),
        ("let_expression", "LetExpression"),
        ("use_binding", "UseBinding"),
        ("do_expression", "DoExpression"),
        ("member_defn", "MemberDeclaration"),
        ("abstract_member", "AbstractMemberDeclaration"),

        // Type definitions
        ("type_defn", "TypeDeclaration"),
        ("type_definition", "TypeDeclaration"),
        ("type_abbrev", "TypeAlias"),
        ("record_type", "RecordDeclaration"),
        ("union_type", "UnionDeclaration"),
        ("enum_type", "EnumDeclaration"),
        ("class_type", "ClassDeclaration"),
        ("interface_type", "InterfaceDeclaration"),
        ("struct_type", "StructDeclaration"),
        ("exception_defn", "ExceptionDeclaration"),

        // Expressions
        ("application_expression", "CallExpression"),
        ("infix_expression", "BinaryExpression"),
        ("prefix_expression", "UnaryExpression"),
        ("if_expression", "IfExpression"),
        ("match_expression", "MatchExpression"),
        ("match_clause", "MatchArm"),
        ("function_expression", "LambdaExpression"),
        ("fun_expression", "LambdaExpression"),
        ("try_expression", "TryExpression"),
        ("while_expression", "WhileExpression"),
        ("for_expression", "ForExpression"),
        ("for_to_expression", "ForToExpression"),
        ("for_in_expression", "ForInExpression"),
        ("computation_expression", "ComputationExpression"),
        ("ce_expression", "ComputationExpression"),
        ("sequence_expression", "SequenceExpression"),
        ("yield_expression", "YieldExpression"),
        ("return_expression", "ReturnExpression"),
        ("lazy_expression", "LazyExpression"),
        ("assert_expression", "AssertExpression"),
        ("upcast_expression", "UpcastExpression"),
        ("downcast_expression", "DowncastExpression"),
        ("typed_expression", "TypedExpression"),
        ("paren_expression", "ParenthesizedExpression"),
        ("begin_end_expression", "BeginExpression"),
        ("new_expression", "NewExpression"),
        ("object_expression", "ObjectExpression"),

        // Data structures
        ("tuple_expression", "TupleExpression"),
        ("list_expression", "ArrayExpression"),
        ("array_expression", "ArrayExpression"),
        ("record_expression", "RecordExpression"),
        ("anon_record_expression", "AnonymousRecordExpression"),
        ("indexer_expression", "IndexExpression"),
        ("slice_expression", "SliceExpression"),
        ("range_expression", "RangeExpression"),

        // Patterns
        ("identifier_pattern", "IdentifierPattern"),
        ("wildcard_pattern", "WildcardPattern"),
        ("const_pattern", "ConstantPattern"),
        ("tuple_pattern", "TuplePattern"),
        ("list_pattern", "ListPattern"),
        ("array_pattern", "ArrayPattern"),
        ("record_pattern", "RecordPattern"),
        ("union_pattern", "UnionPattern"),
        ("as_pattern", "AsPattern"),
        ("or_pattern", "OrPattern"),
        ("and_pattern", "AndPattern"),
        ("cons_pattern", "ConsPattern"),
        ("typed_pattern", "TypedPattern"),
        ("attribute_pattern", "AttributePattern"),
        ("when_clause", "WhenClause"),

        // Literals
        ("int", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("decimal", "NumberLiteral"),
        ("char", "CharacterLiteral"),
        ("string", "StringLiteral"),
        ("verbatim_string", "VerbatimStringLiteral"),
        ("triple_quoted_string", "TripleQuotedStringLiteral"),
        ("interpolated_string", "TemplateLiteral"),
        ("bool", "BooleanLiteral"),
        ("unit", "UnitLiteral"),

        // Types
        ("type_name", "TypeReference"),
        ("type_argument", "TypeArgument"),
        ("tuple_type", "TupleType"),
        ("function_type", "FunctionType"),
        ("array_type", "ArrayType"),
        ("constraint", "TypeConstraint"),
        ("type_parameter", "TypeParameter"),
        ("generic_type", "GenericType"),
        ("anon_record_type", "AnonymousRecordType"),

        // Module system
        ("open_statement", "ImportDeclaration"),
        ("import_decl", "ImportDeclaration"),

        // Attributes and modifiers
        ("attribute_set", "AttributeList"),
        ("attribute", "Attribute"),
        ("access_modifier", "AccessModifier"),
        ("member_modifier", "MemberModifier"),

        // Comments
        ("line_comment", "Comment"),
        ("block_comment", "BlockComment"),
        ("xml_doc", "DocComment"),

        // Active patterns
        ("active_pattern_defn", "ActivePatternDeclaration"),
        ("active_pattern_case", "ActivePatternCase"),

        // Quotations
        ("typed_quotation", "TypedQuotation"),
        ("untyped_quotation", "UntypedQuotation"),

        // Other
        ("extern_binding", "ExternalDeclaration"),
        ("measure_type", "MeasureType"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Julia-specific mappings (scientific computing language)
static JULIA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Julia-specific mappings
    pairs.extend_from_slice(&[
        // Module structure
        ("source_file", "SourceFile"),
        ("module_definition", "ModuleDeclaration"),
        ("baremodule_definition", "BareModuleDeclaration"),

        // Function definitions
        ("function_definition", "FunctionDeclaration"),
        ("short_function_definition", "FunctionDeclaration"),
        ("macro_definition", "MacroDeclaration"),
        ("parameter_list", "ParameterList"),
        ("typed_parameter", "TypedParameter"),
        ("optional_parameter", "OptionalParameter"),
        ("splat_parameter", "RestParameter"),
        ("keyword_parameters", "KeywordParameterList"),

        // Type definitions
        ("struct_definition", "TypeDeclaration"),
        ("mutable_struct_definition", "TypeDeclaration"),
        ("abstract_definition", "AbstractTypeDeclaration"),
        ("primitive_definition", "PrimitiveTypeDeclaration"),
        ("type_alias", "TypeAlias"),

        // Control flow
        ("if_statement", "IfStatement"),
        ("elseif_clause", "ElseIfClause"),
        ("else_clause", "ElseClause"),
        ("for_statement", "ForStatement"),
        ("for_binding", "ForBinding"),
        ("while_statement", "WhileStatement"),
        ("try_statement", "TryStatement"),
        ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"),
        ("do_clause", "DoClause"),
        ("return_statement", "ReturnStatement"),
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),

        // Expressions
        ("call_expression", "CallExpression"),
        ("broadcast_call_expression", "BroadcastCallExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("let_statement", "LetStatement"),
        ("let_binding", "LetBinding"),
        ("assignment", "AssignmentExpression"),
        ("compound_assignment", "CompoundAssignment"),
        ("update_expression", "UpdateExpression"),
        ("field_expression", "MemberExpression"),
        ("index_expression", "IndexExpression"),
        ("range_expression", "RangeExpression"),
        ("comprehension_expression", "ArrayComprehension"),
        ("generator_expression", "GeneratorExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("interpolation_expression", "TemplateInterpolation"),
        ("macro_expression", "MacroExpression"),
        ("quote_expression", "QuoteExpression"),
        ("lambda_expression", "LambdaExpression"),
        ("arrow_function", "ArrowFunction"),
        ("do_block", "DoBlock"),
        ("begin_expression", "BeginExpression"),

        // Data structures
        ("tuple_expression", "TupleExpression"),
        ("named_tuple_expression", "NamedTupleExpression"),
        ("vector_expression", "ArrayExpression"),
        ("matrix_expression", "MatrixExpression"),
        ("typed_expression", "TypedExpression"),
        ("pair_expression", "PairExpression"),

        // Literals
        ("integer_literal", "NumberLiteral"),
        ("float_literal", "NumberLiteral"),
        ("character_literal", "CharacterLiteral"),
        ("string_literal", "StringLiteral"),
        ("prefixed_string_literal", "PrefixedStringLiteral"),
        ("command_literal", "CommandLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("nothing", "NullLiteral"),

        // Identifiers and symbols
        ("identifier", "Identifier"),
        ("symbol", "SymbolLiteral"),
        ("operator", "OperatorIdentifier"),
        ("macro_identifier", "MacroIdentifier"),

        // Types
        ("type_clause", "TypeAnnotation"),
        ("parametric_type", "ParametricType"),
        ("where_clause", "WhereClause"),
        ("type_parameter", "TypeParameter"),
        ("subtype_clause", "SubtypeClause"),

        // Module system
        ("import_statement", "ImportDeclaration"),
        ("using_statement", "UsingDeclaration"),
        ("export_statement", "ExportDeclaration"),
        ("import_path", "ImportPath"),
        ("import_alias", "ImportAlias"),
        ("selected_import", "SelectedImport"),

        // Const and global
        ("const_statement", "ConstDeclaration"),
        ("global_statement", "GlobalDeclaration"),
        ("local_statement", "LocalDeclaration"),

        // Comments and documentation
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
        ("block_comment", "BlockComment"),

        // Quotation and macros
        ("quote_statement", "QuoteStatement"),
        ("macro_argument_list", "MacroArgumentList"),
        ("interpolation", "Interpolation"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Kotlin-specific mappings (JVM/Android language with null safety)
static KOTLIN_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        ("source_file", "SourceFile"), ("package_header", "PackageDeclaration"),
        ("import_header", "ImportDeclaration"), ("import_alias", "ImportAlias"),
        ("class_declaration", "TypeDeclaration"), ("class_body", "ClassBody"),
        ("companion_object", "CompanionObject"), ("object_declaration", "ObjectDeclaration"),
        ("object_literal", "ObjectExpression"), ("enum_class_body", "EnumBody"),
        ("enum_entry", "EnumMember"), ("function_declaration", "FunctionDeclaration"),
        ("function_body", "FunctionBody"), ("anonymous_function", "LambdaExpression"),
        ("lambda_literal", "LambdaExpression"), ("lambda_parameters", "ParameterList"),
        ("function_value_parameters", "ParameterList"), ("parameter", "Parameter"),
        ("property_declaration", "PropertyDeclaration"), ("property_delegate", "PropertyDelegate"),
        ("variable_declaration", "VariableDeclaration"), ("getter", "GetterDeclaration"),
        ("setter", "SetterDeclaration"), ("type_alias", "TypeAliasDeclaration"),
        ("type_parameters", "TypeParameterList"), ("type_parameter", "TypeParameter"),
        ("type_arguments", "TypeArguments"), ("primary_constructor", "ConstructorDeclaration"),
        ("secondary_constructor", "ConstructorDeclaration"), ("constructor_invocation", "ConstructorInvocation"),
        ("if_expression", "IfExpression"), ("when_expression", "MatchExpression"),
        ("when_entry", "MatchArm"), ("for_statement", "ForStatement"),
        ("while_statement", "WhileStatement"), ("do_while_statement", "DoWhileStatement"),
        ("try_expression", "TryStatement"), ("catch_block", "CatchClause"),
        ("finally_block", "FinallyClause"), ("call_expression", "CallExpression"),
        ("value_arguments", "ArgumentList"), ("value_argument", "Argument"),
        ("navigation_expression", "MemberExpression"), ("indexing_expression", "IndexExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"), ("assignment", "AssignmentExpression"),
        ("as_expression", "AsExpression"), ("range_expression", "RangeExpression"),
        ("elvis_expression", "ElvisExpression"), ("prefix_expression", "UnaryExpression"),
        ("postfix_expression", "UpdateExpression"), ("string_literal", "StringLiteral"),
        ("boolean_literal", "BooleanLiteral"), ("integer_literal", "NumberLiteral"),
        ("real_literal", "NumberLiteral"), ("null_literal", "NullLiteral"),
        ("this_expression", "ThisExpression"), ("super_expression", "SuperExpression"),
        ("callable_reference", "MethodReference"), ("annotation", "Annotation"),
        ("modifiers", "Modifiers"), ("visibility_modifier", "VisibilityModifier"),
        ("simple_identifier", "Identifier"), ("identifier", "QualifiedIdentifier"),
        ("user_type", "TypeReference"), ("function_type", "FunctionType"),
        ("nullable_type", "NullableType"), ("comment", "Comment"),
        ("line_comment", "LineComment"), ("multiline_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Swift-specific mappings (Apple's language for iOS/macOS)
static SWIFT_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        ("source_file", "SourceFile"), ("import_declaration", "ImportDeclaration"),
        ("class_declaration", "TypeDeclaration"), ("class_body", "ClassBody"),
        ("protocol_declaration", "ProtocolDeclaration"), ("protocol_body", "ProtocolBody"),
        ("enum_class_body", "EnumBody"), ("enum_entry", "EnumMember"),
        ("function_declaration", "FunctionDeclaration"), ("function_body", "FunctionBody"),
        ("init_declaration", "ConstructorDeclaration"), ("deinit_declaration", "DestructorDeclaration"),
        ("subscript_declaration", "SubscriptDeclaration"), ("lambda_literal", "LambdaExpression"),
        ("parameter", "Parameter"), ("type_parameters", "TypeParameterList"),
        ("type_parameter", "TypeParameter"), ("type_arguments", "TypeArguments"),
        ("property_declaration", "PropertyDeclaration"), ("computed_property", "ComputedProperty"),
        ("computed_getter", "GetterDeclaration"), ("computed_setter", "SetterDeclaration"),
        ("willset_clause", "WillSetClause"), ("didset_clause", "DidSetClause"),
        ("typealias_declaration", "TypeAliasDeclaration"), ("associatedtype_declaration", "AssociatedTypeDeclaration"),
        ("operator_declaration", "OperatorDeclaration"), ("if_expression", "IfExpression"),
        ("for_statement", "ForStatement"), ("while_statement", "WhileStatement"),
        ("repeat_while_statement", "DoWhileStatement"), ("try_expression", "TryExpression"),
        ("await_expression", "AwaitExpression"), ("call_expression", "CallExpression"),
        ("value_arguments", "ArgumentList"), ("value_argument", "Argument"),
        ("navigation_expression", "MemberExpression"), ("constructor_expression", "NewExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"), ("tuple_expression", "TupleExpression"),
        ("array_literal", "ArrayExpression"), ("dictionary_literal", "DictionaryExpression"),
        ("assignment", "AssignmentExpression"), ("as_expression", "AsExpression"),
        ("ternary_expression", "ConditionalExpression"), ("prefix_expression", "UnaryExpression"),
        ("postfix_expression", "UpdateExpression"), ("range_expression", "RangeExpression"),
        ("nil_coalescing_expression", "NilCoalescingExpression"), ("wildcard_pattern", "WildcardPattern"),
        ("line_string_literal", "StringLiteral"), ("multi_line_string_literal", "StringLiteral"),
        ("regex_literal", "RegexLiteral"), ("boolean_literal", "BooleanLiteral"),
        ("integer_literal", "NumberLiteral"), ("real_literal", "NumberLiteral"),
        ("self_expression", "ThisExpression"), ("attribute", "Attribute"),
        ("modifiers", "Modifiers"), ("visibility_modifier", "VisibilityModifier"),
        ("user_type", "TypeReference"), ("function_type", "FunctionType"),
        ("optional_type", "OptionalType"), ("tuple_type", "TupleType"),
        ("array_type", "ArrayType"), ("dictionary_type", "DictionaryType"),
        ("simple_identifier", "Identifier"), ("identifier", "QualifiedIdentifier"),
        ("comment", "Comment"), ("multiline_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Scala-specific mappings (JVM functional/OOP language)
static SCALA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        ("compilation_unit", "SourceFile"), ("package_clause", "PackageDeclaration"),
        ("package_object", "PackageObject"), ("import_declaration", "ImportDeclaration"),
        ("export_declaration", "ExportDeclaration"), ("class_definition", "TypeDeclaration"),
        ("trait_definition", "TraitDeclaration"), ("object_definition", "ObjectDeclaration"),
        ("template_body", "ClassBody"), ("class_parameters", "ParameterList"),
        ("class_parameter", "Parameter"), ("extends_clause", "ExtendsClause"),
        ("enum_definition", "EnumDeclaration"), ("enum_body", "EnumBody"),
        ("simple_enum_case", "EnumMember"), ("full_enum_case", "EnumMember"),
        ("function_definition", "FunctionDeclaration"), ("function_declaration", "FunctionDeclaration"),
        ("parameters", "ParameterList"), ("parameter", "Parameter"),
        ("lambda_expression", "LambdaExpression"), ("type_parameters", "TypeParameterList"),
        ("val_definition", "ValDeclaration"), ("val_declaration", "ValDeclaration"),
        ("var_definition", "VarDeclaration"), ("var_declaration", "VarDeclaration"),
        ("type_definition", "TypeAliasDeclaration"), ("extension_definition", "ExtensionDeclaration"),
        ("given_definition", "GivenDeclaration"), ("if_expression", "IfExpression"),
        ("match_expression", "MatchExpression"), ("case_block", "CaseBlock"),
        ("case_clause", "CaseClause"), ("guard", "Guard"),
        ("for_expression", "ForExpression"), ("while_expression", "WhileExpression"),
        ("return_expression", "ReturnStatement"), ("throw_expression", "ThrowExpression"),
        ("try_expression", "TryStatement"), ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"), ("call_expression", "CallExpression"),
        ("arguments", "ArgumentList"), ("field_expression", "MemberExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"), ("tuple_expression", "TupleExpression"),
        ("assignment_expression", "AssignmentExpression"), ("type_arguments", "TypeArguments"),
        ("infix_expression", "InfixExpression"), ("prefix_expression", "UnaryExpression"),
        ("instance_expression", "NewExpression"), ("interpolated_string_expression", "TemplateLiteral"),
        ("interpolation", "TemplateInterpolation"), ("case_class_pattern", "CaseClassPattern"),
        ("tuple_pattern", "TuplePattern"), ("wildcard", "WildcardPattern"),
        ("string", "StringLiteral"), ("boolean_literal", "BooleanLiteral"),
        ("integer_literal", "NumberLiteral"), ("floating_point_literal", "NumberLiteral"),
        ("null_literal", "NullLiteral"), ("annotation", "Annotation"),
        ("modifiers", "Modifiers"), ("access_modifier", "AccessModifier"),
        ("generic_type", "GenericType"), ("function_type", "FunctionType"),
        ("tuple_type", "TupleType"), ("block", "Block"),
        ("identifier", "Identifier"), ("operator_identifier", "OperatorIdentifier"),
        ("comment", "Comment"), ("block_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Dart-specific mappings (Flutter/UI language)
static DART_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        ("program", "SourceFile"), ("library_name", "LibraryDeclaration"),
        ("library_import", "ImportDeclaration"), ("library_export", "ExportDeclaration"),
        ("part_directive", "PartDirective"), ("class_definition", "TypeDeclaration"),
        ("class_body", "ClassBody"), ("superclass", "ExtendsClause"),
        ("mixins", "MixinsClause"), ("interfaces", "ImplementsClause"),
        ("mixin_declaration", "MixinDeclaration"), ("extension_declaration", "ExtensionDeclaration"),
        ("extension_type_declaration", "ExtensionTypeDeclaration"), ("enum_declaration", "EnumDeclaration"),
        ("enum_body", "EnumBody"), ("enum_constant", "EnumMember"),
        ("type_alias", "TypeAliasDeclaration"), ("function_signature", "FunctionDeclaration"),
        ("function_body", "FunctionBody"), ("function_expression", "FunctionExpression"),
        ("lambda_expression", "LambdaExpression"), ("formal_parameter_list", "ParameterList"),
        ("formal_parameter", "Parameter"), ("type_parameters", "TypeParameterList"),
        ("type_parameter", "TypeParameter"), ("method_signature", "MethodDeclaration"),
        ("getter_signature", "GetterDeclaration"), ("setter_signature", "SetterDeclaration"),
        ("constructor_signature", "ConstructorDeclaration"), ("factory_constructor_signature", "FactoryConstructorDeclaration"),
        ("local_variable_declaration", "LocalVariableDeclaration"), ("initialized_variable_definition", "VariableDeclaration"),
        ("if_statement", "IfStatement"), ("switch_statement", "SwitchStatement"),
        ("switch_expression", "SwitchExpression"), ("for_statement", "ForStatement"),
        ("while_statement", "WhileStatement"), ("throw_expression", "ThrowExpression"),
        ("call_expression", "CallExpression"), ("arguments", "ArgumentList"),
        ("argument", "Argument"), ("named_argument", "NamedArgument"),
        ("selector", "MemberExpression"), ("index_selector", "IndexExpression"),
        ("cascade_section", "CascadeExpression"), ("parenthesized_expression", "ParenthesizedExpression"),
        ("assignment_expression", "AssignmentExpression"), ("conditional_expression", "ConditionalExpression"),
        ("type_arguments", "TypeArguments"), ("unary_expression", "UnaryExpression"),
        ("postfix_expression", "UpdateExpression"), ("type_cast_expression", "CastExpression"),
        ("type_test_expression", "TypeTestExpression"), ("new_expression", "NewExpression"),
        ("const_object_expression", "ConstObjectExpression"), ("string_literal", "StringLiteral"),
        ("template_substitution", "TemplateInterpolation"), ("list_literal", "ArrayExpression"),
        ("set_or_map_literal", "SetOrMapLiteral"), ("record_literal", "RecordLiteral"),
        ("true", "BooleanLiteral"), ("false", "BooleanLiteral"),
        ("null_literal", "NullLiteral"), ("decimal_integer_literal", "NumberLiteral"),
        ("this", "ThisExpression"), ("super", "SuperExpression"),
        ("await_expression", "AwaitExpression"), ("annotation", "Annotation"),
        ("function_type", "FunctionType"), ("nullable_type", "NullableType"),
        ("record_type", "RecordType"), ("void_type", "VoidType"),
        ("identifier", "Identifier"), ("qualified", "QualifiedIdentifier"),
        ("block", "Block"), ("comment", "Comment"),
        ("documentation_comment", "DocComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Arduino-specific mappings (C/C++ variant for embedded)
/// Arduino inherits from C_CPP_MAPPINGS with additional Arduino-specific constructs
static ARDUINO_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    // Inherit all C/C++ mappings first
    for (k, v) in &C_CPP_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Arduino-specific function types
        ("function_definition", "FunctionDeclaration"),
        // Note: setup() and loop() are just function_definition in tree-sitter
        // Rules can match them by name pattern like: pattern: "void setup()"

        // Preprocessor directives (same as C/C++)
        ("preproc_include", "IncludeDirective"),
        ("preproc_def", "DefineDirective"),
        ("preproc_ifdef", "IfdefDirective"),
        ("preproc_if", "IfDirective"),

        // Hardware-related (standard C constructs)
        ("call_expression", "CallExpression"),  // pinMode, digitalWrite, etc.
        ("binary_expression", "BinaryExpression"),
        ("number_literal", "NumberLiteral"),
        ("string_literal", "StringLiteral"),
        ("identifier", "Identifier"),
        ("comment", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Apex-specific mappings (Salesforce Java-like language)
static APEX_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Source and structure
        ("compilation_unit", "SourceFile"),

        // Type declarations
        ("class_declaration", "TypeDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("trigger_declaration", "TriggerDeclaration"),  // Apex-specific trigger

        // Method declarations
        ("method_declaration", "MethodDeclaration"),
        ("constructor_declaration", "ConstructorDeclaration"),

        // Variable/Field declarations
        ("field_declaration", "FieldDeclaration"),
        ("local_variable_declaration", "VariableDeclaration"),
        ("variable_declarator", "VariableDeclarator"),
        ("property_declaration", "PropertyDeclaration"),

        // Control flow
        ("if_statement", "IfStatement"),
        ("else_clause", "ElseClause"),
        ("for_statement", "ForStatement"),
        ("enhanced_for_statement", "ForEachStatement"),  // for (Type var : collection)
        ("while_statement", "WhileStatement"),
        ("do_statement", "DoWhileStatement"),
        ("switch_statement", "SwitchStatement"),
        ("when_clause", "CaseClause"),  // Apex uses 'when' instead of 'case'

        // Exception handling
        ("try_statement", "TryStatement"),
        ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"),
        ("throw_statement", "ThrowStatement"),

        // Expressions
        ("method_invocation", "CallExpression"),
        ("assignment_expression", "AssignmentExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("instanceof_expression", "InstanceofExpression"),
        ("cast_expression", "CastExpression"),
        ("new_expression", "NewExpression"),

        // SOQL/SOSL queries (Apex-specific)
        ("soql_query", "SoqlQuery"),
        ("soql_query_body", "SoqlQueryBody"),
        ("sosl_query", "SoslQuery"),
        ("sosl_query_body", "SoslQueryBody"),
        ("dml_statement", "DmlStatement"),  // insert, update, delete, upsert, merge

        // Access modifiers
        ("modifiers", "Modifiers"),
        ("annotation", "Annotation"),

        // Literals and identifiers
        ("identifier", "Identifier"),
        ("type_identifier", "TypeIdentifier"),
        ("string_literal", "StringLiteral"),
        ("integer_literal", "NumberLiteral"),
        ("decimal_literal", "NumberLiteral"),
        ("boolean", "BooleanLiteral"),
        ("null_literal", "NullLiteral"),

        // Other
        ("block", "Block"),
        ("return_statement", "ReturnStatement"),
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("expression_statement", "ExpressionStatement"),
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
        ("block_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Cairo-specific mappings (Rust-like smart contract language for StarkNet)
static CAIRO_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Source file
        ("source_file", "SourceFile"),

        // Function declarations
        ("function_item", "FunctionDeclaration"),
        ("function_definition", "FunctionDeclaration"),
        ("fn_declaration", "FunctionDeclaration"),

        // Struct and type declarations
        ("struct_definition", "TypeDeclaration"),
        ("struct_declaration", "StructDeclaration"),
        ("struct_item", "StructDeclaration"),
        ("enum_definition", "EnumDeclaration"),
        ("enum_item", "EnumDeclaration"),
        ("type_alias", "TypeAlias"),

        // Trait and impl (Rust-like)
        ("trait_definition", "TraitDeclaration"),
        ("trait_item", "TraitDeclaration"),
        ("impl_block", "ImplDeclaration"),
        ("impl_item", "ImplDeclaration"),

        // Module system
        ("mod_item", "ModuleDeclaration"),
        ("mod_declaration", "ModuleDeclaration"),
        ("use_declaration", "ImportDeclaration"),
        ("use_statement", "ImportDeclaration"),

        // Cairo-specific: Storage (smart contracts)
        ("storage_declaration", "StorageDeclaration"),  // #[storage]
        ("storage_var", "StorageVariable"),

        // Cairo-specific: Contract
        ("contract_declaration", "ContractDeclaration"),
        ("component_declaration", "ComponentDeclaration"),

        // Cairo-specific: Attributes/decorators
        ("attribute_item", "Attribute"),
        ("attribute", "Attribute"),
        ("inner_attribute", "InnerAttribute"),

        // Variables
        ("let_statement", "LetDeclaration"),
        ("let_declaration", "VariableDeclaration"),
        ("const_item", "ConstDeclaration"),

        // Control flow
        ("if_expression", "IfExpression"),
        ("if_statement", "IfStatement"),
        ("match_expression", "MatchExpression"),
        ("match_arm", "MatchArm"),
        ("loop_expression", "LoopExpression"),
        ("while_expression", "WhileExpression"),
        ("for_expression", "ForExpression"),

        // Expressions
        ("call_expression", "CallExpression"),
        ("method_call_expression", "CallExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("field_expression", "MemberExpression"),
        ("index_expression", "IndexExpression"),
        ("reference_expression", "ReferenceExpression"),
        ("tuple_expression", "TupleExpression"),
        ("array_expression", "ArrayExpression"),

        // Literals
        ("integer_literal", "NumberLiteral"),
        ("string_literal", "StringLiteral"),
        ("boolean_literal", "BooleanLiteral"),
        ("felt_literal", "NumberLiteral"),  // Cairo's felt252 type

        // Other
        ("identifier", "Identifier"),
        ("scoped_identifier", "ScopedIdentifier"),
        ("type_identifier", "TypeIdentifier"),
        ("generic_type", "GenericType"),
        ("block", "Block"),
        ("return_statement", "ReturnStatement"),
        ("return_expression", "ReturnExpression"),
        ("break_expression", "BreakStatement"),
        ("continue_expression", "ContinueStatement"),
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// CUE-specific mappings (Configuration language with constraints)
static CUE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Source file
        ("source_file", "SourceFile"),

        // Package
        ("package_clause", "PackageDeclaration"),
        ("package_identifier", "Identifier"),

        // Imports
        ("import_declaration", "ImportDeclaration"),
        ("import_spec", "ImportSpec"),
        ("import_path", "ImportPath"),

        // Definitions (CUE's main construct)
        ("definition", "Definition"),  // #name: value
        ("let_clause", "LetClause"),
        ("alias", "TypeAlias"),

        // Fields
        ("field", "Field"),
        ("field_name", "FieldName"),
        ("label", "Label"),
        ("optional", "OptionalField"),  // field?: value
        ("required", "RequiredField"),  // field!: value

        // Struct and list
        ("struct_lit", "ObjectExpression"),
        ("list_lit", "ArrayExpression"),
        ("ellipsis", "SpreadElement"),

        // Constraints (CUE-specific)
        ("constraint", "Constraint"),
        ("bound", "Bound"),  // <, >, <=, >=, !=
        ("unification", "Unification"),  // &
        ("disjunction", "Disjunction"),  // |

        // Expressions
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("selector_expression", "MemberExpression"),
        ("index_expression", "IndexExpression"),
        ("call_expression", "CallExpression"),
        ("interpolation", "TemplateInterpolation"),
        ("conditional", "ConditionalExpression"),  // if ... then ... else
        ("comprehension", "Comprehension"),

        // For comprehension
        ("for_clause", "ForClause"),
        ("if_clause", "IfClause"),

        // Literals
        ("identifier", "Identifier"),
        ("qualified_identifier", "QualifiedIdentifier"),
        ("string", "StringLiteral"),
        ("simple_string_lit", "StringLiteral"),
        ("multiline_string_lit", "StringLiteral"),
        ("bytes_lit", "BytesLiteral"),
        ("number", "NumberLiteral"),
        ("int_lit", "NumberLiteral"),
        ("float_lit", "NumberLiteral"),
        ("bool", "BooleanLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("null", "NullLiteral"),
        ("top", "TopLiteral"),  // _ (top/any)
        ("bottom", "BottomLiteral"),  // _|_ (bottom/error)

        // Types
        ("builtin_type", "TypeReference"),  // string, int, float, bool, bytes

        // Attributes
        ("attribute", "Attribute"),

        // Comments
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Dhall-specific mappings (Programmable configuration language)
static DHALL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Source file
        ("complete_expression", "SourceFile"),
        ("expression", "Expression"),

        // Let bindings
        ("let_binding", "VariableDeclaration"),
        ("let_expression", "LetExpression"),

        // Lambda (function)
        ("lambda_expression", "LambdaExpression"),
        ("lambda", "LambdaExpression"),
        ("forall_expression", "ForallType"),  // Type-level universal quantification
        ("forall", "ForallType"),

        // Conditionals
        ("if_then_else", "IfExpression"),
        ("if_expression", "IfExpression"),

        // Records (objects)
        ("record_literal", "ObjectExpression"),
        ("record_type_literal", "TypeDeclaration"),
        ("record_field", "Field"),
        ("record_field_type", "FieldDeclaration"),

        // Unions (sum types)
        ("union_literal", "UnionExpression"),
        ("union_type_literal", "UnionDeclaration"),
        ("union_field_type", "EnumMember"),

        // Lists
        ("list_literal", "ArrayExpression"),
        ("list_type", "ArrayType"),

        // Merge expression (Dhall-specific)
        ("merge_expression", "MergeExpression"),
        ("merge", "MergeExpression"),

        // Type operations
        ("assert_expression", "AssertExpression"),
        ("type_annotation", "TypeAnnotation"),
        ("with_expression", "WithExpression"),  // record update

        // Field access
        ("selector_expression", "MemberExpression"),
        ("selector", "MemberExpression"),
        ("projection", "Projection"),  // { field1, field2 }
        ("projection_by_type", "ProjectionByType"),  // { Type }

        // Function application
        ("application_expression", "CallExpression"),
        ("application", "CallExpression"),

        // Operators
        ("binary_expression", "BinaryExpression"),
        ("operator_expression", "BinaryExpression"),
        ("combine", "CombineOperator"),  // //\\
        ("combine_types", "CombineTypesOperator"),  // /\\
        ("prefer", "PreferOperator"),  // //
        ("list_append", "ListAppendOperator"),  // #

        // Imports (Dhall can import from URLs, files, env vars)
        ("import_expression", "ImportExpression"),
        ("http_import", "HttpImport"),
        ("env_import", "EnvImport"),
        ("local_import", "LocalImport"),
        ("import_hash", "ImportHash"),  // sha256 hash check
        ("missing", "MissingImport"),

        // Literals
        ("identifier", "Identifier"),
        ("builtin", "BuiltinIdentifier"),  // Natural, Integer, Text, etc.
        ("text_literal", "StringLiteral"),
        ("double_quote_literal", "StringLiteral"),
        ("single_quote_literal", "StringLiteral"),
        ("natural_literal", "NumberLiteral"),
        ("integer_literal", "NumberLiteral"),
        ("double_literal", "NumberLiteral"),
        ("bool_literal", "BooleanLiteral"),
        ("True", "BooleanLiteral"),
        ("False", "BooleanLiteral"),

        // Built-in types
        ("Type", "TypeKeyword"),
        ("Kind", "KindKeyword"),
        ("Sort", "SortKeyword"),

        // Tomap (Dhall-specific)
        ("tomap_expression", "ToMapExpression"),

        // Comments
        ("line_comment", "Comment"),
        ("block_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// BitBake-specific mappings (OpenEmbedded/Yocto build system)
static BITBAKE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Source file
        ("recipe", "SourceFile"),
        ("source_file", "SourceFile"),

        // Variable assignments (main BitBake construct)
        ("variable_assignment", "AssignmentExpression"),
        ("variable_expansion", "VariableExpansion"),
        ("variable_name", "Identifier"),
        ("identifier", "Identifier"),

        // Assignment operators (BitBake has many)
        ("assignment", "AssignmentExpression"),  // VAR = "value"
        ("default_assignment", "DefaultAssignment"),  // VAR ?= "value"
        ("weak_assignment", "WeakAssignment"),  // VAR ??= "value"
        ("immediate_assignment", "ImmediateAssignment"),  // VAR := "value"
        ("append", "AppendAssignment"),  // VAR += "value"
        ("prepend", "PrependAssignment"),  // VAR =+ "value"
        ("colon_append", "ColonAppend"),  // VAR:append = "value"
        ("colon_prepend", "ColonPrepend"),  // VAR:prepend = "value"
        ("colon_remove", "ColonRemove"),  // VAR:remove = "value"

        // Tasks (BitBake-specific)
        ("task", "TaskDeclaration"),
        ("task_definition", "TaskDeclaration"),
        ("python_task", "PythonTaskDeclaration"),
        ("shell_task", "ShellTaskDeclaration"),
        ("addtask", "AddTaskStatement"),  // addtask do_foo before do_bar
        ("deltask", "DeleteTaskStatement"),  // deltask do_foo

        // Inherit (BitBake class inheritance)
        ("inherit", "InheritStatement"),
        ("inherit_directive", "InheritStatement"),
        ("inherit_class", "InheritClass"),

        // Include/Require
        ("include", "IncludeStatement"),
        ("include_directive", "IncludeStatement"),
        ("require", "RequireStatement"),
        ("require_directive", "RequireStatement"),

        // Export
        ("export", "ExportStatement"),
        ("export_directive", "ExportStatement"),

        // Python/Shell embedded code
        ("python_block", "PythonBlock"),
        ("shell_block", "ShellBlock"),
        ("inline_python", "InlinePython"),  // ${@...}
        ("shell_function", "ShellFunction"),
        ("fakeroot", "FakerootModifier"),  // fakeroot python do_foo() {...}

        // Flags (variable flags)
        ("variable_flag", "VariableFlag"),
        ("flag_name", "FlagName"),

        // Overrides (conditional assignments)
        ("override", "Override"),
        ("override_style", "OverrideStyle"),  // VAR:class-target = "value"

        // Functions
        ("function_definition", "FunctionDeclaration"),
        ("anonymous_function", "AnonymousFunctionDeclaration"),

        // Literals
        ("string_literal", "StringLiteral"),
        ("quoted_string", "StringLiteral"),
        ("raw_string", "RawStringLiteral"),

        // Comments
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// CommonLisp-specific mappings (Lisp dialect with CLOS)
static COMMONLISP_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add CommonLisp-specific mappings
    pairs.extend_from_slice(&[
        // Function definitions
        ("defun", "FunctionDeclaration"),
        ("defmacro", "MacroDeclaration"),
        ("defgeneric", "GenericFunctionDeclaration"),
        ("defmethod", "MethodDeclaration"),
        ("lambda", "LambdaExpression"),
        ("function", "FunctionReference"),

        // Type and class definitions (CLOS)
        ("defclass", "TypeDeclaration"),
        ("defstruct", "StructDeclaration"),
        ("deftype", "TypeAliasDeclaration"),
        ("defconstant", "ConstDeclaration"),
        ("defvar", "VariableDeclaration"),
        ("defparameter", "ParameterDeclaration"),

        // Control flow
        ("if", "IfExpression"),
        ("cond", "CondExpression"),
        ("case", "CaseExpression"),
        ("when", "WhenExpression"),
        ("unless", "UnlessExpression"),
        ("progn", "Block"),
        ("block", "Block"),

        // Loops
        ("loop", "LoopExpression"),
        ("do", "DoExpression"),
        ("dolist", "DoListExpression"),
        ("dotimes", "DoTimesExpression"),

        // Let bindings
        ("let", "LetExpression"),
        ("let*", "LetStarExpression"),
        ("flet", "FletExpression"),
        ("labels", "LabelsExpression"),

        // S-expressions and lists
        ("list", "ListExpression"),
        ("cons", "ConsExpression"),
        ("car", "CarExpression"),
        ("cdr", "CdrExpression"),
        ("quote", "QuoteExpression"),
        ("quasiquote", "QuasiquoteExpression"),
        ("unquote", "UnquoteExpression"),
        ("backquote", "BackquoteExpression"),

        // Function calls
        ("funcall", "CallExpression"),
        ("apply", "ApplyExpression"),

        // Error handling
        ("handler-case", "TryExpression"),
        ("handler-bind", "HandlerBindExpression"),
        ("restart-case", "RestartCaseExpression"),
        ("unwind-protect", "UnwindProtectExpression"),

        // Multiple values
        ("values", "ValuesExpression"),
        ("multiple-value-bind", "MultipleValueBindExpression"),

        // Literals
        ("string", "StringLiteral"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("character", "CharacterLiteral"),
        ("symbol", "SymbolLiteral"),
        ("keyword", "KeywordLiteral"),
        ("nil", "NullLiteral"),
        ("t", "BooleanLiteral"),

        // Sequences
        ("vector", "ArrayExpression"),
        ("array", "ArrayExpression"),
        ("hash_table", "HashTableExpression"),

        // Package system
        ("defpackage", "PackageDeclaration"),
        ("in-package", "InPackageDeclaration"),
        ("use-package", "UsePackageDeclaration"),
        ("export", "ExportDeclaration"),
        ("import", "ImportDeclaration"),

        // Other
        ("comment", "Comment"),
        ("declare", "DeclareExpression"),
        ("the", "TypeAssertionExpression"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// AWK-specific mappings (text processing language)
static AWK_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add AWK-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),
        ("rule", "Rule"),
        ("pattern", "Pattern"),
        ("action", "Block"),

        // Function definitions
        ("function_definition", "FunctionDeclaration"),
        ("function_call", "CallExpression"),
        ("func_name", "Identifier"),
        ("param_list", "ParameterList"),

        // Control flow
        ("if_statement", "IfStatement"),
        ("else_clause", "ElseClause"),
        ("while_statement", "WhileStatement"),
        ("do_while_statement", "DoWhileStatement"),
        ("for_statement", "ForStatement"),
        ("for_in_statement", "ForInStatement"),

        // Control keywords
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("next_statement", "NextStatement"),
        ("nextfile_statement", "NextFileStatement"),
        ("exit_statement", "ExitStatement"),
        ("return_statement", "ReturnStatement"),

        // Expressions
        ("binary_exp", "BinaryExpression"),
        ("unary_exp", "UnaryExpression"),
        ("ternary_exp", "ConditionalExpression"),
        ("assignment_exp", "AssignmentExpression"),
        ("update_exp", "UpdateExpression"),
        ("concatenation", "ConcatenationExpression"),
        ("regex", "RegexLiteral"),
        ("regex_pattern", "RegexPattern"),
        ("regex_constant", "RegexLiteral"),
        ("field_ref", "FieldReference"),
        ("array_ref", "IndexExpression"),
        ("getline", "GetlineExpression"),
        ("pipe", "PipeExpression"),

        // Statements
        ("print_statement", "PrintStatement"),
        ("printf_statement", "PrintfStatement"),
        ("delete_statement", "DeleteStatement"),

        // Built-in patterns
        ("BEGIN", "BeginPattern"),
        ("END", "EndPattern"),
        ("BEGINFILE", "BeginFilePattern"),
        ("ENDFILE", "EndFilePattern"),

        // Literals
        ("string", "StringLiteral"),
        ("number", "NumberLiteral"),
        ("identifier", "Identifier"),

        // Other
        ("comment", "Comment"),
        ("block", "Block"),
        ("exp_list", "ExpressionList"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// CUDA-specific mappings (GPU programming, extends C/C++)
static CUDA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all C/C++ mappings first since CUDA extends C/C++
    for (k, v) in &C_CPP_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add CUDA-specific mappings
    pairs.extend_from_slice(&[
        // CUDA execution configuration
        ("kernel_call", "KernelCallExpression"),
        ("kernel_call_expression", "KernelCallExpression"),
        ("execution_configuration", "ExecutionConfiguration"),
        ("launch_bounds", "LaunchBoundsAttribute"),

        // CUDA function qualifiers
        ("__global__", "GlobalAttribute"),
        ("__device__", "DeviceAttribute"),
        ("__host__", "HostAttribute"),
        ("__shared__", "SharedAttribute"),
        ("__constant__", "ConstantAttribute"),
        ("__managed__", "ManagedAttribute"),
        ("__restrict__", "RestrictAttribute"),
        ("__noinline__", "NoinlineAttribute"),
        ("__forceinline__", "ForceinlineAttribute"),

        // CUDA function declarations (with qualifiers)
        ("kernel_declaration", "KernelDeclaration"),
        ("device_function_declaration", "DeviceFunctionDeclaration"),
        ("host_function_declaration", "HostFunctionDeclaration"),

        // Thread/block indexing
        ("blockIdx", "BlockIndex"),
        ("threadIdx", "ThreadIndex"),
        ("blockDim", "BlockDimension"),
        ("gridDim", "GridDimension"),
        ("warpSize", "WarpSize"),

        // Memory management
        ("cudaMalloc", "CudaMallocCall"),
        ("cudaFree", "CudaFreeCall"),
        ("cudaMemcpy", "CudaMemcpyCall"),
        ("cudaMemset", "CudaMemsetCall"),
        ("cudaMallocManaged", "CudaMallocManagedCall"),
        ("cudaMallocHost", "CudaMallocHostCall"),
        ("cudaFreeHost", "CudaFreeHostCall"),

        // Synchronization
        ("__syncthreads", "SyncThreadsCall"),
        ("__syncwarp", "SyncWarpCall"),
        ("__threadfence", "ThreadFenceCall"),
        ("__threadfence_block", "ThreadFenceBlockCall"),
        ("__threadfence_system", "ThreadFenceSystemCall"),
        ("cudaDeviceSynchronize", "DeviceSynchronizeCall"),

        // Atomic operations
        ("atomicAdd", "AtomicAddCall"),
        ("atomicSub", "AtomicSubCall"),
        ("atomicExch", "AtomicExchCall"),
        ("atomicMin", "AtomicMinCall"),
        ("atomicMax", "AtomicMaxCall"),
        ("atomicAnd", "AtomicAndCall"),
        ("atomicOr", "AtomicOrCall"),
        ("atomicXor", "AtomicXorCall"),
        ("atomicCAS", "AtomicCASCall"),

        // Texture and surface memory
        ("texture_reference", "TextureReference"),
        ("surface_reference", "SurfaceReference"),

        // CUDA vector types
        ("dim3", "Dim3Type"),
        ("float2", "Float2Type"),
        ("float3", "Float3Type"),
        ("float4", "Float4Type"),
        ("int2", "Int2Type"),
        ("int3", "Int3Type"),
        ("int4", "Int4Type"),
        ("uint2", "Uint2Type"),
        ("uint3", "Uint3Type"),
        ("uint4", "Uint4Type"),
        ("double2", "Double2Type"),
        ("double3", "Double3Type"),
        ("double4", "Double4Type"),

        // Warp-level primitives
        ("__shfl_sync", "ShflSyncCall"),
        ("__shfl_up_sync", "ShflUpSyncCall"),
        ("__shfl_down_sync", "ShflDownSyncCall"),
        ("__shfl_xor_sync", "ShflXorSyncCall"),
        ("__ballot_sync", "BallotSyncCall"),
        ("__all_sync", "AllSyncCall"),
        ("__any_sync", "AnySyncCall"),

        // Cooperative groups
        ("cooperative_groups", "CooperativeGroupsNamespace"),

        // Error handling
        ("cudaGetLastError", "GetLastErrorCall"),
        ("cudaPeekAtLastError", "PeekAtLastErrorCall"),
        ("cudaGetErrorString", "GetErrorStringCall"),

        // Stream operations
        ("cudaStreamCreate", "StreamCreateCall"),
        ("cudaStreamDestroy", "StreamDestroyCall"),
        ("cudaStreamSynchronize", "StreamSynchronizeCall"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Agda-specific mappings (dependently typed proof assistant/programming language)
static AGDA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Agda-specific mappings
    pairs.extend_from_slice(&[
        // Module system
        ("module_declaration", "ModuleDeclaration"),
        ("module_header", "ModuleHeader"),
        ("module_application", "ModuleApplication"),
        ("open_statement", "OpenStatement"),
        ("import_statement", "ImportDeclaration"),

        // Function definitions
        ("function_clause", "FunctionDeclaration"),
        ("function_signature", "TypeSignature"),
        ("function_name", "FunctionName"),
        ("clause", "FunctionClause"),

        // Type declarations
        ("data_declaration", "TypeDeclaration"),
        ("data_signature", "DataSignature"),
        ("record_declaration", "RecordDeclaration"),
        ("record_signature", "RecordSignature"),
        ("constructor", "ConstructorDeclaration"),
        ("field_declaration", "FieldDeclaration"),

        // Pattern matching
        ("with_abstraction", "WithAbstraction"),
        ("with_expression", "WithExpression"),
        ("rewrite_equation", "RewriteEquation"),
        ("pattern", "Pattern"),
        ("dot_pattern", "DotPattern"),
        ("absurd_pattern", "AbsurdPattern"),
        ("literal_pattern", "LiteralPattern"),
        ("constructor_pattern", "ConstructorPattern"),

        // Expressions
        ("application", "CallExpression"),
        ("lambda", "LambdaExpression"),
        ("let_expression", "LetExpression"),
        ("case_expression", "MatchExpression"),
        ("do_block", "DoExpression"),

        // Types and universes
        ("type_signature", "TypeSignature"),
        ("pi_type", "PiType"),
        ("function_type", "FunctionType"),
        ("forall_expression", "ForallExpression"),
        ("set", "SetType"),
        ("prop", "PropType"),

        // Literals
        ("integer_literal", "NumberLiteral"),
        ("float_literal", "NumberLiteral"),
        ("string_literal", "StringLiteral"),
        ("char_literal", "CharacterLiteral"),

        // Identifiers
        ("qualified_name", "QualifiedIdentifier"),
        ("name", "Identifier"),
        ("identifier", "Identifier"),

        // Implicit arguments
        ("implicit_argument", "ImplicitArgument"),
        ("instance_argument", "InstanceArgument"),

        // Pragmas and postulates
        ("pragma", "Pragma"),
        ("postulate", "PostulateDeclaration"),
        ("primitive", "PrimitiveDeclaration"),
        ("abstract_block", "AbstractBlock"),
        ("private_block", "PrivateBlock"),
        ("mutual_block", "MutualBlock"),

        // Instance declarations
        ("instance_declaration", "InstanceDeclaration"),

        // Macros
        ("macro_declaration", "MacroDeclaration"),
        ("syntax_declaration", "SyntaxDeclaration"),

        // Telescopes (parameter lists)
        ("telescope", "Telescope"),
        ("typed_binding", "TypedBinding"),
        ("untyped_binding", "UntypedBinding"),

        // Comments
        ("comment", "Comment"),
        ("block_comment", "BlockComment"),
        ("line_comment", "LineComment"),

        // Holes
        ("hole", "Hole"),
        ("goal", "Goal"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Objective-C specific mappings (Apple systems language)
static OBJC_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("translation_unit", "SourceFile"), ("preproc_include", "IncludeDirective"),
        ("preproc_import", "ImportDirective"), ("preproc_def", "DefineDirective"),
        ("preproc_ifdef", "IfdefDirective"), ("preproc_if", "IfDirective"),
        ("module_import", "ModuleImport"),

        // Class declarations
        ("class_interface", "InterfaceDeclaration"), ("class_implementation", "TypeDeclaration"),
        ("category_interface", "CategoryDeclaration"), ("category_implementation", "CategoryImplementation"),
        ("protocol_declaration", "ProtocolDeclaration"), ("protocol_forward_declaration", "ProtocolForwardDeclaration"),
        ("class_forward_declaration", "ClassForwardDeclaration"),

        // Properties and methods
        ("method_declaration", "MethodDeclaration"), ("method_definition", "MethodDeclaration"),
        ("class_method_declaration", "ClassMethodDeclaration"), ("instance_method_declaration", "InstanceMethodDeclaration"),
        ("property_declaration", "PropertyDeclaration"), ("synthesize_definition", "SynthesizeDeclaration"),
        ("dynamic_definition", "DynamicDeclaration"),

        // Method parts
        ("method_selector", "MethodSelector"), ("selector_expression", "SelectorExpression"),
        ("keyword_selector", "KeywordSelector"), ("keyword_declarator", "KeywordDeclarator"),
        ("keyword_argument", "KeywordArgument"), ("type_qualifier", "TypeQualifier"),

        // Expressions
        ("message_expression", "MessageExpression"), ("selector", "Selector"),
        ("message_selector", "MessageSelector"), ("keyword_argument_list", "KeywordArgumentList"),
        ("protocol_expression", "ProtocolExpression"), ("encode_expression", "EncodeExpression"),
        ("available_expression", "AvailableExpression"), ("boxed_expression", "BoxedExpression"),
        ("array_expression", "ArrayExpression"), ("dictionary_expression", "DictionaryExpression"),
        ("block_expression", "BlockExpression"),

        // Control flow (inherited from C)
        ("if_statement", "IfStatement"), ("switch_statement", "SwitchStatement"),
        ("for_statement", "ForStatement"), ("for_in_statement", "ForInStatement"),
        ("while_statement", "WhileStatement"), ("do_statement", "DoWhileStatement"),
        ("try_statement", "TryStatement"), ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"), ("throw_statement", "ThrowStatement"),
        ("autoreleasepool_statement", "AutoreleasePoolStatement"),
        ("synchronized_statement", "SynchronizedStatement"),

        // Types
        ("class_name", "TypeReference"), ("protocol_name", "ProtocolReference"),
        ("superclass_reference", "SuperclassReference"), ("type_identifier", "TypeIdentifier"),
        ("generic_type_references", "GenericTypeReferences"), ("protocol_qualifiers", "ProtocolQualifiers"),
        ("struct_specifier", "StructDeclaration"), ("enum_specifier", "EnumDeclaration"),
        ("union_specifier", "UnionDeclaration"), ("ns_enum_specifier", "NSEnumDeclaration"),

        // Literals and identifiers
        ("string_literal", "StringLiteral"), ("number_literal", "NumberLiteral"),
        ("char_literal", "CharacterLiteral"), ("identifier", "Identifier"),
        ("nil", "NilLiteral"), ("YES", "BooleanLiteral"), ("NO", "BooleanLiteral"),
        ("self", "SelfExpression"), ("super", "SuperExpression"),

        // Function/C constructs
        ("function_definition", "FunctionDeclaration"), ("function_declarator", "FunctionDeclarator"),
        ("declaration", "Declaration"), ("compound_statement", "Block"),
        ("expression_statement", "ExpressionStatement"), ("return_statement", "ReturnStatement"),
        ("call_expression", "CallExpression"), ("subscript_expression", "IndexExpression"),
        ("field_expression", "MemberExpression"), ("cast_expression", "CastExpression"),
        ("sizeof_expression", "SizeofExpression"), ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"), ("conditional_expression", "ConditionalExpression"),
        ("assignment_expression", "AssignmentExpression"), ("comma_expression", "SequenceExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("pointer_expression", "PointerExpression"),

        // Comments
        ("comment", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// D language specific mappings (systems programming language)
static D_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("module", "SourceFile"), ("module_declaration", "ModuleDeclaration"),
        ("import_declaration", "ImportDeclaration"), ("import_expression", "ImportExpression"),

        // Declarations
        ("function_declaration", "FunctionDeclaration"), ("function_definition", "FunctionDeclaration"),
        ("class_declaration", "TypeDeclaration"), ("struct_declaration", "StructDeclaration"),
        ("interface_declaration", "InterfaceDeclaration"), ("enum_declaration", "EnumDeclaration"),
        ("union_declaration", "UnionDeclaration"), ("alias_declaration", "TypeAlias"),
        ("template_declaration", "TemplateDeclaration"), ("mixin_declaration", "MixinDeclaration"),
        ("mixin_expression", "MixinExpression"), ("pragma_declaration", "PragmaDeclaration"),

        // Testing and contracts
        ("unittest", "TestDeclaration"), ("invariant", "InvariantDeclaration"),
        ("in_contract", "InContract"), ("out_contract", "OutContract"),
        ("contract", "Contract"), ("static_assert", "StaticAssert"),

        // Variable declarations
        ("variable_declaration", "VariableDeclaration"), ("auto_declaration", "AutoDeclaration"),
        ("enum_member", "EnumMember"), ("parameter", "Parameter"),

        // Control flow
        ("if_statement", "IfStatement"), ("static_if_declaration", "StaticIfDeclaration"),
        ("switch_statement", "SwitchStatement"), ("case_statement", "CaseClause"),
        ("default_statement", "DefaultClause"), ("final_switch_statement", "FinalSwitchStatement"),
        ("for_statement", "ForStatement"), ("foreach_statement", "ForEachStatement"),
        ("foreach_reverse_statement", "ForEachReverseStatement"),
        ("while_statement", "WhileStatement"), ("do_statement", "DoWhileStatement"),
        ("break_statement", "BreakStatement"), ("continue_statement", "ContinueStatement"),
        ("return_statement", "ReturnStatement"), ("goto_statement", "GotoStatement"),
        ("labeled_statement", "LabeledStatement"), ("throw_expression", "ThrowExpression"),
        ("try_statement", "TryStatement"), ("catch_clause", "CatchClause"),
        ("finally_clause", "FinallyClause"), ("scope_statement", "ScopeStatement"),
        ("synchronized_statement", "SynchronizedStatement"),
        ("with_statement", "WithStatement"), ("asm_statement", "AsmStatement"),

        // Expressions
        ("call_expression", "CallExpression"), ("template_instance", "TemplateInstance"),
        ("member_expression", "MemberExpression"), ("index_expression", "IndexExpression"),
        ("slice_expression", "SliceExpression"), ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"), ("cast_expression", "CastExpression"),
        ("new_expression", "NewExpression"), ("delete_expression", "DeleteExpression"),
        ("assert_expression", "AssertExpression"), ("typeof_expression", "TypeofExpression"),
        ("typeid_expression", "TypeidExpression"), ("traits_expression", "TraitsExpression"),
        ("is_expression", "IsExpression"), ("lambda_expression", "LambdaExpression"),
        ("function_literal", "FunctionLiteral"), ("conditional_expression", "ConditionalExpression"),
        ("assignment_expression", "AssignmentExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),

        // Literals
        ("string_literal", "StringLiteral"), ("char_literal", "CharacterLiteral"),
        ("integer_literal", "NumberLiteral"), ("float_literal", "NumberLiteral"),
        ("identifier", "Identifier"), ("true", "BooleanLiteral"), ("false", "BooleanLiteral"),
        ("null", "NullLiteral"), ("array_literal", "ArrayExpression"),
        ("assoc_array_literal", "DictionaryExpression"), ("special_keyword", "SpecialKeyword"),

        // Types
        ("type_identifier", "TypeIdentifier"), ("array_type", "ArrayType"),
        ("pointer_type", "PointerType"), ("slice_type", "SliceType"),
        ("function_type", "FunctionType"), ("delegate_type", "DelegateType"),
        ("void_type", "VoidType"), ("auto_type", "AutoType"),

        // Blocks
        ("block", "Block"), ("function_body", "FunctionBody"),

        // D-specific modifiers
        ("attribute", "Attribute"), ("storage_class", "StorageClass"),
        ("function_attribute", "FunctionAttribute"), ("linkage_attribute", "LinkageAttribute"),

        // Comments
        ("comment", "Comment"), ("nesting_block_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Crystal language specific mappings (Ruby-like compiled language)
static CRYSTAL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"), ("require", "RequireStatement"),
        ("require_expression", "RequireExpression"),

        // Declarations
        ("def", "FunctionDeclaration"), ("abstract_def", "AbstractMethodDeclaration"),
        ("class", "TypeDeclaration"), ("abstract_class", "AbstractClassDeclaration"),
        ("struct", "StructDeclaration"), ("module", "ModuleDeclaration"),
        ("lib", "LibDeclaration"), ("enum", "EnumDeclaration"),
        ("alias", "TypeAlias"), ("annotation", "AnnotationDeclaration"),

        // Macros
        ("macro", "MacroDeclaration"), ("macro_expression", "MacroExpression"),
        ("macro_for_loop", "MacroForLoop"), ("macro_if", "MacroIf"),
        ("macro_var", "MacroVar"), ("macro_literal", "MacroLiteral"),

        // Variables and parameters
        ("local_variable", "VariableDeclaration"), ("instance_variable", "InstanceVariable"),
        ("class_variable", "ClassVariable"), ("constant", "ConstDeclaration"),
        ("parameter", "Parameter"), ("block_parameter", "BlockParameter"),
        ("splat_parameter", "SplatParameter"), ("double_splat_parameter", "DoubleSplatParameter"),

        // Control flow
        ("if", "IfStatement"), ("unless", "UnlessStatement"),
        ("case", "CaseStatement"), ("when", "WhenClause"),
        ("select", "SelectStatement"), ("while", "WhileStatement"),
        ("until", "UntilStatement"), ("loop", "LoopStatement"),
        ("break", "BreakStatement"), ("next", "ContinueStatement"),
        ("return", "ReturnStatement"), ("yield", "YieldExpression"),

        // Exception handling
        ("exception_handler", "TryStatement"), ("rescue", "RescueClause"),
        ("ensure", "EnsureClause"), ("raise", "RaiseExpression"),
        ("begin", "BeginStatement"),

        // Expressions
        ("call", "CallExpression"), ("method_call", "MethodCall"),
        ("index_operator", "IndexExpression"), ("assign", "AssignmentExpression"),
        ("op_assign", "CompoundAssignment"), ("binary", "BinaryExpression"),
        ("unary", "UnaryExpression"), ("ternary", "ConditionalExpression"),
        ("not", "NotExpression"), ("and", "AndExpression"),
        ("or", "OrExpression"), ("range", "RangeExpression"),
        ("splat", "SplatExpression"), ("double_splat", "DoubleSplatExpression"),
        ("proc", "ProcExpression"), ("typeof", "TypeofExpression"),
        ("sizeof", "SizeofExpression"), ("instance_sizeof", "InstanceSizeofExpression"),
        ("pointerof", "PointerofExpression"), ("offsetof", "OffsetofExpression"),
        ("as", "AsExpression"), ("as?", "SafeAsExpression"),
        ("is_a", "IsAExpression"), ("responds_to", "RespondsToExpression"),
        ("nil?", "NilCheckExpression"),

        // Blocks and procs
        ("block", "Block"), ("do_end_block", "DoBlock"),
        ("brace_block", "BraceBlock"),

        // Literals
        ("string", "StringLiteral"), ("string_interpolation", "StringInterpolation"),
        ("symbol", "SymbolLiteral"), ("regex", "RegexLiteral"),
        ("integer", "NumberLiteral"), ("float", "NumberLiteral"),
        ("char", "CharacterLiteral"), ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"), ("nil", "NilLiteral"),
        ("array", "ArrayExpression"), ("array_literal", "ArrayLiteral"),
        ("hash", "DictionaryExpression"), ("hash_literal", "DictionaryLiteral"),
        ("tuple", "TupleExpression"), ("named_tuple", "NamedTupleExpression"),
        ("proc_literal", "ProcLiteral"), ("heredoc", "HeredocLiteral"),

        // Types
        ("type_declaration", "TypeAnnotation"), ("union_type", "UnionType"),
        ("generic_instance_type", "GenericType"), ("nilable_type", "NilableType"),
        ("pointer_type", "PointerType"), ("static_array_type", "StaticArrayType"),
        ("proc_type", "ProcType"), ("self_type", "SelfType"),
        ("typeof_type", "TypeofType"), ("path", "TypePath"),

        // Identifiers
        ("identifier", "Identifier"), ("constant", "Constant"),
        ("self", "SelfExpression"), ("type_id", "TypeIdentifier"),

        // Comments
        ("comment", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Zig language specific mappings (systems programming language)
static ZIG_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("root", "SourceFile"), ("container_doc_comment", "DocComment"),

        // Declarations
        ("fn_decl", "FunctionDeclaration"), ("FnProto", "FunctionPrototype"),
        ("container_decl", "TypeDeclaration"), ("container_decl_auto", "TypeDeclaration"),
        ("struct", "StructDeclaration"), ("enum", "EnumDeclaration"),
        ("union", "UnionDeclaration"), ("opaque", "OpaqueDeclaration"),
        ("error_set_decl", "ErrorSetDeclaration"),

        // Variable declarations
        ("var_decl", "VariableDeclaration"), ("global_var_decl", "GlobalVariableDeclaration"),
        ("local_var_decl", "LocalVariableDeclaration"),
        ("const", "ConstDeclaration"), ("var", "VarDeclaration"),
        ("ParamDecl", "ParameterDeclaration"), ("field_decl", "FieldDeclaration"),

        // Testing
        ("test_decl", "TestDeclaration"),

        // Comptime
        ("comptime_expr", "ComptimeExpression"), ("comptime", "ComptimeBlock"),

        // Control flow
        ("if_expr", "IfExpression"), ("if_stmt", "IfStatement"),
        ("else_expr", "ElseClause"), ("switch_expr", "SwitchExpression"),
        ("switch_prong", "SwitchCase"), ("switch_range", "SwitchRange"),
        ("for_expr", "ForExpression"), ("for_stmt", "ForStatement"),
        ("while_expr", "WhileExpression"), ("while_stmt", "WhileStatement"),
        ("while_cont", "WhileContinue"),
        ("break_expr", "BreakExpression"), ("continue_expr", "ContinueExpression"),
        ("return_expr", "ReturnStatement"), ("unreachable", "UnreachableExpression"),
        ("defer_expr", "DeferStatement"), ("errdefer_expr", "ErrDeferStatement"),

        // Error handling
        ("catch", "CatchExpression"), ("try", "TryExpression"),
        ("orelse", "OrelseExpression"), ("error_union", "ErrorUnion"),
        ("payload_expr", "PayloadExpression"),

        // Expressions
        ("call_expr", "CallExpression"), ("builtin_call_expr", "BuiltinCallExpression"),
        ("field_expr", "MemberExpression"), ("ptr_access_expr", "PointerAccessExpression"),
        ("slice_expr", "SliceExpression"), ("index_expr", "IndexExpression"),
        ("deref_expr", "DereferenceExpression"), ("address_of_expr", "AddressOfExpression"),
        ("binary_expr", "BinaryExpression"), ("unary_expr", "UnaryExpression"),
        ("grouped_expr", "ParenthesizedExpression"),
        ("asm_expr", "AsmExpression"), ("block_expr", "BlockExpression"),
        ("labeled_block", "LabeledBlock"), ("nosuspend_expr", "NosuspendExpression"),
        ("async_expr", "AsyncExpression"), ("await_expr", "AwaitExpression"),
        ("suspend_expr", "SuspendExpression"), ("resume_expr", "ResumeExpression"),

        // Literals
        ("string_literal", "StringLiteral"), ("multiline_string_literal", "MultilineStringLiteral"),
        ("char_literal", "CharacterLiteral"), ("integer_literal", "NumberLiteral"),
        ("float_literal", "NumberLiteral"), ("enum_literal", "EnumLiteral"),
        ("true", "BooleanLiteral"), ("false", "BooleanLiteral"),
        ("null_literal", "NullLiteral"), ("undefined_literal", "UndefinedLiteral"),
        ("array_init", "ArrayInitializer"), ("struct_init", "StructInitializer"),
        ("anonymous_struct_init", "AnonymousStructInit"),
        ("tuple_init", "TupleInitializer"),

        // Types
        ("type_expr", "TypeExpression"), ("ptr_type_expr", "PointerType"),
        ("array_type_expr", "ArrayType"), ("slice_type", "SliceType"),
        ("optional_type", "OptionalType"), ("error_union_type", "ErrorUnionType"),
        ("fn_type", "FunctionType"), ("anytype", "AnyType"),
        ("type", "TypeKeyword"), ("anyframe_type", "AnyframeType"),

        // Identifiers
        ("identifier", "Identifier"), ("field_access", "FieldAccess"),
        ("BUILTINIDENTIFIER", "BuiltinIdentifier"),

        // Blocks
        ("block", "Block"),

        // Comments
        ("line_comment", "LineComment"), ("doc_comment", "DocComment"),
        ("container_doc_comment", "ContainerDocComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Ada-specific mappings (WASM on-demand language)
/// Ada is a safety-critical systems programming language with strong typing.
static ADA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("compilation", "SourceFile"), ("compilation_unit", "CompilationUnit"),
        ("library_item", "LibraryItem"), ("subunit", "Subunit"),

        // Package declarations
        ("package_declaration", "PackageDeclaration"), ("package_specification", "PackageSpecification"),
        ("package_body", "PackageBody"), ("generic_package_declaration", "GenericPackageDeclaration"),
        ("package_instantiation", "PackageInstantiation"),

        // Subprogram declarations (functions and procedures)
        ("procedure_declaration", "FunctionDeclaration"), ("procedure_specification", "FunctionDeclaration"),
        ("procedure_body", "FunctionBody"), ("function_declaration", "FunctionDeclaration"),
        ("function_specification", "FunctionDeclaration"), ("function_body", "FunctionBody"),
        ("subprogram_body", "FunctionBody"), ("subprogram_declaration", "FunctionDeclaration"),
        ("generic_subprogram_declaration", "GenericFunctionDeclaration"),
        ("subprogram_instantiation", "FunctionInstantiation"),

        // Parameters
        ("parameter_specification", "Parameter"), ("formal_part", "ParameterList"),
        ("parameter_association", "Argument"), ("actual_parameter_part", "ArgumentList"),

        // Type declarations
        ("type_declaration", "TypeDeclaration"), ("full_type_declaration", "TypeDeclaration"),
        ("private_type_declaration", "PrivateTypeDeclaration"), ("subtype_declaration", "SubtypeDeclaration"),
        ("incomplete_type_declaration", "ForwardTypeDeclaration"),
        ("derived_type_definition", "DerivedTypeDeclaration"),
        ("record_type_definition", "RecordDeclaration"), ("record_definition", "RecordBody"),
        ("component_declaration", "FieldDeclaration"), ("component_list", "FieldList"),
        ("discriminant_part", "DiscriminantList"), ("discriminant_specification", "Discriminant"),
        ("variant_part", "VariantPart"), ("variant", "Variant"),
        ("enumeration_type_definition", "EnumDeclaration"), ("enumeration_literal", "EnumMember"),
        ("array_type_definition", "ArrayTypeDeclaration"), ("access_type_definition", "PointerTypeDeclaration"),
        ("interface_type_definition", "InterfaceDeclaration"),

        // Object declarations (variables/constants)
        ("object_declaration", "VariableDeclaration"), ("number_declaration", "ConstDeclaration"),
        ("exception_declaration", "ExceptionDeclaration"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"), ("if_expression", "IfExpression"),
        ("elsif_statement_item", "ElseIfClause"), ("else_statement", "ElseClause"),
        ("case_statement", "SwitchStatement"), ("case_expression", "SwitchExpression"),
        ("case_statement_alternative", "CaseClause"), ("discrete_choice", "CaseLabel"),
        ("case_expression_alternative", "CaseExpressionArm"),

        // Control flow - loops
        ("loop_statement", "LoopStatement"), ("for_loop_statement", "ForStatement"),
        ("while_loop_statement", "WhileStatement"), ("iteration_scheme", "IterationScheme"),
        ("loop_parameter_specification", "ForIterator"),
        ("exit_statement", "BreakStatement"), ("return_statement", "ReturnStatement"),

        // Block and compound statements
        ("block_statement", "Block"), ("handled_sequence_of_statements", "Block"),
        ("sequence_of_statements", "StatementList"), ("null_statement", "EmptyStatement"),

        // Exception handling
        ("exception_handler", "CatchClause"), ("raise_statement", "ThrowStatement"),
        ("exception_choice", "CatchType"),

        // Expressions
        ("name", "Identifier"), ("identifier", "Identifier"),
        ("selected_component", "MemberExpression"), ("indexed_component", "IndexExpression"),
        ("slice", "SliceExpression"), ("attribute_reference", "AttributeReference"),
        ("function_call", "CallExpression"), ("procedure_call_statement", "CallExpression"),
        ("qualified_expression", "QualifiedExpression"), ("type_conversion", "CastExpression"),
        ("allocator", "NewExpression"), ("aggregate", "AggregateExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("conditional_expression", "ConditionalExpression"),
        ("quantified_expression", "QuantifiedExpression"),
        ("raise_expression", "ThrowExpression"),

        // Operators
        ("binary_adding_operator", "BinaryExpression"), ("unary_adding_operator", "UnaryExpression"),
        ("multiplying_operator", "BinaryExpression"), ("relational_operator", "BinaryExpression"),
        ("logical_operator", "BinaryExpression"), ("highest_precedence_operator", "BinaryExpression"),
        ("membership_test", "InExpression"),

        // Literals
        ("numeric_literal", "NumberLiteral"), ("integer_literal", "NumberLiteral"),
        ("real_literal", "NumberLiteral"), ("string_literal", "StringLiteral"),
        ("character_literal", "CharacterLiteral"), ("null", "NullLiteral"),

        // Task and protected types (Ada concurrency)
        ("task_declaration", "TaskDeclaration"), ("task_type_declaration", "TaskTypeDeclaration"),
        ("task_body", "TaskBody"), ("task_definition", "TaskDefinition"),
        ("protected_type_declaration", "ProtectedTypeDeclaration"),
        ("protected_body", "ProtectedBody"), ("protected_definition", "ProtectedDefinition"),
        ("entry_declaration", "EntryDeclaration"), ("entry_body", "EntryBody"),
        ("accept_statement", "AcceptStatement"), ("select_statement", "SelectStatement"),
        ("entry_call_statement", "EntryCallStatement"),
        ("delay_statement", "DelayStatement"), ("abort_statement", "AbortStatement"),
        ("requeue_statement", "RequeueStatement"),

        // Use and with clauses (imports)
        ("with_clause", "ImportDeclaration"), ("use_clause", "UseDeclaration"),
        ("use_type_clause", "UseTypeDeclaration"), ("use_package_clause", "UsePackageDeclaration"),

        // Generic units
        ("generic_declaration", "GenericDeclaration"), ("generic_formal_part", "GenericFormalPart"),
        ("generic_formal_parameter", "GenericParameter"),

        // Representation clauses
        ("representation_clause", "RepresentationClause"),
        ("attribute_definition_clause", "AttributeDefinitionClause"),
        ("record_representation_clause", "RecordRepresentationClause"),
        ("enumeration_representation_clause", "EnumerationRepresentationClause"),

        // Pragmas and aspects
        ("pragma", "Pragma"), ("pragma_argument_association", "PragmaArgument"),
        ("aspect_specification", "AspectSpecification"), ("aspect_association", "AspectAssociation"),

        // Comments
        ("comment", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Fortran-specific mappings (WASM on-demand language)
/// Fortran is a scientific/numerical computing language.
static FORTRAN_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("translation_unit", "SourceFile"), ("program", "ProgramDeclaration"),
        ("program_statement", "ProgramDeclaration"), ("end_program_statement", "EndProgram"),

        // Module declarations
        ("module", "ModuleDeclaration"), ("module_statement", "ModuleDeclaration"),
        ("end_module_statement", "EndModule"), ("submodule", "SubmoduleDeclaration"),
        ("use_statement", "ImportDeclaration"),

        // Subroutines and functions
        ("subroutine", "SubroutineDeclaration"), ("subroutine_statement", "SubroutineDeclaration"),
        ("end_subroutine_statement", "EndSubroutine"),
        ("function", "FunctionDeclaration"), ("function_statement", "FunctionDeclaration"),
        ("end_function_statement", "EndFunction"),
        ("entry_statement", "EntryStatement"),
        ("return_statement", "ReturnStatement"), ("stop_statement", "StopStatement"),
        ("contains_statement", "ContainsStatement"),

        // Interface and abstract
        ("interface", "InterfaceDeclaration"), ("interface_statement", "InterfaceDeclaration"),
        ("end_interface_statement", "EndInterface"),
        ("abstract_interface", "AbstractInterfaceDeclaration"),
        ("generic_statement", "GenericDeclaration"), ("procedure_statement", "ProcedureStatement"),

        // Parameters
        ("dummy_argument_list", "ParameterList"), ("dummy_argument", "Parameter"),
        ("actual_argument_list", "ArgumentList"), ("actual_argument", "Argument"),
        ("argument_list", "ArgumentList"), ("keyword_argument", "NamedArgument"),

        // Type declarations
        ("type_declaration_statement", "TypeDeclaration"),
        ("derived_type_definition", "TypeDeclaration"), ("derived_type_statement", "TypeDeclaration"),
        ("end_type_statement", "EndType"),
        ("type_bound_procedure_part", "TypeBoundProcedures"),
        ("type_bound_procedure_statement", "TypeBoundProcedure"),
        ("private_components_statement", "PrivateClause"), ("sequence_statement", "SequenceClause"),

        // Variable declarations
        ("variable_declaration", "VariableDeclaration"),
        ("declaration_statement", "VariableDeclaration"),
        ("parameter_statement", "ConstDeclaration"), ("data_statement", "DataStatement"),
        ("common_statement", "CommonStatement"), ("equivalence_statement", "EquivalenceStatement"),
        ("save_statement", "SaveStatement"), ("dimension_statement", "DimensionStatement"),
        ("implicit_statement", "ImplicitStatement"), ("implicit_none_statement", "ImplicitNone"),
        ("external_statement", "ExternalStatement"), ("intrinsic_statement", "IntrinsicStatement"),
        ("allocatable_statement", "AllocatableStatement"), ("pointer_statement", "PointerStatement"),
        ("target_statement", "TargetStatement"), ("intent_statement", "IntentStatement"),
        ("optional_statement", "OptionalStatement"), ("public_statement", "PublicStatement"),
        ("private_statement", "PrivateStatement"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"), ("if_then_statement", "IfStatement"),
        ("elseif_statement", "ElseIfClause"), ("else_statement", "ElseClause"),
        ("end_if_statement", "EndIf"),
        ("select_case_statement", "SwitchStatement"), ("case_statement", "CaseClause"),
        ("end_select_statement", "EndSelect"),
        ("select_type_statement", "TypeSwitchStatement"), ("type_guard_statement", "TypeGuard"),
        ("class_is_statement", "ClassIsClause"), ("type_is_statement", "TypeIsClause"),
        ("select_rank_statement", "RankSwitchStatement"), ("rank_statement", "RankClause"),

        // Control flow - loops
        ("do_loop_statement", "DoStatement"), ("end_do_statement", "EndDo"),
        ("do_concurrent_statement", "DoConcurrentStatement"),
        ("while_statement", "WhileStatement"), ("forall_statement", "ForAllStatement"),
        ("forall_construct_statement", "ForAllConstruct"),
        ("exit_statement", "BreakStatement"), ("cycle_statement", "ContinueStatement"),

        // Block and compound statements
        ("block_construct", "Block"), ("block_statement", "Block"),
        ("end_block_statement", "EndBlock"),
        ("associate_statement", "AssociateStatement"), ("end_associate_statement", "EndAssociate"),

        // I/O statements
        ("write_statement", "WriteStatement"), ("read_statement", "ReadStatement"),
        ("print_statement", "PrintStatement"), ("open_statement", "OpenStatement"),
        ("close_statement", "CloseStatement"), ("inquire_statement", "InquireStatement"),
        ("rewind_statement", "RewindStatement"), ("backspace_statement", "BackspaceStatement"),
        ("endfile_statement", "EndfileStatement"), ("format_statement", "FormatStatement"),

        // Memory management
        ("allocate_statement", "AllocateStatement"), ("deallocate_statement", "DeallocateStatement"),
        ("nullify_statement", "NullifyStatement"),

        // Error handling
        ("error_stop_statement", "ErrorStopStatement"),

        // Expressions
        ("identifier", "Identifier"), ("name", "Identifier"),
        ("call_expression", "CallExpression"), ("function_reference", "CallExpression"),
        ("subroutine_call", "CallExpression"), ("array_element", "IndexExpression"),
        ("array_section", "SliceExpression"), ("derived_type_member_expression", "MemberExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("math_expression", "BinaryExpression"), ("relational_expression", "BinaryExpression"),
        ("logical_expression", "BinaryExpression"), ("concatenation_expression", "ConcatExpression"),
        ("unary_expression", "UnaryExpression"), ("complex_literal", "ComplexLiteral"),

        // Assignment
        ("assignment_statement", "AssignmentStatement"), ("pointer_assignment_statement", "PointerAssignment"),
        ("where_statement", "WhereStatement"), ("forall_assignment_statement", "ForAllAssignment"),

        // Array constructors
        ("array_constructor", "ArrayExpression"), ("implied_do_loop", "ImpliedDoLoop"),

        // Literals
        ("number_literal", "NumberLiteral"), ("integer_literal", "NumberLiteral"),
        ("real_literal", "NumberLiteral"), ("string_literal", "StringLiteral"),
        ("boolean_literal", "BooleanLiteral"), ("boz_literal", "BinaryLiteral"),

        // Coarrays (Fortran parallel features)
        ("codimension_statement", "CodimensionStatement"),
        ("sync_all_statement", "SyncAllStatement"), ("sync_images_statement", "SyncImagesStatement"),
        ("critical_statement", "CriticalStatement"), ("end_critical_statement", "EndCritical"),

        // Include and preprocessor
        ("include_statement", "IncludeDirective"), ("preproc_include", "IncludeDirective"),
        ("preproc_def", "DefineDirective"), ("preproc_if", "IfDirective"),

        // Comments
        ("comment", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// COBOL-specific mappings (Native DLL language)
/// COBOL is a legacy enterprise/mainframe language.
static COBOL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure - divisions
        ("program", "SourceFile"), ("source_unit", "SourceFile"),
        ("identification_division", "IdentificationDivision"),
        ("environment_division", "EnvironmentDivision"),
        ("data_division", "DataDivision"),
        ("procedure_division", "ProcedureDeclaration"),

        // Identification division
        ("program_id_paragraph", "ProgramIdParagraph"),
        ("author_paragraph", "AuthorParagraph"),
        ("installation_paragraph", "InstallationParagraph"),
        ("date_written_paragraph", "DateWrittenParagraph"),
        ("date_compiled_paragraph", "DateCompiledParagraph"),
        ("security_paragraph", "SecurityParagraph"),

        // Environment division sections
        ("configuration_section", "ConfigurationSection"),
        ("input_output_section", "InputOutputSection"),
        ("file_control_paragraph", "FileControlParagraph"),
        ("select_clause", "SelectClause"),

        // Data division sections
        ("file_section", "FileSection"), ("working_storage_section", "WorkingStorageSection"),
        ("local_storage_section", "LocalStorageSection"), ("linkage_section", "LinkageSection"),
        ("screen_section", "ScreenSection"), ("report_section", "ReportSection"),
        ("communication_section", "CommunicationSection"),

        // Data descriptions
        ("data_description_entry", "DataDescriptionEntry"),
        ("level_number", "LevelNumber"), ("data_name", "DataName"),
        ("picture_clause", "PictureClause"), ("value_clause", "ValueClause"),
        ("occurs_clause", "OccursClause"), ("redefines_clause", "RedefinesClause"),
        ("usage_clause", "UsageClause"), ("sign_clause", "SignClause"),
        ("justified_clause", "JustifiedClause"), ("blank_when_zero_clause", "BlankWhenZeroClause"),
        ("synchronized_clause", "SynchronizedClause"),
        ("file_description_entry", "FileDescriptionEntry"),
        ("record_description_entry", "RecordDescriptionEntry"),
        ("copy_statement", "CopyStatement"), ("replace_statement", "ReplaceStatement"),

        // Procedure division structure
        ("section", "Section"), ("section_header", "SectionHeader"),
        ("paragraph", "Paragraph"), ("paragraph_header", "ParagraphHeader"),
        ("sentence", "Sentence"),

        // Arithmetic statements
        ("add_statement", "AddStatement"), ("subtract_statement", "SubtractStatement"),
        ("multiply_statement", "MultiplyStatement"), ("divide_statement", "DivideStatement"),
        ("compute_statement", "ComputeStatement"),

        // Data movement statements
        ("move_statement", "AssignmentStatement"), ("set_statement", "SetStatement"),
        ("initialize_statement", "InitializeStatement"), ("inspect_statement", "InspectStatement"),
        ("string_statement", "StringStatement"), ("unstring_statement", "UnstringStatement"),

        // Control flow
        ("if_statement", "IfStatement"), ("else_clause", "ElseClause"),
        ("end_if", "EndIf"),
        ("evaluate_statement", "SwitchStatement"), ("when_clause", "CaseClause"),
        ("when_other_clause", "DefaultClause"), ("end_evaluate", "EndEvaluate"),

        // Loop statements
        ("perform_statement", "PerformStatement"),
        ("perform_inline", "PerformInline"), ("perform_procedure", "PerformProcedure"),
        ("perform_times", "PerformTimes"), ("perform_until", "PerformUntil"),
        ("perform_varying", "ForStatement"), ("end_perform", "EndPerform"),

        // Branching statements
        ("go_to_statement", "GotoStatement"), ("alter_statement", "AlterStatement"),
        ("stop_statement", "StopStatement"), ("exit_statement", "ExitStatement"),
        ("continue_statement", "ContinueStatement"),
        ("goback_statement", "ReturnStatement"), ("return_statement", "ReturnStatement"),

        // I/O statements
        ("open_statement", "OpenStatement"), ("close_statement", "CloseStatement"),
        ("read_statement", "ReadStatement"), ("write_statement", "WriteStatement"),
        ("rewrite_statement", "RewriteStatement"), ("delete_statement", "DeleteStatement"),
        ("start_statement", "StartStatement"),
        ("accept_statement", "AcceptStatement"), ("display_statement", "DisplayStatement"),

        // File handling
        ("file_status", "FileStatus"), ("at_end_clause", "AtEndClause"),
        ("not_at_end_clause", "NotAtEndClause"),
        ("invalid_key_clause", "InvalidKeyClause"), ("not_invalid_key_clause", "NotInvalidKeyClause"),

        // Error handling
        ("on_size_error_clause", "OnSizeErrorClause"),
        ("not_on_size_error_clause", "NotOnSizeErrorClause"),
        ("on_overflow_clause", "OnOverflowClause"), ("not_on_overflow_clause", "NotOnOverflowClause"),
        ("on_exception_clause", "OnExceptionClause"), ("not_on_exception_clause", "NotOnExceptionClause"),
        ("declaratives", "Declaratives"), ("use_statement", "UseStatement"),

        // Sort and merge
        ("sort_statement", "SortStatement"), ("merge_statement", "MergeStatement"),
        ("release_statement", "ReleaseStatement"),

        // Call statements
        ("call_statement", "CallExpression"), ("cancel_statement", "CancelStatement"),
        ("entry_statement", "EntryStatement"),
        ("using_clause", "UsingClause"), ("returning_clause", "ReturningClause"),

        // Expressions and identifiers
        ("identifier", "Identifier"), ("qualified_word", "QualifiedIdentifier"),
        ("subscript", "IndexExpression"), ("reference_modification", "SliceExpression"),
        ("arithmetic_expression", "BinaryExpression"), ("condition", "BinaryExpression"),
        ("relation_condition", "BinaryExpression"), ("class_condition", "ClassCondition"),
        ("sign_condition", "SignCondition"), ("condition_name_condition", "ConditionNameCondition"),
        ("combined_condition", "BinaryExpression"), ("negated_condition", "UnaryExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),

        // Literals
        ("numeric_literal", "NumberLiteral"), ("integer", "NumberLiteral"),
        ("decimal_number", "NumberLiteral"), ("floating_point_number", "NumberLiteral"),
        ("alphanumeric_literal", "StringLiteral"), ("string", "StringLiteral"),
        ("national_literal", "StringLiteral"), ("boolean_literal", "BooleanLiteral"),
        ("figurative_constant", "FigurativeConstant"),
        ("zero", "NullLiteral"), ("space", "SpaceLiteral"),
        ("high_value", "HighValueLiteral"), ("low_value", "LowValueLiteral"),
        ("quote", "QuoteLiteral"), ("all", "AllLiteral"),

        // Intrinsic functions
        ("function_call", "CallExpression"), ("intrinsic_function", "IntrinsicFunction"),

        // Comments
        ("comment", "Comment"), ("comment_entry", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Verilog-specific mappings (WASM on-demand language)
/// Verilog is a hardware description language for digital circuits.
static VERILOG_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Source structure
        ("source_file", "SourceFile"), ("source_text", "SourceFile"),
        ("description", "Description"),

        // Module declarations
        ("module_declaration", "ModuleDeclaration"),
        ("module_header", "ModuleHeader"), ("module_keyword", "ModuleKeyword"),
        ("module_identifier", "Identifier"),
        ("port_declaration", "PortDeclaration"), ("list_of_ports", "PortList"),
        ("list_of_port_declarations", "PortDeclarationList"),
        ("port", "Port"), ("port_expression", "PortExpression"),
        ("port_reference", "PortReference"), ("port_identifier", "Identifier"),
        ("input_declaration", "InputDeclaration"), ("output_declaration", "OutputDeclaration"),
        ("inout_declaration", "InoutDeclaration"),

        // Net and variable declarations
        ("net_declaration", "NetDeclaration"), ("wire_declaration", "WireDeclaration"),
        ("reg_declaration", "RegDeclaration"),
        ("integer_declaration", "IntegerDeclaration"), ("real_declaration", "RealDeclaration"),
        ("time_declaration", "TimeDeclaration"), ("realtime_declaration", "RealtimeDeclaration"),
        ("net_type", "NetType"), ("net_identifier", "Identifier"),
        ("dimension", "Dimension"), ("range", "Range"),
        ("msb_constant_expression", "MsbExpression"), ("lsb_constant_expression", "LsbExpression"),

        // Parameter declarations
        ("parameter_declaration", "ParameterDeclaration"),
        ("localparam_declaration", "LocalparamDeclaration"),
        ("specparam_declaration", "SpecparamDeclaration"),
        ("defparam_statement", "DefparamStatement"),

        // Behavioral constructs
        ("always_construct", "AlwaysBlock"), ("always_keyword", "AlwaysKeyword"),
        ("always_comb", "AlwaysComb"), ("always_ff", "AlwaysFf"),
        ("always_latch", "AlwaysLatch"),
        ("initial_construct", "InitialBlock"),
        ("final_construct", "FinalBlock"),

        // Sensitivity list
        ("event_control", "EventControl"), ("event_expression", "EventExpression"),
        ("edge_identifier", "EdgeIdentifier"), ("posedge", "Posedge"), ("negedge", "Negedge"),

        // Procedural statements
        ("procedural_timing_control_statement", "TimingControlStatement"),
        ("delay_control", "DelayControl"), ("delay_value", "DelayValue"),
        ("statement", "Statement"), ("statement_or_null", "StatementOrNull"),
        ("seq_block", "Block"), ("par_block", "ParallelBlock"),
        ("begin_keyword", "BeginKeyword"), ("end_keyword", "EndKeyword"),
        ("fork_keyword", "ForkKeyword"), ("join_keyword", "JoinKeyword"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"), ("else_clause", "ElseClause"),
        ("case_statement", "CaseStatement"), ("casex_statement", "CasexStatement"),
        ("casez_statement", "CasezStatement"),
        ("case_item", "CaseItem"), ("default_item", "DefaultItem"),

        // Control flow - loops
        ("loop_statement", "LoopStatement"),
        ("forever_statement", "ForeverStatement"),
        ("repeat_statement", "RepeatStatement"),
        ("while_statement", "WhileStatement"),
        ("for_statement", "ForStatement"),
        ("for_initialization", "ForInitialization"), ("for_step", "ForStep"),
        ("disable_statement", "DisableStatement"),

        // Assignments
        ("blocking_assignment", "BlockingAssignment"),
        ("nonblocking_assignment", "NonblockingAssignment"),
        ("continuous_assign", "ContinuousAssign"), ("assign_statement", "AssignStatement"),
        ("procedural_continuous_assignment", "ProceduralContinuousAssignment"),
        ("variable_lvalue", "LValue"), ("net_lvalue", "LValue"),

        // Expressions
        ("expression", "Expression"), ("primary", "Primary"),
        ("binary_expression", "BinaryExpression"), ("unary_expression", "UnaryExpression"),
        ("conditional_expression", "ConditionalExpression"),
        ("concatenation", "ConcatenationExpression"), ("multiple_concatenation", "ReplicationExpression"),
        ("mintypmax_expression", "MintypmaxExpression"),
        ("range_expression", "RangeExpression"), ("indexed_range", "IndexedRange"),
        ("part_select_range", "PartSelectRange"), ("bit_select", "BitSelect"),
        ("hierarchical_identifier", "HierarchicalIdentifier"),

        // Operators
        ("binary_operator", "BinaryOperator"), ("unary_operator", "UnaryOperator"),
        ("inc_or_dec_operator", "IncOrDecOperator"),

        // Function and task
        ("function_declaration", "FunctionDeclaration"), ("function_identifier", "Identifier"),
        ("function_body_declaration", "FunctionBody"),
        ("task_declaration", "TaskDeclaration"), ("task_identifier", "Identifier"),
        ("task_body_declaration", "TaskBody"),
        ("tf_declaration", "TfDeclaration"), ("tf_call", "TfCall"),
        ("system_tf_call", "SystemTfCall"),

        // Instances and primitives
        ("module_instantiation", "ModuleInstantiation"),
        ("module_instance", "ModuleInstance"), ("instance_identifier", "Identifier"),
        ("list_of_port_connections", "PortConnectionList"),
        ("named_port_connection", "NamedPortConnection"),
        ("ordered_port_connection", "OrderedPortConnection"),
        ("gate_instantiation", "GateInstantiation"),
        ("udp_instantiation", "UdpInstantiation"), ("udp_declaration", "UdpDeclaration"),

        // Generate constructs
        ("generate_region", "GenerateRegion"), ("generate_block", "GenerateBlock"),
        ("genvar_declaration", "GenvarDeclaration"),
        ("generate_loop_statement", "GenerateForLoop"),
        ("generate_conditional_statement", "GenerateIf"),
        ("generate_case_statement", "GenerateCase"),

        // Specify block (timing)
        ("specify_block", "SpecifyBlock"), ("specparam_assignment", "SpecparamAssignment"),
        ("path_declaration", "PathDeclaration"), ("timing_check", "TimingCheck"),

        // Attributes
        ("attribute_instance", "AttributeInstance"), ("attr_spec", "AttributeSpec"),

        // Literals
        ("number", "NumberLiteral"), ("integral_number", "IntegerLiteral"),
        ("decimal_number", "DecimalNumber"), ("binary_number", "BinaryNumber"),
        ("octal_number", "OctalNumber"), ("hex_number", "HexNumber"),
        ("real_number", "RealNumber"), ("unsigned_number", "UnsignedNumber"),
        ("string_literal", "StringLiteral"), ("time_literal", "TimeLiteral"),

        // Identifiers
        ("simple_identifier", "Identifier"), ("escaped_identifier", "EscapedIdentifier"),
        ("system_tf_identifier", "SystemIdentifier"),

        // Preprocessor
        ("include_statement", "IncludeDirective"), ("text_macro_usage", "MacroUsage"),
        ("text_macro_definition", "MacroDefinition"),
        ("ifdef_directive", "IfdefDirective"), ("ifndef_directive", "IfndefDirective"),
        ("else_directive", "ElseDirective"), ("endif_directive", "EndifDirective"),
        ("timescale_directive", "TimescaleDirective"),

        // Comments
        ("comment", "Comment"), ("line_comment", "LineComment"), ("block_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// LaTeX-specific mappings (document typesetting language)
static LATEX_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add LaTeX-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),
        ("source_file", "SourceFile"),
        ("section", "Section"),
        ("subsection", "Subsection"),
        ("subsubsection", "Subsubsection"),
        ("chapter", "Chapter"),
        ("part", "Part"),
        ("paragraph", "Paragraph"),
        ("subparagraph", "Subparagraph"),

        // Commands and environments
        ("command", "Command"),
        ("generic_command", "Command"),
        ("environment", "Environment"),
        ("generic_environment", "Environment"),
        ("math_environment", "MathEnvironment"),
        ("displayed_equation", "Equation"),
        ("inline_formula", "InlineFormula"),
        ("equation", "Equation"),

        // Environment delimiters
        ("begin", "BeginEnvironment"),
        ("end", "EndEnvironment"),
        ("text_mode", "TextMode"),
        ("math_mode", "MathMode"),

        // Package and document management
        ("package_include", "PackageImport"),
        ("usepackage", "PackageImport"),
        ("documentclass", "DocumentClass"),
        ("input", "InputDirective"),
        ("include", "IncludeDirective"),

        // Text formatting
        ("text", "Text"),
        ("word", "Word"),
        ("curly_group", "CurlyGroup"),
        ("brack_group", "BracketGroup"),
        ("mixed_group", "MixedGroup"),

        // Labels and references
        ("label_definition", "LabelDefinition"),
        ("label_reference", "LabelReference"),
        ("citation", "Citation"),
        ("new_command_definition", "CommandDefinition"),

        // Math content
        ("superscript", "Superscript"),
        ("subscript", "Subscript"),
        ("fraction", "Fraction"),
        ("sqrt", "SquareRoot"),
        ("operator", "Operator"),

        // Comments
        ("comment", "Comment"),
        ("line_comment", "LineComment"),
        ("block_comment", "BlockComment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Nushell-specific mappings (modern shell language)
static NUSHELL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Nushell-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("nu_script", "SourceFile"),

        // Declarations
        ("decl_def", "FunctionDeclaration"),
        ("decl_alias", "AliasDeclaration"),
        ("decl_module", "ModuleDeclaration"),
        ("decl_use", "ImportDeclaration"),
        ("decl_export", "ExportDeclaration"),
        ("decl_extern", "ExternDeclaration"),

        // Statements
        ("stmt_let", "VariableDeclaration"),
        ("stmt_mut", "MutableVariableDeclaration"),
        ("stmt_const", "ConstDeclaration"),
        ("assignment", "AssignmentExpression"),

        // Control flow
        ("ctrl_if", "IfStatement"),
        ("ctrl_for", "ForStatement"),
        ("ctrl_while", "WhileStatement"),
        ("ctrl_loop", "LoopStatement"),
        ("ctrl_match", "MatchExpression"),
        ("ctrl_try", "TryStatement"),

        // Match constructs
        ("match_arm", "MatchArm"),
        ("match_guard", "MatchGuard"),
        ("match_pattern", "MatchPattern"),
        ("default_arm", "DefaultArm"),

        // Expressions
        ("expr_binary", "BinaryExpression"),
        ("expr_unary", "UnaryExpression"),
        ("expr_parenthesized", "ParenthesizedExpression"),
        ("expr_interpolated", "InterpolatedExpression"),

        // Commands and pipelines
        ("command", "CommandExpression"),
        ("command_list", "CommandList"),
        ("pipeline", "PipelineExpression"),
        ("pipe_element", "PipeElement"),
        ("where_command", "WhereCommand"),
        ("where_predicate", "WherePredicate"),

        // Literals
        ("val_string", "StringLiteral"),
        ("val_number", "NumberLiteral"),
        ("val_bool", "BooleanLiteral"),
        ("val_nothing", "NullLiteral"),
        ("val_date", "DateLiteral"),
        ("val_duration", "DurationLiteral"),
        ("val_filesize", "FilesizeLiteral"),
        ("val_binary", "BinaryLiteral"),
        ("val_range", "RangeLiteral"),
        ("val_interpolated", "InterpolatedString"),

        // Collections
        ("val_list", "ArrayExpression"),
        ("val_record", "ObjectExpression"),
        ("val_table", "TableExpression"),
        ("list_body", "ListBody"),
        ("record_body", "RecordBody"),
        ("record_entry", "KeyValuePair"),
        ("val_entry", "Entry"),

        // Closures
        ("val_closure", "ClosureExpression"),

        // Cell paths (Nushell-specific navigation)
        ("val_cellpath", "CellPath"),
        ("cell_path", "CellPath"),

        // Variables
        ("val_variable", "VariableReference"),
        ("env_var", "EnvironmentVariable"),

        // Parameters
        ("parameter", "Parameter"),
        ("parameter_bracks", "OptionalParameter"),
        ("parameter_parens", "ParenthesizedParameter"),
        ("parameter_pipes", "PipedParameter"),
        ("param_long_flag", "LongFlag"),
        ("param_short_flag", "ShortFlag"),
        ("param_opt", "OptionalParameter"),
        ("param_rest", "RestParameter"),
        ("param_type", "ParameterType"),
        ("param_value", "ParameterValue"),
        ("param_completer", "ParameterCompleter"),
        ("returns", "ReturnType"),

        // Flags
        ("long_flag", "LongFlag"),
        ("short_flag", "ShortFlag"),

        // Types
        ("flat_type", "TypeReference"),
        ("collection_type", "CollectionType"),
        ("composite_type", "CompositeType"),
        ("list_type", "ListType"),

        // Other
        ("block", "Block"),
        ("identifier", "Identifier"),
        ("cmd_identifier", "CommandIdentifier"),
        ("path", "Path"),
        ("scope_pattern", "ScopePattern"),
        ("wild_card", "Wildcard"),
        ("redirection", "Redirection"),
        ("string_content", "StringContent"),
        ("unquoted", "UnquotedString"),
        ("attribute", "Attribute"),
        ("attribute_list", "AttributeList"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Starlark/Bazel-specific mappings (Python-like build language)
static STARLARK_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all Python mappings first (Starlark is Python-derived)
    for (k, v) in &PYTHON_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add/override Starlark-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("module", "SourceFile"),

        // Declarations
        ("function_definition", "FunctionDeclaration"),
        ("decorated_definition", "DecoratedDefinition"),

        // Statements
        ("if_statement", "IfStatement"),
        ("for_statement", "ForStatement"),
        ("while_statement", "WhileStatement"),
        ("match_statement", "MatchStatement"),
        ("with_statement", "WithStatement"),
        ("assert_statement", "AssertStatement"),
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("pass_statement", "PassStatement"),
        ("return_statement", "ReturnStatement"),
        ("delete_statement", "DeleteStatement"),
        ("exec_statement", "ExecStatement"),
        ("print_statement", "PrintStatement"),
        ("expression_statement", "ExpressionStatement"),
        ("assignment", "AssignmentExpression"),
        ("augmented_assignment", "AugmentedAssignmentExpression"),

        // Control flow clauses
        ("elif_clause", "ElseIfClause"),
        ("else_clause", "ElseClause"),
        ("if_clause", "IfClause"),
        ("case_clause", "CaseClause"),
        ("with_clause", "WithClause"),
        ("with_item", "WithItem"),
        ("for_in_clause", "ForInClause"),

        // Expressions
        ("expression", "Expression"),
        ("conditional_expression", "ConditionalExpression"),
        ("boolean_operator", "BooleanExpression"),
        ("comparison_operator", "ComparisonExpression"),
        ("not_operator", "NotExpression"),
        ("lambda", "LambdaExpression"),
        ("named_expression", "NamedExpression"),
        ("as_pattern", "AsPattern"),
        ("binary_operator", "BinaryExpression"),
        ("unary_operator", "UnaryExpression"),
        ("call", "CallExpression"),
        ("attribute", "MemberExpression"),
        ("subscript", "IndexExpression"),
        ("slice", "SliceExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),

        // Literals
        ("string", "StringLiteral"),
        ("concatenated_string", "ConcatenatedString"),
        ("integer", "NumberLiteral"),
        ("float", "NumberLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("none", "NullLiteral"),
        ("ellipsis", "EllipsisLiteral"),

        // Collections
        ("list", "ArrayExpression"),
        ("list_comprehension", "ListComprehension"),
        ("dictionary", "ObjectExpression"),
        ("dictionary_comprehension", "DictionaryComprehension"),
        ("set", "SetExpression"),
        ("set_comprehension", "SetComprehension"),
        ("tuple", "TupleExpression"),

        // Parameters and arguments
        ("parameters", "ParameterList"),
        ("parameter", "Parameter"),
        ("default_parameter", "DefaultParameter"),
        ("typed_parameter", "TypedParameter"),
        ("typed_default_parameter", "TypedDefaultParameter"),
        ("list_splat_pattern", "RestParameter"),
        ("dictionary_splat_pattern", "KeywordRestParameter"),
        ("keyword_separator", "KeywordSeparator"),
        ("positional_separator", "PositionalSeparator"),
        ("lambda_parameters", "LambdaParameters"),
        ("argument_list", "ArgumentList"),
        ("keyword_argument", "KeywordArgument"),
        ("list_splat", "SpreadArgument"),
        ("dictionary_splat", "DictionarySpread"),
        ("parenthesized_list_splat", "ParenthesizedSpread"),

        // Patterns
        ("pattern", "Pattern"),
        ("tuple_pattern", "TuplePattern"),
        ("list_pattern", "ListPattern"),
        ("splat_pattern", "SplatPattern"),
        ("case_pattern", "CasePattern"),
        ("class_pattern", "ClassPattern"),
        ("complex_pattern", "ComplexPattern"),
        ("dict_pattern", "DictPattern"),
        ("keyword_pattern", "KeywordPattern"),
        ("union_pattern", "UnionPattern"),
        ("pattern_list", "PatternList"),

        // Types
        ("type", "TypeReference"),
        ("generic_type", "GenericType"),
        ("constrained_type", "ConstrainedType"),
        ("member_type", "MemberType"),
        ("union_type", "UnionType"),
        ("splat_type", "SplatType"),
        ("type_parameter", "TypeParameter"),

        // Other
        ("block", "Block"),
        ("identifier", "Identifier"),
        ("dotted_name", "QualifiedIdentifier"),
        ("expression_list", "ExpressionList"),
        ("pair", "KeyValuePair"),
        ("format_expression", "FormatExpression"),
        ("format_specifier", "FormatSpecifier"),
        ("interpolation", "StringInterpolation"),
        ("chevron", "Chevron"),
        ("comment", "Comment"),
        ("escape_sequence", "EscapeSequence"),
        ("string_content", "StringContent"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Solidity-specific mappings (Ethereum smart contract language)
static SOLIDITY_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Solidity-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("source_file", "SourceFile"),

        // Declarations
        ("contract_declaration", "ContractDeclaration"),
        ("contract_body", "ContractBody"),
        ("interface_declaration", "InterfaceDeclaration"),
        ("library_declaration", "LibraryDeclaration"),
        ("function_definition", "FunctionDeclaration"),
        ("function_body", "FunctionBody"),
        ("constructor_definition", "ConstructorDeclaration"),
        ("modifier_definition", "ModifierDeclaration"),
        ("modifier_invocation", "ModifierInvocation"),
        ("fallback_receive_definition", "FallbackDeclaration"),
        ("event_definition", "EventDeclaration"),
        ("event_parameter", "EventParameter"),
        ("error_declaration", "ErrorDeclaration"),
        ("error_parameter", "ErrorParameter"),
        ("struct_declaration", "StructDeclaration"),
        ("struct_body", "StructBody"),
        ("struct_member", "StructMember"),
        ("enum_declaration", "EnumDeclaration"),
        ("enum_body", "EnumBody"),
        ("state_variable_declaration", "StateVariableDeclaration"),
        ("constant_variable_declaration", "ConstantDeclaration"),
        ("variable_declaration", "VariableDeclaration"),
        ("variable_declaration_statement", "VariableDeclarationStatement"),
        ("variable_declaration_tuple", "DestructuringDeclaration"),
        ("type_alias", "TypeAlias"),
        ("user_defined_type_definition", "UserDefinedTypeDefinition"),
        ("user_defined_type", "UserDefinedType"),

        // Import/Export
        ("import_directive", "ImportDeclaration"),
        ("pragma_directive", "PragmaDirective"),
        ("pragma_value", "PragmaValue"),
        ("solidity_pragma_token", "SolidityPragma"),
        ("any_pragma_token", "AnyPragma"),
        ("solidity_version_comparison_operator", "VersionComparison"),

        // Using directive
        ("using_directive", "UsingDirective"),
        ("using_alias", "UsingAlias"),
        ("user_definable_operator", "UserDefinableOperator"),

        // Inheritance
        ("inheritance_specifier", "InheritanceSpecifier"),

        // Statements
        ("block_statement", "Block"),
        ("if_statement", "IfStatement"),
        ("for_statement", "ForStatement"),
        ("while_statement", "WhileStatement"),
        ("do_while_statement", "DoWhileStatement"),
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("return_statement", "ReturnStatement"),
        ("emit_statement", "EmitStatement"),
        ("revert_statement", "RevertStatement"),
        ("revert_arguments", "RevertArguments"),
        ("expression_statement", "ExpressionStatement"),

        // Error handling
        ("try_statement", "TryStatement"),
        ("catch_clause", "CatchClause"),

        // Expressions
        ("expression", "Expression"),
        ("call_expression", "CallExpression"),
        ("call_argument", "Argument"),
        ("call_struct_argument", "StructArgument"),
        ("member_expression", "MemberExpression"),
        ("array_access", "IndexExpression"),
        ("slice_access", "SliceExpression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("update_expression", "UpdateExpression"),
        ("assignment_expression", "AssignmentExpression"),
        ("augmented_assignment_expression", "AugmentedAssignmentExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("tuple_expression", "TupleExpression"),
        ("inline_array_expression", "ArrayExpression"),
        ("new_expression", "NewExpression"),
        ("type_cast_expression", "CastExpression"),
        ("payable_conversion_expression", "PayableConversion"),
        ("meta_type_expression", "MetaTypeExpression"),
        ("struct_expression", "StructExpression"),
        ("struct_field_assignment", "StructFieldAssignment"),

        // Literals
        ("string_literal", "StringLiteral"),
        ("hex_string_literal", "HexStringLiteral"),
        ("unicode_string_literal", "UnicodeStringLiteral"),
        ("number_literal", "NumberLiteral"),
        ("number_unit", "NumberUnit"),
        ("boolean_literal", "BooleanLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),

        // Types
        ("type_name", "TypeReference"),
        ("primitive_type", "PrimitiveType"),

        // Parameters
        ("parameter", "Parameter"),
        ("return_parameter", "ReturnParameter"),
        ("return_type_definition", "ReturnTypeDefinition"),

        // Visibility and state
        ("visibility", "Visibility"),
        ("state_mutability", "StateMutability"),
        ("state_location", "StateLocation"),
        ("layout_specifier", "LayoutSpecifier"),
        ("override_specifier", "OverrideSpecifier"),

        // Inline assembly (Yul)
        ("assembly_statement", "AssemblyBlock"),
        ("assembly_flags", "AssemblyFlags"),
        ("yul_block", "YulBlock"),
        ("yul_assignment", "YulAssignment"),
        ("yul_variable_declaration", "YulVariableDeclaration"),
        ("yul_function_definition", "YulFunctionDeclaration"),
        ("yul_function_call", "YulCallExpression"),
        ("yul_if_statement", "YulIfStatement"),
        ("yul_for_statement", "YulForStatement"),
        ("yul_switch_statement", "YulSwitchStatement"),
        ("yul_break", "YulBreakStatement"),
        ("yul_continue", "YulContinueStatement"),
        ("yul_leave", "YulLeaveStatement"),
        ("yul_identifier", "YulIdentifier"),
        ("yul_path", "YulPath"),
        ("yul_label", "YulLabel"),
        ("yul_decimal_number", "YulNumberLiteral"),
        ("yul_hex_number", "YulHexLiteral"),
        ("yul_string_literal", "YulStringLiteral"),
        ("yul_hex_string_literal", "YulHexStringLiteral"),
        ("yul_boolean", "YulBooleanLiteral"),
        ("yul_evm_builtin", "YulBuiltin"),

        // Other
        ("identifier", "Identifier"),
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// LLVM IR-specific mappings (LLVM Intermediate Representation)
static LLVM_IR_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add LLVM IR-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("module", "Module"),
        ("source_file_name", "SourceFileName"),
        ("target_definition", "TargetDefinition"),
        ("target_triple", "TargetTriple"),
        ("data_layout", "DataLayout"),

        // Declarations
        ("fn_define", "FunctionDeclaration"),
        ("declare", "FunctionDeclaration"),
        ("function_header", "FunctionHeader"),
        ("function_body", "FunctionBody"),
        ("global_global", "GlobalDeclaration"),
        ("global_type", "TypeDeclaration"),
        ("alias", "AliasDeclaration"),
        ("ifunc", "IFuncDeclaration"),
        ("comdat", "ComdatDeclaration"),

        // Instructions
        ("instruction", "Instruction"),
        ("instruction_ret", "ReturnInstruction"),
        ("instruction_br", "BranchInstruction"),
        ("instruction_switch", "SwitchInstruction"),
        ("instruction_indirectbr", "IndirectBranchInstruction"),
        ("instruction_invoke", "InvokeInstruction"),
        ("instruction_callbr", "CallBrInstruction"),
        ("instruction_resume", "ResumeInstruction"),
        ("instruction_unreachable", "UnreachableInstruction"),
        ("instruction_call", "CallInstruction"),
        ("instruction_alloca", "AllocaInstruction"),
        ("instruction_load", "LoadInstruction"),
        ("instruction_store", "StoreInstruction"),
        ("instruction_fence", "FenceInstruction"),
        ("instruction_cmpxchg", "CmpXchgInstruction"),
        ("instruction_atomicrmw", "AtomicRMWInstruction"),
        ("instruction_getelementptr", "GEPInstruction"),
        ("instruction_bin_op", "BinaryInstruction"),
        ("instruction_fneg", "FNegInstruction"),
        ("instruction_icmp", "ICmpInstruction"),
        ("instruction_fcmp", "FCmpInstruction"),
        ("instruction_phi", "PhiInstruction"),
        ("instruction_select", "SelectInstruction"),
        ("instruction_freeze", "FreezeInstruction"),
        ("instruction_cast", "CastInstruction"),
        ("instruction_va_arg", "VAArgInstruction"),
        ("instruction_extractelement", "ExtractElementInstruction"),
        ("instruction_insertelement", "InsertElementInstruction"),
        ("instruction_shufflevector", "ShuffleVectorInstruction"),
        ("instruction_extractvalue", "ExtractValueInstruction"),
        ("instruction_insertvalue", "InsertValueInstruction"),
        ("instruction_landingpad", "LandingPadInstruction"),
        ("instruction_catchpad", "CatchPadInstruction"),
        ("instruction_cleanuppad", "CleanupPadInstruction"),
        ("instruction_catchret", "CatchRetInstruction"),
        ("instruction_cleanupret", "CleanupRetInstruction"),
        ("instruction_catchswitch", "CatchSwitchInstruction"),

        // Constant expressions
        ("constant_expr", "ConstantExpression"),
        ("constant_bin_op", "ConstantBinaryOp"),
        ("constant_cast", "ConstantCast"),
        ("constant_icmp", "ConstantICmp"),
        ("constant_fcmp", "ConstantFCmp"),
        ("constant_fneg", "ConstantFNeg"),
        ("constant_getelementptr", "ConstantGEP"),
        ("constant_select", "ConstantSelect"),
        ("constant_extractelement", "ConstantExtractElement"),
        ("constant_insertelement", "ConstantInsertElement"),
        ("constant_shufflevector", "ConstantShuffleVector"),
        ("constant_extractvalue", "ConstantExtractValue"),
        ("constant_insertvalue", "ConstantInsertValue"),
        ("blockaddress", "BlockAddress"),

        // Types
        ("type", "TypeReference"),
        ("type_keyword", "PrimitiveType"),
        ("array_type", "ArrayType"),
        ("vector_type", "VectorType"),
        ("struct_type", "StructType"),
        ("packed_struct_type", "PackedStructType"),

        // Values and operands
        ("value", "Value"),
        ("type_and_value", "TypedValue"),
        ("argument", "Argument"),
        ("argument_list", "ArgumentList"),
        ("operand_bundles", "OperandBundles"),

        // Literals
        ("number", "NumberLiteral"),
        ("float", "FloatLiteral"),
        ("cstring", "StringLiteral"),
        ("string", "StringLiteral"),
        ("array_value", "ArrayLiteral"),
        ("array_vector_body", "ArrayVectorBody"),
        ("vector_value", "VectorLiteral"),
        ("struct_value", "StructLiteral"),
        ("struct_body", "StructBody"),
        ("packed_struct_value", "PackedStructLiteral"),

        // Variables
        ("global_var", "GlobalVariable"),
        ("local_var", "LocalVariable"),
        ("var", "Variable"),
        ("label", "Label"),

        // Metadata
        ("metadata", "Metadata"),
        ("metadata_ref", "MetadataRef"),
        ("metadata_refs", "MetadataRefs"),
        ("metadata_name", "MetadataName"),
        ("metadata_attachment", "MetadataAttachment"),
        ("metadata_tuple", "MetadataTuple"),
        ("global_metadata", "GlobalMetadata"),
        ("specialized_md", "SpecializedMetadata"),
        ("specialized_md_value", "SpecializedMetadataValue"),

        // Attributes
        ("attribute", "Attribute"),
        ("attribute_name", "AttributeName"),
        ("param_or_return_attrs", "ParameterAttributes"),
        ("unnamed_attr_grp", "UnnamedAttributeGroup"),
        ("attr_ref", "AttributeRef"),
        ("memory_attribute", "MemoryAttribute"),
        ("memory_attribute_val", "MemoryAttributeValue"),

        // Linkage and visibility
        ("linkage", "Linkage"),
        ("linkage_aux", "LinkageAux"),
        ("visibility", "Visibility"),
        ("dll_storage_class", "DllStorageClass"),
        ("thread_local", "ThreadLocal"),
        ("unnamed_addr", "UnnamedAddr"),
        ("addrspace", "AddressSpace"),
        ("dso_local", "DsoLocal"),

        // Calling conventions
        ("calling_conv", "CallingConvention"),

        // Atomic operations
        ("atomic_ordering", "AtomicOrdering"),
        ("atomic_bin_op_keyword", "AtomicBinOp"),
        ("syncscope", "SyncScope"),

        // Fast math flags
        ("fast_math", "FastMathFlags"),

        // Comparison conditions
        ("icmp_cond", "ICmpCondition"),
        ("fcmp_cond", "FCmpCondition"),

        // Binary operations
        ("bin_op_keyword", "BinaryOpKeyword"),
        ("cast_inst", "CastInstruction"),

        // Exception handling
        ("landingpad_clause", "LandingPadClause"),

        // Inline assembly
        ("inline_asm", "InlineAsm"),
        ("asm", "Asm"),

        // Module-level
        ("module_asm", "ModuleAsm"),
        ("use_list_order", "UseListOrder"),
        ("use_list_order_bb", "UseListOrderBB"),
        ("uwtable", "UWTable"),
        ("alignment", "Alignment"),

        // Summary
        ("summary_entry", "SummaryEntry"),
        ("summary_value", "SummaryValue"),
        ("summary_ref", "SummaryRef"),

        // Comdats
        ("comdat_ref", "ComdatRef"),

        // Comments
        ("comment", "Comment"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Odin-specific mappings (modern systems programming language)
static ODIN_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Odin-specific mappings
    pairs.extend_from_slice(&[
        // Program structure
        ("source_file", "SourceFile"),
        ("package_declaration", "PackageDeclaration"),

        // Declarations
        ("declaration", "Declaration"),
        ("procedure_declaration", "FunctionDeclaration"),
        ("overloaded_procedure_declaration", "OverloadedFunctionDeclaration"),
        ("import_declaration", "ImportDeclaration"),
        ("var_declaration", "VariableDeclaration"),
        ("variable_declaration", "VariableDeclaration"),
        ("const_declaration", "ConstDeclaration"),
        ("const_type_declaration", "ConstTypeDeclaration"),
        ("struct_declaration", "StructDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("union_declaration", "UnionDeclaration"),
        ("bit_field_declaration", "BitFieldDeclaration"),
        ("foreign_block", "ForeignBlock"),

        // Procedure parts
        ("procedure", "Procedure"),
        ("procedure_type", "ProcedureType"),
        ("parameters", "ParameterList"),
        ("parameter", "Parameter"),
        ("default_parameter", "DefaultParameter"),
        ("polymorphic_parameters", "PolymorphicParameters"),
        ("calling_convention", "CallingConvention"),
        ("where_clause", "WhereClause"),

        // Statements
        ("statement", "Statement"),
        ("block", "Block"),
        ("tagged_block", "TaggedBlock"),
        ("assignment_statement", "AssignmentStatement"),
        ("update_statement", "UpdateStatement"),
        ("return_statement", "ReturnStatement"),
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("fallthrough_statement", "FallthroughStatement"),
        ("if_statement", "IfStatement"),
        ("switch_statement", "SwitchStatement"),
        ("switch_case", "SwitchCase"),
        ("for_statement", "ForStatement"),
        ("defer_statement", "DeferStatement"),
        ("using_statement", "UsingStatement"),
        ("when_statement", "WhenStatement"),
        ("label_statement", "LabelStatement"),

        // Expressions
        ("expression", "Expression"),
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("call_expression", "CallExpression"),
        ("selector_call_expression", "SelectorCallExpression"),
        ("index_expression", "IndexExpression"),
        ("slice_expression", "SliceExpression"),
        ("member_expression", "MemberExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("address", "AddressExpression"),
        ("cast_expression", "CastExpression"),
        ("in_expression", "InExpression"),
        ("range_expression", "RangeExpression"),
        ("variadic_expression", "VariadicExpression"),
        ("or_break_expression", "OrBreakExpression"),
        ("or_continue_expression", "OrContinueExpression"),
        ("or_return_expression", "OrReturnExpression"),

        // Literals
        ("literal", "Literal"),
        ("number", "NumberLiteral"),
        ("float", "FloatLiteral"),
        ("string", "StringLiteral"),
        ("boolean", "BooleanLiteral"),
        ("character", "CharacterLiteral"),
        ("nil", "NullLiteral"),
        ("uninitialized", "UninitializedLiteral"),

        // Composite literals
        ("struct", "StructLiteral"),
        ("map", "MapLiteral"),
        ("matrix", "MatrixLiteral"),
        ("bit_set", "BitSetLiteral"),

        // Types
        ("type", "TypeReference"),
        ("named_type", "NamedType"),
        ("array_type", "ArrayType"),
        ("pointer_type", "PointerType"),
        ("map_type", "MapType"),
        ("matrix_type", "MatrixType"),
        ("bit_set_type", "BitSetType"),
        ("bit_field_type", "BitFieldType"),
        ("struct_type", "StructType"),
        ("enum_type", "EnumType"),
        ("union_type", "UnionType"),
        ("tuple_type", "TupleType"),
        ("variadic_type", "VariadicType"),
        ("conditional_type", "ConditionalType"),
        ("constant_type", "ConstantType"),
        ("polymorphic_type", "PolymorphicType"),
        ("specialized_type", "SpecializedType"),
        ("distinct_type", "DistinctType"),
        ("field_type", "FieldType"),
        ("empty_type", "EmptyType"),

        // Struct members
        ("field", "Field"),
        ("struct_member", "StructMember"),
        ("struct_field", "StructField"),

        // Attributes
        ("attribute", "Attribute"),
        ("attributes", "AttributeList"),
        ("tag", "Tag"),
        ("build_tag", "BuildTag"),

        // Identifiers
        ("identifier", "Identifier"),
        ("field_identifier", "FieldIdentifier"),

        // Comments
        ("comment", "Comment"),
        ("block_comment", "BlockComment"),

        // Other
        ("escape_sequence", "EscapeSequence"),
        ("string_content", "StringContent"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// Markdown-specific mappings (documentation format)
static MARKDOWN_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();

    // Include all generic mappings first
    for (k, v) in &GENERIC_MAPPINGS.mappings {
        pairs.push((*k, *v));
    }

    // Then add Markdown-specific mappings
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),
        ("section", "Section"),

        // Headings
        ("atx_heading", "Heading"),
        ("setext_heading", "Heading"),
        ("heading_content", "HeadingContent"),

        // Text blocks
        ("paragraph", "Paragraph"),
        ("block_quote", "BlockQuote"),
        ("thematic_break", "ThematicBreak"),
        ("html_block", "HtmlBlock"),

        // Code
        ("fenced_code_block", "CodeBlock"),
        ("indented_code_block", "CodeBlock"),
        ("code_span", "InlineCode"),
        ("code_fence_content", "CodeContent"),
        ("info_string", "CodeLanguage"),

        // Lists
        ("list", "List"),
        ("list_item", "ListItem"),
        ("task_list_marker", "TaskListMarker"),

        // Links and images
        ("link", "Link"),
        ("image", "Image"),
        ("link_destination", "LinkDestination"),
        ("link_title", "LinkTitle"),
        ("link_text", "LinkText"),
        ("link_label", "LinkLabel"),
        ("link_reference_definition", "LinkReferenceDefinition"),

        // Tables (GFM extension)
        ("pipe_table", "Table"),
        ("table_header_row", "TableHeaderRow"),
        ("table_row", "TableRow"),
        ("table_cell", "TableCell"),
        ("table_delimiter_row", "TableDelimiterRow"),

        // Inline formatting
        ("emphasis", "Emphasis"),
        ("strong_emphasis", "StrongEmphasis"),
        ("strikethrough", "Strikethrough"),
        ("hard_line_break", "LineBreak"),
        ("soft_line_break", "SoftLineBreak"),

        // Raw content
        ("text", "Text"),
        ("html_tag", "HtmlTag"),
        ("entity_reference", "EntityReference"),
        ("numeric_character_reference", "NumericCharacterReference"),

        // Frontmatter (common extension)
        ("front_matter", "FrontMatter"),
        ("yaml_block", "YamlBlock"),
    ]);

    NodeKindMappings::new(&pairs)
});

/// CSV-specific mappings (tabular data format)
/// CSV is a simple data format with rows, fields, and primitive values.
static CSV_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),

        // Rows and fields
        ("row", "Row"),
        ("field", "Field"),

        // Data types / literals
        ("boolean", "BooleanLiteral"),
        ("float", "NumberLiteral"),
        ("number", "NumberLiteral"),
        ("text", "StringLiteral"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Comment-specific mappings (comment parsing for TODO, FIXME, etc.)
/// Comment grammar parses structured comment content like tags, URIs, and user references.
static COMMENT_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Document structure
        ("source", "SourceFile"),

        // Comment elements
        ("tag", "Tag"),
        ("name", "Identifier"),
        ("uri", "Uri"),
        ("user", "UserReference"),
        ("text", "Text"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Doxygen-specific mappings (documentation comment format)
/// Doxygen is a documentation generator supporting special tags and code blocks.
static DOXYGEN_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),
        ("description", "Description"),
        ("brief_description", "BriefDescription"),
        ("brief_header", "BriefHeader"),
        ("brief_text", "BriefText"),

        // Code elements
        ("code_block", "CodeBlock"),
        ("code_block_content", "CodeBlockContent"),
        ("code_block_start", "CodeBlockStart"),
        ("code_block_end", "CodeBlockEnd"),
        ("code_block_language", "CodeLanguage"),
        ("code_word", "InlineCode"),
        ("code", "InlineCode"),

        // Identifiers and references
        ("identifier", "Identifier"),
        ("qualified_identifier", "QualifiedIdentifier"),
        ("function", "FunctionReference"),
        ("function_link", "FunctionLink"),
        ("link", "Link"),
        ("type", "TypeReference"),

        // Tags and metadata
        ("tag", "Tag"),
        ("tag_name", "TagName"),
        ("storageclass", "StorageClass"),

        // Text formatting
        ("emphasis", "Emphasis"),
        ("text", "Text"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Vim script-specific mappings (Vim editor scripting language)
/// Vim script is used for configuring and extending the Vim editor.
static VIM_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Function definitions
        ("function_definition", "FunctionDeclaration"),
        ("function_declaration", "FunctionDeclaration"),
        ("lambda_expression", "LambdaExpression"),
        ("parameters", "ParameterList"),
        ("argument", "Parameter"),
        ("default_parameter", "DefaultParameter"),
        ("spread", "SpreadElement"),

        // Control flow - conditionals
        ("if_statement", "IfStatement"),
        ("else_statement", "ElseClause"),
        ("elseif_statement", "ElseIfClause"),

        // Control flow - loops
        ("for_loop", "ForStatement"),
        ("while_loop", "WhileStatement"),

        // Control flow - try/catch
        ("try_statement", "TryStatement"),
        ("catch_statement", "CatchClause"),
        ("finally_statement", "FinallyClause"),
        ("throw_statement", "ThrowStatement"),

        // Control flow - jumps
        ("break_statement", "BreakStatement"),
        ("continue_statement", "ContinueStatement"),
        ("return_statement", "ReturnStatement"),

        // Expressions
        ("call_expression", "CallExpression"),
        ("method_expression", "MethodExpression"),
        ("binary_operation", "BinaryExpression"),
        ("unary_operation", "UnaryExpression"),
        ("ternary_expression", "ConditionalExpression"),
        ("field_expression", "MemberExpression"),
        ("index_expression", "IndexExpression"),
        ("slice_expression", "SliceExpression"),

        // Variable and assignment
        ("let_statement", "VariableDeclaration"),
        ("const_statement", "ConstDeclaration"),
        ("unlet_statement", "UnletStatement"),
        ("list_assignment", "DestructuringAssignment"),

        // Literals
        ("string_literal", "StringLiteral"),
        ("integer_literal", "NumberLiteral"),
        ("float_literal", "NumberLiteral"),
        ("list", "ArrayExpression"),
        ("dictionnary", "ObjectExpression"),
        ("literal_dictionary", "ObjectExpression"),
        ("literal_key", "PropertyKey"),
        ("heredoc", "HeredocLiteral"),

        // Identifiers and scopes
        ("identifier", "Identifier"),
        ("scoped_identifier", "ScopedIdentifier"),
        ("scope_dict", "ScopeDict"),
        ("scope", "Scope"),
        ("curly_braces_name", "DynamicIdentifier"),

        // Variables
        ("env_variable", "EnvironmentVariable"),
        ("option", "OptionVariable"),
        ("register", "RegisterVariable"),

        // Commands
        ("command", "Command"),
        ("user_command", "UserCommand"),
        ("command_statement", "CommandStatement"),
        ("command_name", "CommandName"),
        ("command_argument", "CommandArgument"),
        ("command_attribute", "CommandAttribute"),
        ("commands", "CommandList"),
        ("arguments", "ArgumentList"),
        ("bang", "BangModifier"),
        ("bangs", "BangModifiers"),

        // Echo/Output statements
        ("echo_statement", "EchoStatement"),
        ("echon_statement", "EchonStatement"),
        ("echoerr_statement", "EchoErrStatement"),
        ("echomsg_statement", "EchoMsgStatement"),
        ("echohl_statement", "EchoHlStatement"),

        // Execute and eval
        ("execute_statement", "ExecuteStatement"),
        ("eval_statement", "EvalStatement"),
        ("call_statement", "CallStatement"),
        ("normal_statement", "NormalStatement"),

        // Source/Runtime
        ("source_statement", "SourceStatement"),
        ("runtime_statement", "RuntimeStatement"),
        ("script", "Script"),
        ("script_file", "ScriptFile"),

        // Autocommands
        ("autocmd_statement", "AutocmdStatement"),
        ("augroup_statement", "AugroupStatement"),
        ("augroup_name", "AugroupName"),
        ("au_event", "AutocmdEvent"),
        ("au_event_list", "AutocmdEventList"),
        ("au_once", "AutocmdOnce"),
        ("au_nested", "AutocmdNested"),

        // Mappings
        ("map_statement", "MapStatement"),

        // Syntax and highlighting
        ("syntax_statement", "SyntaxStatement"),
        ("highlight_statement", "HighlightStatement"),
        ("hl_attribute", "HighlightAttribute"),
        ("hl_group", "HighlightGroup"),
        ("hl_groups", "HighlightGroups"),

        // Settings
        ("set_statement", "SetStatement"),
        ("setlocal_statement", "SetLocalStatement"),
        ("setfiletype_statement", "SetFiletypeStatement"),
        ("set_item", "SetItem"),
        ("set_value", "SetValue"),
        ("option_name", "OptionName"),
        ("inv_option", "InvertOption"),
        ("no_option", "NoOption"),
        ("default_option", "DefaultOption"),

        // File operations
        ("edit_statement", "EditStatement"),
        ("enew_statement", "EnewStatement"),
        ("view_statement", "ViewStatement"),
        ("find_statement", "FindStatement"),

        // Window/Tab management
        ("wincmd_statement", "WincmdStatement"),
        ("tab_statement", "TabStatement"),
        ("aboveleft_statement", "AboveleftStatement"),
        ("belowright_statement", "BelowrightStatement"),
        ("botright_statement", "BotrightStatement"),
        ("topleft_statement", "TopleftStatement"),
        ("vertical_statement", "VerticalStatement"),

        // Silent/Filter
        ("silent_statement", "SilentStatement"),
        ("bang_filter_statement", "BangFilterStatement"),
        ("filter_command", "FilterCommand"),

        // Ex commands and ranges
        ("ex_statement", "ExStatement"),
        ("range_statement", "RangeStatement"),
        ("range", "Range"),
        ("current_line", "CurrentLine"),
        ("last_line", "LastLine"),
        ("next_line", "NextLine"),
        ("mark", "Mark"),
        ("pattern", "Pattern"),
        ("pattern_multi", "PatternMulti"),
        ("pattern_offset", "PatternOffset"),
        ("previous_pattern", "PreviousPattern"),

        // Substitution
        ("substitute_statement", "SubstituteStatement"),
        ("global_statement", "GlobalStatement"),

        // Quickfix/Location
        ("cnext_statement", "CnextStatement"),
        ("cprevious_statement", "CpreviousStatement"),

        // Filetype and encoding
        ("filetype_statement", "FiletypeStatement"),
        ("filetype", "Filetype"),
        ("scriptencoding_statement", "ScriptEncodingStatement"),
        ("encoding", "Encoding"),

        // Insert mode
        ("startinsert_statement", "StartInsertStatement"),
        ("stopinsert_statement", "StopInsertStatement"),
        ("visual_statement", "VisualStatement"),

        // Colorscheme and options
        ("colorscheme_statement", "ColorschemeStatement"),
        ("color", "Color"),
        ("options_statement", "OptionsStatement"),

        // Sign
        ("sign_statement", "SignStatement"),
        ("sign_argument", "SignArgument"),

        // User commands
        ("comclear_statement", "ComclearStatement"),
        ("delcommand_statement", "DelcommandStatement"),

        // Embedded languages
        ("lua_statement", "LuaStatement"),
        ("python_statement", "PythonStatement"),
        ("ruby_statement", "RubyStatement"),
        ("perl_statement", "PerlStatement"),

        // Miscellaneous
        ("body", "Block"),
        ("comment", "Comment"),
        ("shebang", "Shebang"),
        ("keycode", "Keycode"),
        ("register_statement", "RegisterStatement"),
        ("marker_definition", "MarkerDefinition"),
        ("match_case", "MatchCase"),
        ("unknown_builtin_statement", "UnknownBuiltinStatement"),
        ("unknown_command_name", "UnknownCommandName"),
        ("file", "File"),
        ("file_format", "FileFormat"),
        ("wildcard", "Wildcard"),
        ("plus_cmd", "PlusCommand"),
        ("plus_plus_opt", "PlusPlusOption"),
        ("where", "WhereClause"),
        ("name", "Name"),
        ("value", "Value"),
        ("text", "Text"),
        ("chunk", "Chunk"),
        ("behavior", "Behavior"),
        ("fallback", "Fallback"),
        ("font", "Font"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Gleam-specific mappings (functional language for the BEAM VM)
/// Gleam is a type-safe functional language that compiles to Erlang and JavaScript.
static GLEAM_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("source_file", "SourceFile"), ("module", "Module"),
        ("target_group", "TargetGroup"), ("target", "Target"),

        // Declarations
        ("function", "FunctionDeclaration"), ("external_function", "ExternalFunctionDeclaration"),
        ("external_function_body", "ExternalFunctionBody"),
        ("type_definition", "TypeDeclaration"), ("type_alias", "TypeAlias"),
        ("external_type", "ExternalType"),
        ("constant", "ConstantDeclaration"),

        // Data constructors
        ("data_constructor", "DataConstructor"), ("data_constructors", "DataConstructors"),
        ("data_constructor_argument", "DataConstructorArgument"),
        ("data_constructor_arguments", "DataConstructorArguments"),
        ("constructor_name", "ConstructorName"),

        // Functions
        ("function_parameter", "ParameterDeclaration"), ("function_parameters", "Parameters"),
        ("function_parameter_types", "ParameterTypes"),
        ("function_type", "FunctionType"),
        ("anonymous_function", "LambdaExpression"),

        // Control flow
        ("case", "MatchExpression"), ("case_clause", "MatchArm"),
        ("case_clauses", "MatchArms"), ("case_subjects", "MatchSubjects"),
        ("case_clause_pattern", "CasePattern"), ("case_clause_patterns", "CasePatterns"),
        ("case_clause_guard", "Guard"),
        ("assert", "AssertStatement"), ("let_assert", "LetAssert"),

        // Expressions
        ("function_call", "CallExpression"), ("arguments", "Arguments"),
        ("argument", "Argument"),
        ("binary_expression", "BinaryExpression"),
        ("boolean_negation", "UnaryExpression"), ("integer_negation", "UnaryExpression"),
        ("field_access", "MemberExpression"),
        ("pipeline_echo", "PipelineExpression"),
        ("block", "Block"),

        // Pattern matching
        ("tuple_pattern", "TuplePattern"), ("list_pattern", "ListPattern"),
        ("list_pattern_tail", "ListPatternTail"),
        ("record_pattern", "RecordPattern"),
        ("record_pattern_argument", "RecordPatternArgument"),
        ("record_pattern_arguments", "RecordPatternArguments"),
        ("discard", "DiscardPattern"), ("pattern_spread", "SpreadPattern"),

        // Data structures
        ("tuple", "TupleExpression"), ("list", "ListExpression"),
        ("record", "RecordExpression"),
        ("record_update", "RecordUpdate"),
        ("record_update_argument", "RecordUpdateArgument"),
        ("record_update_arguments", "RecordUpdateArguments"),
        ("bit_array", "BitArray"), ("bit_array_pattern", "BitArrayPattern"),
        ("bit_array_segment", "BitArraySegment"),
        ("bit_array_segment_option", "BitArraySegmentOption"),
        ("bit_array_segment_options", "BitArraySegmentOptions"),

        // Literals
        ("string", "StringLiteral"), ("float", "NumberLiteral"),
        ("integer", "NumberLiteral"), ("escape_sequence", "EscapeSequence"),

        // Statements
        ("let", "VariableDeclaration"), ("use", "UseStatement"),
        ("use_assignment", "UseAssignment"), ("use_assignments", "UseAssignments"),
        ("echo", "EchoStatement"), ("panic", "PanicStatement"),
        ("todo", "TodoStatement"),

        // Imports
        ("import", "ImportDeclaration"),
        ("unqualified_import", "UnqualifiedImport"),
        ("unqualified_imports", "UnqualifiedImports"),

        // Types
        ("type", "TypeExpression"), ("type_identifier", "TypeIdentifier"),
        ("type_name", "TypeName"), ("type_var", "TypeVariable"),
        ("type_hole", "TypeHole"), ("type_argument", "TypeArgument"),
        ("type_arguments", "TypeArguments"), ("type_parameter", "TypeParameter"),
        ("type_parameters", "TypeParameters"),
        ("tuple_type", "TupleType"),
        ("remote_type_identifier", "QualifiedTypeIdentifier"),
        ("remote_constructor_name", "QualifiedConstructorName"),

        // Identifiers and misc
        ("identifier", "Identifier"), ("label", "Label"),
        ("hole", "Hole"), ("attribute", "Attribute"),
        ("attribute_value", "AttributeValue"),
        ("visibility_modifier", "VisibilityModifier"),
        ("opacity_modifier", "OpacityModifier"),

        // Comments
        ("comment", "Comment"), ("module_comment", "ModuleComment"),
        ("statement_comment", "StatementComment"),
        ("doc_comment_content", "DocCommentContent"),
        ("quoted_content", "QuotedContent"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Svelte-specific mappings (reactive frontend framework)
/// Svelte is a component framework that compiles to vanilla JavaScript.
static SVELTE_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),

        // Elements
        ("element", "Element"), ("script_element", "ScriptElement"),
        ("style_element", "StyleElement"),

        // Tags
        ("start_tag", "StartTag"), ("end_tag", "EndTag"),
        ("self_closing_tag", "SelfClosingTag"),
        ("tag_name", "TagName"),
        ("erroneous_end_tag", "ErroneousEndTag"),
        ("erroneous_end_tag_name", "ErroneousEndTagName"),

        // Attributes
        ("attribute", "Attribute"), ("attribute_name", "AttributeName"),
        ("attribute_value", "AttributeValue"),
        ("quoted_attribute_value", "QuotedAttributeValue"),
        ("expr_attribute_value", "ExpressionAttributeValue"),

        // Control flow blocks
        ("if_statement", "IfStatement"),
        ("if_start_expr", "IfStart"), ("if_end_expr", "IfEnd"),
        ("else_statement", "ElseStatement"),
        ("else_if_statement", "ElseIfStatement"),
        ("else_if_expr", "ElseIfExpression"),
        ("else_expr", "ElseExpression"),

        // Each blocks (iteration)
        ("each_statement", "EachStatement"),
        ("each_start_expr", "EachStart"), ("each_end_expr", "EachEnd"),
        ("else_each_statement", "ElseEachStatement"),

        // Await blocks (async)
        ("await_statement", "AwaitStatement"),
        ("await_start_expr", "AwaitStart"), ("await_end_expr", "AwaitEnd"),
        ("then_statement", "ThenStatement"), ("then_expr", "ThenExpression"),
        ("catch_statement", "CatchStatement"), ("catch_expr", "CatchExpression"),

        // Key blocks
        ("key_statement", "KeyStatement"),
        ("key_start_expr", "KeyStart"), ("key_end_expr", "KeyEnd"),

        // Snippet blocks (Svelte 5)
        ("snippet_statement", "SnippetStatement"),
        ("snippet_start_expr", "SnippetStart"), ("snippet_end_expr", "SnippetEnd"),
        ("snippet_name", "SnippetName"),

        // Expressions
        ("expression", "Expression"),
        ("html_expr", "HtmlExpression"),
        ("const_expr", "ConstExpression"),
        ("debug_expr", "DebugExpression"),
        ("render_expr", "RenderExpression"),

        // Raw content
        ("text", "Text"), ("raw_text", "RawText"),
        ("raw_text_expr", "RawTextExpression"),
        ("raw_text_each", "RawTextEach"),
        ("raw_text_await", "RawTextAwait"),

        // Comments and misc
        ("comment", "Comment"),
        ("as", "AsKeyword"), ("then", "ThenKeyword"),
        ("special_block_keyword", "SpecialBlockKeyword"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// WGSL-specific mappings (WebGPU Shading Language)
/// WGSL is the shader language for WebGPU, used for GPU programming.
static WGSL_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("source_file", "SourceFile"),
        ("enable_directive", "EnableDirective"),

        // Declarations
        ("function_declaration", "FunctionDeclaration"),
        ("function_return_type_declaration", "ReturnTypeDeclaration"),
        ("struct_declaration", "StructDeclaration"),
        ("struct_member", "FieldDeclaration"),
        ("type_alias_declaration", "TypeAlias"),
        ("global_variable_declaration", "GlobalVariableDeclaration"),
        ("global_constant_declaration", "GlobalConstantDeclaration"),
        ("variable_declaration", "VariableDeclaration"),
        ("variable_identifier_declaration", "VariableIdentifierDeclaration"),
        ("variable_qualifier", "VariableQualifier"),

        // Parameters
        ("parameter", "ParameterDeclaration"),
        ("parameter_list", "Parameters"),

        // Statements
        ("compound_statement", "Block"),
        ("variable_statement", "VariableStatement"),
        ("assignment_statement", "AssignmentStatement"),
        ("increment_statement", "IncrementStatement"),
        ("decrement_statement", "DecrementStatement"),
        ("return_statement", "ReturnStatement"),
        ("discard_statement", "DiscardStatement"),

        // Control flow
        ("if_statement", "IfStatement"),
        ("else_statement", "ElseClause"),
        ("switch_statement", "SwitchStatement"),
        ("switch_body", "SwitchBody"),
        ("case_selectors", "CaseSelectors"),
        ("case_compound_statement", "CaseBlock"),
        ("for_statement", "ForStatement"),
        ("for_header", "ForHeader"),
        ("while_statement", "WhileStatement"),
        ("loop_statement", "LoopStatement"),
        ("continuing_statement", "ContinuingStatement"),
        ("continuing_compound_statement", "ContinuingBlock"),
        ("break_statement", "BreakStatement"),
        ("break_if_statement", "BreakIfStatement"),
        ("continue_statement", "ContinueStatement"),
        ("fallthrough_statement", "FallthroughStatement"),

        // Expressions
        ("binary_expression", "BinaryExpression"),
        ("unary_expression", "UnaryExpression"),
        ("parenthesized_expression", "ParenthesizedExpression"),
        ("postfix_expression", "PostfixExpression"),
        ("subscript_expression", "IndexExpression"),
        ("argument_list_expression", "CallExpression"),
        ("type_constructor_or_function_call_expression", "CallExpression"),
        ("bitcast_expression", "BitcastExpression"),
        ("lhs_expression", "LhsExpression"),
        ("const_expression", "ConstExpression"),
        ("composite_value_decomposition_expression", "DecompositionExpression"),

        // Types
        ("type_declaration", "TypeReference"),
        ("address_space", "AddressSpace"),
        ("access_mode", "AccessMode"),
        ("texel_format", "TexelFormat"),

        // Operators
        ("compound_assignment_operator", "CompoundAssignmentOperator"),

        // Literals
        ("int_literal", "NumberLiteral"),
        ("float_literal", "NumberLiteral"),
        ("bool_literal", "BooleanLiteral"),
        ("const_literal", "ConstLiteral"),

        // Attributes and identifiers
        ("attribute", "Attribute"),
        ("identifier", "Identifier"),

        // Comments
        ("line_comment", "LineComment"),
        ("block_comment", "BlockComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Astro-specific mappings (web framework for content-driven sites)
/// Astro is a modern static site generator with component islands.
static ASTRO_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Document structure
        ("document", "Document"),
        ("frontmatter", "Frontmatter"),
        ("frontmatter_js_block", "FrontmatterJsBlock"),
        ("doctype", "Doctype"),

        // Elements
        ("element", "Element"),
        ("script_element", "ScriptElement"),
        ("style_element", "StyleElement"),

        // Tags
        ("start_tag", "StartTag"),
        ("end_tag", "EndTag"),
        ("self_closing_tag", "SelfClosingTag"),
        ("tag_name", "TagName"),
        ("erroneous_end_tag", "ErroneousEndTag"),
        ("erroneous_end_tag_name", "ErroneousEndTagName"),

        // Attributes
        ("attribute", "Attribute"),
        ("attribute_name", "AttributeName"),
        ("attribute_value", "AttributeValue"),
        ("quoted_attribute_value", "QuotedAttributeValue"),
        ("attribute_backtick_string", "AttributeBacktickString"),
        ("attribute_interpolation", "AttributeInterpolation"),
        ("attribute_js_expr", "AttributeJsExpression"),

        // Interpolation
        ("html_interpolation", "HtmlInterpolation"),

        // Content
        ("text", "Text"),
        ("permissible_text", "PermissibleText"),
        ("raw_text", "RawText"),

        // Comments
        ("comment", "Comment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Prisma-specific mappings (database schema language)
/// Prisma is a modern ORM with a schema-first approach.
static PRISMA_MAPPINGS: Lazy<NodeKindMappings> = Lazy::new(|| {
    let mut pairs: Vec<(&'static str, &'static str)> = Vec::new();
    for (k, v) in &GENERIC_MAPPINGS.mappings { pairs.push((*k, *v)); }
    pairs.extend_from_slice(&[
        // Program structure
        ("program", "SourceFile"),

        // Declarations
        ("model_declaration", "ModelDeclaration"),
        ("enum_declaration", "EnumDeclaration"),
        ("type_declaration", "TypeDeclaration"),
        ("view_declaration", "ViewDeclaration"),
        ("datasource_declaration", "DatasourceDeclaration"),
        ("generator_declaration", "GeneratorDeclaration"),

        // Model/Type contents
        ("column_declaration", "FieldDeclaration"),
        ("column_type", "FieldType"),
        ("enum_block", "EnumBlock"),
        ("enumeral", "EnumMember"),
        ("statement_block", "StatementBlock"),
        ("block_attribute_declaration", "BlockAttribute"),

        // Expressions
        ("call_expression", "CallExpression"),
        ("binary_expression", "BinaryExpression"),
        ("member_expression", "MemberExpression"),
        ("assignment_expression", "AssignmentExpression"),
        ("type_expression", "TypeExpression"),
        ("arguments", "Arguments"),
        ("array", "ArrayExpression"),

        // Attributes
        ("attribute", "Attribute"),

        // Literals
        ("string", "StringLiteral"),
        ("number", "NumberLiteral"),
        ("true", "BooleanLiteral"),
        ("false", "BooleanLiteral"),
        ("null", "NullLiteral"),

        // Identifiers
        ("identifier", "Identifier"),
        ("property_identifier", "PropertyIdentifier"),
        ("variable", "Variable"),

        // Type helpers
        ("maybe", "OptionalType"),
        ("type_declaration_type", "TypeDeclarationType"),

        // Keywords
        ("model", "ModelKeyword"),
        ("enum", "EnumKeyword"),
        ("type", "TypeKeyword"),
        ("view", "ViewKeyword"),
        ("datasource", "DatasourceKeyword"),
        ("generator", "GeneratorKeyword"),

        // Comments
        ("comment", "Comment"),
        ("developer_comment", "DeveloperComment"),
    ]);
    NodeKindMappings::new(&pairs)
});

/// Get the appropriate mappings for a language.
///
/// Returns language-specific mappings if available, otherwise returns generic mappings.
pub fn get_mappings(language: &str) -> &'static NodeKindMappings {
    match language.to_lowercase().as_str() {
        // Primary languages with full mappings
        "javascript" | "js" => &JAVASCRIPT_MAPPINGS,
        "typescript" | "ts" | "tsx" | "jsx" => &TYPESCRIPT_MAPPINGS,
        "python" | "py" => &PYTHON_MAPPINGS,
        "go" | "golang" => &GO_MAPPINGS,
        "rust" | "rs" => &RUST_MAPPINGS,
        "c" | "cpp" | "c++" | "cxx" | "cc" | "h" | "hpp" => &C_CPP_MAPPINGS,
        "java" => &JAVA_MAPPINGS,
        "groovy" | "gvy" | "gy" | "gsh" => &GROOVY_MAPPINGS,
        "ruby" | "rb" => &RUBY_MAPPINGS,
        "powershell" | "ps1" | "psm1" | "psd1" | "pwsh" => &POWERSHELL_MAPPINGS,
        "c-sharp" | "csharp" | "cs" | "c_sharp" => &CSHARP_MAPPINGS,

        // Scripting languages
        "bash" | "sh" | "shell" | "zsh" => &BASH_MAPPINGS,
        "lua" => &LUA_MAPPINGS,
        "php" => &PHP_MAPPINGS,
        "perl" | "pl" | "pm" => &PERL_MAPPINGS,
        "r" | "rscript" => &R_MAPPINGS,

        // Functional languages
        "elixir" | "ex" | "exs" => &ELIXIR_MAPPINGS,
        "erlang" | "erl" | "hrl" => &ERLANG_MAPPINGS,
        "elm" => &ELM_MAPPINGS,
        "clojure" | "clj" | "cljs" | "cljc" | "edn" => &CLOJURE_MAPPINGS,
        "nix" => &NIX_MAPPINGS,
        "haskell" | "hs" => &HASKELL_MAPPINGS,
        "ocaml" | "ml" | "mli" => &OCAML_MAPPINGS,
        "fsharp" | "f#" | "fs" | "fsi" | "fsx" => &FSHARP_MAPPINGS,
        "julia" | "jl" => &JULIA_MAPPINGS,

        // DSL and schema languages
        "hcl" | "tf" | "tfvars" => &HCL_MAPPINGS,
        "graphql" | "gql" => &GRAPHQL_MAPPINGS,
        "proto" | "protobuf" => &PROTOBUF_MAPPINGS,
        "sql" => &SQL_MAPPINGS,

        // Infrastructure languages
        "bicep" => &BICEP_MAPPINGS,
        "dockerfile" | "docker" => &DOCKERFILE_MAPPINGS,

        // Build system languages
        "cmake" => &CMAKE_MAPPINGS,
        "make" | "makefile" => &MAKE_MAPPINGS,

        // Frontend framework languages
        "vue" => &VUE_MAPPINGS,
        "angular" => &ANGULAR_MAPPINGS,

        // Markup and styling languages
        "html" | "htm" => &HTML_MAPPINGS,
        "xml" | "xsl" | "xslt" => &XML_MAPPINGS,
        "css" | "scss" | "less" => &CSS_MAPPINGS,
        "markdown" | "md" => &MARKDOWN_MAPPINGS,

        // Data format languages
        "json" => &JSON_MAPPINGS,
        "toml" => &TOML_MAPPINGS,
        "yaml" | "yml" => &YAML_MAPPINGS,

        // WASM-based languages (high priority)
        "kotlin" | "kt" | "kts" => &KOTLIN_MAPPINGS,
        "swift" => &SWIFT_MAPPINGS,
        "scala" | "sc" => &SCALA_MAPPINGS,
        "dart" => &DART_MAPPINGS,

        // WASM-based languages (newly mapped)
        "arduino" | "ino" | "pde" => &ARDUINO_MAPPINGS,
        "apex" | "cls" | "trigger" => &APEX_MAPPINGS,
        "cairo" => &CAIRO_MAPPINGS,
        "cue" => &CUE_MAPPINGS,
        "dhall" => &DHALL_MAPPINGS,
        "bitbake" | "bb" | "bbappend" | "bbclass" | "conf" => &BITBAKE_MAPPINGS,

        // WASM-based specialized languages
        "commonlisp" | "lisp" | "cl" | "lsp" => &COMMONLISP_MAPPINGS,
        "awk" | "gawk" | "mawk" | "nawk" => &AWK_MAPPINGS,
        "cuda" | "cu" | "cuh" => &CUDA_MAPPINGS,
        "agda" => &AGDA_MAPPINGS,

        // Systems WASM languages
        "objc" | "objective-c" | "objectivec" | "m" | "mm" => &OBJC_MAPPINGS,
        "d" | "dlang" => &D_MAPPINGS,
        "crystal" | "cr" => &CRYSTAL_MAPPINGS,
        "zig" => &ZIG_MAPPINGS,

        // Specialized WASM/Native languages
        "ada" | "adb" | "ads" => &ADA_MAPPINGS,
        "fortran" | "f" | "for" | "f90" | "f95" | "f03" | "f08" | "f18" => &FORTRAN_MAPPINGS,
        "cobol" | "cob" | "cbl" | "cpy" => &COBOL_MAPPINGS,
        "verilog" | "v" | "vh" | "sv" | "svh" => &VERILOG_MAPPINGS,

        // Document typesetting languages (WASM)
        "latex" | "tex" => &LATEX_MAPPINGS,

        // Simple/specialized languages
        "csv" => &CSV_MAPPINGS,
        "comment" => &COMMENT_MAPPINGS,
        "doxygen" => &DOXYGEN_MAPPINGS,
        "vim" | "viml" | "vimscript" => &VIM_MAPPINGS,

        // New popular tree-sitter languages
        "gleam" => &GLEAM_MAPPINGS,
        "svelte" => &SVELTE_MAPPINGS,
        "wgsl" => &WGSL_MAPPINGS,
        "astro" => &ASTRO_MAPPINGS,
        "prisma" => &PRISMA_MAPPINGS,

        // Modern shell and scripting languages
        "nushell" | "nu" => &NUSHELL_MAPPINGS,

        // Build system languages
        "starlark" | "bzl" | "bazel" | "star" => &STARLARK_MAPPINGS,

        // Blockchain/Smart contract languages
        "solidity" | "sol" => &SOLIDITY_MAPPINGS,

        // Compiler/Low-level IRs
        "llvm" | "llvm-ir" | "ll" => &LLVM_IR_MAPPINGS,

        // Modern systems programming languages
        "odin" => &ODIN_MAPPINGS,

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
        // Use a truly unmappable node name that doesn't match any heuristic pattern
        assert_eq!(mappings.get("xyz_completely_unknown_xyz"), "Unknown");
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

    #[test]
    fn test_html_mappings() {
        let mappings = get_mappings("html");
        // HTML-specific mappings
        assert_eq!(mappings.get("element"), "Element");
        assert_eq!(mappings.get("start_tag"), "StartTag");
        assert_eq!(mappings.get("end_tag"), "EndTag");
        assert_eq!(mappings.get("self_closing_tag"), "SelfClosingTag");
        assert_eq!(mappings.get("tag_name"), "TagName");
        assert_eq!(mappings.get("attribute"), "Attribute");
        assert_eq!(mappings.get("attribute_name"), "AttributeName");
        assert_eq!(mappings.get("doctype"), "Doctype");
        assert_eq!(mappings.get("script_element"), "ScriptElement");
        assert_eq!(mappings.get("style_element"), "StyleElement");
        assert_eq!(mappings.get("text"), "Text");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_html_aliases() {
        // Test all HTML language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("html"), get_mappings("htm")));
    }

    #[test]
    fn test_reverse_lookup_html() {
        let native_types = get_native_types_for_uast("Element", "html");
        assert!(native_types.contains(&"element"));

        let native_types = get_native_types_for_uast("StartTag", "html");
        assert!(native_types.contains(&"start_tag"));

        let native_types = get_native_types_for_uast("TagName", "html");
        assert!(native_types.contains(&"tag_name"));

        let native_types = get_native_types_for_uast("Attribute", "html");
        assert!(native_types.contains(&"attribute"));

        let native_types = get_native_types_for_uast("ScriptElement", "html");
        assert!(native_types.contains(&"script_element"));
    }

    #[test]
    fn test_xml_mappings() {
        let mappings = get_mappings("xml");
        // XML-specific mappings (note: XML grammar uses different naming conventions)
        assert_eq!(mappings.get("element"), "Element");
        assert_eq!(mappings.get("STag"), "StartTag");
        assert_eq!(mappings.get("ETag"), "EndTag");
        assert_eq!(mappings.get("EmptyElemTag"), "SelfClosingElement");
        assert_eq!(mappings.get("Attribute"), "Attribute");
        assert_eq!(mappings.get("XMLDecl"), "XmlDeclaration");
        assert_eq!(mappings.get("doctypedecl"), "Doctype");
        assert_eq!(mappings.get("CData"), "CDataSection");
        assert_eq!(mappings.get("document"), "Document");
        assert_eq!(mappings.get("prolog"), "Prolog");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_xml_aliases() {
        // Test all XML language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("xml"), get_mappings("xsl")));
        assert!(std::ptr::eq(get_mappings("xml"), get_mappings("xslt")));
    }

    #[test]
    fn test_reverse_lookup_xml() {
        let native_types = get_native_types_for_uast("Element", "xml");
        assert!(native_types.contains(&"element"));

        let native_types = get_native_types_for_uast("StartTag", "xml");
        assert!(native_types.contains(&"STag"));

        let native_types = get_native_types_for_uast("Doctype", "xml");
        assert!(native_types.contains(&"doctypedecl"));

        let native_types = get_native_types_for_uast("CDataSection", "xml");
        assert!(native_types.contains(&"CData"));
    }

    #[test]
    fn test_css_mappings() {
        let mappings = get_mappings("css");
        // CSS-specific mappings
        assert_eq!(mappings.get("rule_set"), "RuleSet");
        assert_eq!(mappings.get("class_selector"), "ClassSelector");
        assert_eq!(mappings.get("id_selector"), "IdSelector");
        assert_eq!(mappings.get("declaration"), "Declaration");
        assert_eq!(mappings.get("property_name"), "PropertyName");
        assert_eq!(mappings.get("media_statement"), "MediaQuery");
        assert_eq!(mappings.get("keyframes_statement"), "KeyframesRule");
        assert_eq!(mappings.get("stylesheet"), "Stylesheet");
        assert_eq!(mappings.get("call_expression"), "CallExpression");
        assert_eq!(mappings.get("pseudo_class_selector"), "PseudoClassSelector");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
        assert_eq!(mappings.get("identifier"), "Identifier");
    }

    #[test]
    fn test_css_aliases() {
        // Test all CSS language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("css"), get_mappings("scss")));
        assert!(std::ptr::eq(get_mappings("css"), get_mappings("less")));
    }

    #[test]
    fn test_reverse_lookup_css() {
        let native_types = get_native_types_for_uast("RuleSet", "css");
        assert!(native_types.contains(&"rule_set"));

        let native_types = get_native_types_for_uast("ClassSelector", "css");
        assert!(native_types.contains(&"class_selector"));

        let native_types = get_native_types_for_uast("Declaration", "css");
        assert!(native_types.contains(&"declaration"));

        let native_types = get_native_types_for_uast("MediaQuery", "css");
        assert!(native_types.contains(&"media_statement"));

        let native_types = get_native_types_for_uast("KeyframesRule", "css");
        assert!(native_types.contains(&"keyframes_statement"));
    }

    #[test]
    fn test_nix_mappings() {
        let mappings = get_mappings("nix");
        // Nix-specific mappings
        assert_eq!(mappings.get("function_expression"), "LambdaExpression");
        assert_eq!(mappings.get("apply_expression"), "CallExpression");
        assert_eq!(mappings.get("let_expression"), "LetExpression");
        assert_eq!(mappings.get("if_expression"), "IfExpression");
        assert_eq!(mappings.get("with_expression"), "WithExpression");
        assert_eq!(mappings.get("attrset_expression"), "ObjectExpression");
        assert_eq!(mappings.get("list_expression"), "ArrayExpression");
        assert_eq!(mappings.get("binding"), "PropertyDeclaration");
        assert_eq!(mappings.get("string_expression"), "StringLiteral");
        assert_eq!(mappings.get("integer_expression"), "NumberLiteral");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
        assert_eq!(mappings.get("identifier"), "Identifier");
    }

    #[test]
    fn test_reverse_lookup_nix() {
        let native_types = get_native_types_for_uast("LambdaExpression", "nix");
        assert!(native_types.contains(&"function_expression"));

        let native_types = get_native_types_for_uast("CallExpression", "nix");
        assert!(native_types.contains(&"apply_expression"));

        let native_types = get_native_types_for_uast("LetExpression", "nix");
        assert!(native_types.contains(&"let_expression"));

        let native_types = get_native_types_for_uast("ObjectExpression", "nix");
        assert!(native_types.contains(&"attrset_expression"));

        let native_types = get_native_types_for_uast("ArrayExpression", "nix");
        assert!(native_types.contains(&"list_expression"));
    }

    #[test]
    fn test_bicep_mappings() {
        let mappings = get_mappings("bicep");
        // Bicep-specific mappings
        assert_eq!(mappings.get("resource_declaration"), "ResourceDeclaration");
        assert_eq!(mappings.get("module_declaration"), "ModuleDeclaration");
        assert_eq!(mappings.get("parameter_declaration"), "ParameterDeclaration");
        assert_eq!(mappings.get("output_declaration"), "OutputDeclaration");
        assert_eq!(mappings.get("for_statement"), "ForStatement");
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("decorator"), "Decorator");
        assert_eq!(mappings.get("object"), "ObjectExpression");
        assert_eq!(mappings.get("array"), "ArrayExpression");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
        assert_eq!(mappings.get("identifier"), "Identifier");
    }

    #[test]
    fn test_reverse_lookup_bicep() {
        let native_types = get_native_types_for_uast("ResourceDeclaration", "bicep");
        assert!(native_types.contains(&"resource_declaration"));

        let native_types = get_native_types_for_uast("ModuleDeclaration", "bicep");
        assert!(native_types.contains(&"module_declaration"));

        let native_types = get_native_types_for_uast("ParameterDeclaration", "bicep");
        assert!(native_types.contains(&"parameter_declaration"));

        let native_types = get_native_types_for_uast("OutputDeclaration", "bicep");
        assert!(native_types.contains(&"output_declaration"));

        let native_types = get_native_types_for_uast("ForStatement", "bicep");
        assert!(native_types.contains(&"for_statement"));
    }

    #[test]
    fn test_dockerfile_mappings() {
        let mappings = get_mappings("dockerfile");
        // Dockerfile-specific mappings
        assert_eq!(mappings.get("from_instruction"), "FromInstruction");
        assert_eq!(mappings.get("run_instruction"), "RunInstruction");
        assert_eq!(mappings.get("copy_instruction"), "CopyInstruction");
        assert_eq!(mappings.get("env_instruction"), "EnvInstruction");
        assert_eq!(mappings.get("expose_instruction"), "ExposeInstruction");
        assert_eq!(mappings.get("cmd_instruction"), "CmdInstruction");
        assert_eq!(mappings.get("entrypoint_instruction"), "EntrypointInstruction");
        assert_eq!(mappings.get("workdir_instruction"), "WorkdirInstruction");
        assert_eq!(mappings.get("image_spec"), "ImageSpec");
        assert_eq!(mappings.get("double_quoted_string"), "StringLiteral");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_dockerfile_aliases() {
        // Test Dockerfile language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("dockerfile"), get_mappings("docker")));
    }

    #[test]
    fn test_reverse_lookup_dockerfile() {
        let native_types = get_native_types_for_uast("FromInstruction", "dockerfile");
        assert!(native_types.contains(&"from_instruction"));

        let native_types = get_native_types_for_uast("RunInstruction", "dockerfile");
        assert!(native_types.contains(&"run_instruction"));

        let native_types = get_native_types_for_uast("CopyInstruction", "dockerfile");
        assert!(native_types.contains(&"copy_instruction"));

        let native_types = get_native_types_for_uast("EnvInstruction", "dockerfile");
        assert!(native_types.contains(&"env_instruction"));

        let native_types = get_native_types_for_uast("ExposeInstruction", "dockerfile");
        assert!(native_types.contains(&"expose_instruction"));
    }

    #[test]
    fn test_commonlisp_mappings() {
        let mappings = get_mappings("commonlisp");
        // CommonLisp-specific mappings
        assert_eq!(mappings.get("defun"), "FunctionDeclaration");
        assert_eq!(mappings.get("defmacro"), "MacroDeclaration");
        assert_eq!(mappings.get("defclass"), "TypeDeclaration");
        assert_eq!(mappings.get("let"), "LetExpression");
        assert_eq!(mappings.get("cond"), "CondExpression");
        assert_eq!(mappings.get("lambda"), "LambdaExpression");
        assert_eq!(mappings.get("loop"), "LoopExpression");
        assert_eq!(mappings.get("handler-case"), "TryExpression");
        assert_eq!(mappings.get("defpackage"), "PackageDeclaration");
        assert_eq!(mappings.get("symbol"), "SymbolLiteral");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_commonlisp_aliases() {
        // Test all CommonLisp language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("commonlisp"), get_mappings("lisp")));
        assert!(std::ptr::eq(get_mappings("commonlisp"), get_mappings("cl")));
        assert!(std::ptr::eq(get_mappings("commonlisp"), get_mappings("lsp")));
    }

    #[test]
    fn test_reverse_lookup_commonlisp() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "commonlisp");
        assert!(native_types.contains(&"defun"));

        let native_types = get_native_types_for_uast("TypeDeclaration", "commonlisp");
        assert!(native_types.contains(&"defclass"));

        let native_types = get_native_types_for_uast("LetExpression", "commonlisp");
        assert!(native_types.contains(&"let"));

        let native_types = get_native_types_for_uast("LambdaExpression", "commonlisp");
        assert!(native_types.contains(&"lambda"));
    }

    #[test]
    fn test_awk_mappings() {
        let mappings = get_mappings("awk");
        // AWK-specific mappings
        assert_eq!(mappings.get("rule"), "Rule");
        assert_eq!(mappings.get("pattern"), "Pattern");
        assert_eq!(mappings.get("action"), "Block");
        assert_eq!(mappings.get("function_definition"), "FunctionDeclaration");
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("for_statement"), "ForStatement");
        assert_eq!(mappings.get("while_statement"), "WhileStatement");
        assert_eq!(mappings.get("print_statement"), "PrintStatement");
        assert_eq!(mappings.get("regex"), "RegexLiteral");
        assert_eq!(mappings.get("field_ref"), "FieldReference");
        assert_eq!(mappings.get("BEGIN"), "BeginPattern");
        assert_eq!(mappings.get("END"), "EndPattern");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_awk_aliases() {
        // Test all AWK language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("awk"), get_mappings("gawk")));
        assert!(std::ptr::eq(get_mappings("awk"), get_mappings("mawk")));
        assert!(std::ptr::eq(get_mappings("awk"), get_mappings("nawk")));
    }

    #[test]
    fn test_reverse_lookup_awk() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "awk");
        assert!(native_types.contains(&"function_definition"));

        let native_types = get_native_types_for_uast("Rule", "awk");
        assert!(native_types.contains(&"rule"));

        let native_types = get_native_types_for_uast("Block", "awk");
        assert!(native_types.contains(&"action"));

        let native_types = get_native_types_for_uast("PrintStatement", "awk");
        assert!(native_types.contains(&"print_statement"));
    }

    #[test]
    fn test_cuda_mappings() {
        let mappings = get_mappings("cuda");
        // CUDA-specific mappings
        assert_eq!(mappings.get("kernel_declaration"), "KernelDeclaration");
        assert_eq!(mappings.get("kernel_call"), "KernelCallExpression");
        assert_eq!(mappings.get("__global__"), "GlobalAttribute");
        assert_eq!(mappings.get("__device__"), "DeviceAttribute");
        assert_eq!(mappings.get("__shared__"), "SharedAttribute");
        assert_eq!(mappings.get("blockIdx"), "BlockIndex");
        assert_eq!(mappings.get("threadIdx"), "ThreadIndex");
        assert_eq!(mappings.get("__syncthreads"), "SyncThreadsCall");
        assert_eq!(mappings.get("cudaMalloc"), "CudaMallocCall");
        assert_eq!(mappings.get("atomicAdd"), "AtomicAddCall");
        assert_eq!(mappings.get("dim3"), "Dim3Type");
        // Inherited from C/C++
        assert_eq!(mappings.get("function_definition"), "FunctionDeclaration");
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_cuda_aliases() {
        // Test all CUDA language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("cuda"), get_mappings("cu")));
        assert!(std::ptr::eq(get_mappings("cuda"), get_mappings("cuh")));
    }

    #[test]
    fn test_reverse_lookup_cuda() {
        let native_types = get_native_types_for_uast("KernelDeclaration", "cuda");
        assert!(native_types.contains(&"kernel_declaration"));

        let native_types = get_native_types_for_uast("GlobalAttribute", "cuda");
        assert!(native_types.contains(&"__global__"));

        let native_types = get_native_types_for_uast("BlockIndex", "cuda");
        assert!(native_types.contains(&"blockIdx"));

        // Also inherits from C/C++
        let native_types = get_native_types_for_uast("FunctionDeclaration", "cuda");
        assert!(native_types.contains(&"function_definition"));
    }

    #[test]
    fn test_agda_mappings() {
        let mappings = get_mappings("agda");
        // Agda-specific mappings
        assert_eq!(mappings.get("function_clause"), "FunctionDeclaration");
        assert_eq!(mappings.get("data_declaration"), "TypeDeclaration");
        assert_eq!(mappings.get("record_declaration"), "RecordDeclaration");
        assert_eq!(mappings.get("module_declaration"), "ModuleDeclaration");
        assert_eq!(mappings.get("with_abstraction"), "WithAbstraction");
        assert_eq!(mappings.get("pi_type"), "PiType");
        assert_eq!(mappings.get("forall_expression"), "ForallExpression");
        assert_eq!(mappings.get("implicit_argument"), "ImplicitArgument");
        assert_eq!(mappings.get("hole"), "Hole");
        assert_eq!(mappings.get("postulate"), "PostulateDeclaration");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_reverse_lookup_agda() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "agda");
        assert!(native_types.contains(&"function_clause"));

        let native_types = get_native_types_for_uast("TypeDeclaration", "agda");
        assert!(native_types.contains(&"data_declaration"));

        let native_types = get_native_types_for_uast("RecordDeclaration", "agda");
        assert!(native_types.contains(&"record_declaration"));

        let native_types = get_native_types_for_uast("ModuleDeclaration", "agda");
        assert!(native_types.contains(&"module_declaration"));

        let native_types = get_native_types_for_uast("WithAbstraction", "agda");
        assert!(native_types.contains(&"with_abstraction"));
    }

    #[test]
    fn test_csv_mappings() {
        let mappings = get_mappings("csv");
        // CSV-specific mappings
        assert_eq!(mappings.get("document"), "Document");
        assert_eq!(mappings.get("row"), "Row");
        assert_eq!(mappings.get("field"), "Field");
        assert_eq!(mappings.get("boolean"), "BooleanLiteral");
        assert_eq!(mappings.get("float"), "NumberLiteral");
        assert_eq!(mappings.get("number"), "NumberLiteral");
        assert_eq!(mappings.get("text"), "StringLiteral");
    }

    #[test]
    fn test_comment_mappings() {
        let mappings = get_mappings("comment");
        // Comment grammar-specific mappings
        assert_eq!(mappings.get("source"), "SourceFile");
        assert_eq!(mappings.get("tag"), "Tag");
        assert_eq!(mappings.get("name"), "Identifier");
        assert_eq!(mappings.get("uri"), "Uri");
        assert_eq!(mappings.get("user"), "UserReference");
        assert_eq!(mappings.get("text"), "Text");
    }

    #[test]
    fn test_doxygen_mappings() {
        let mappings = get_mappings("doxygen");
        // Doxygen-specific mappings
        assert_eq!(mappings.get("document"), "Document");
        assert_eq!(mappings.get("description"), "Description");
        assert_eq!(mappings.get("brief_description"), "BriefDescription");
        assert_eq!(mappings.get("code_block"), "CodeBlock");
        assert_eq!(mappings.get("code_word"), "InlineCode");
        assert_eq!(mappings.get("identifier"), "Identifier");
        assert_eq!(mappings.get("qualified_identifier"), "QualifiedIdentifier");
        assert_eq!(mappings.get("function"), "FunctionReference");
        assert_eq!(mappings.get("link"), "Link");
        assert_eq!(mappings.get("tag"), "Tag");
        assert_eq!(mappings.get("emphasis"), "Emphasis");
        assert_eq!(mappings.get("text"), "Text");
    }

    #[test]
    fn test_vim_mappings() {
        let mappings = get_mappings("vim");
        // Vim script-specific mappings
        assert_eq!(mappings.get("function_definition"), "FunctionDeclaration");
        assert_eq!(mappings.get("lambda_expression"), "LambdaExpression");
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("else_statement"), "ElseClause");
        assert_eq!(mappings.get("for_loop"), "ForStatement");
        assert_eq!(mappings.get("while_loop"), "WhileStatement");
        assert_eq!(mappings.get("try_statement"), "TryStatement");
        assert_eq!(mappings.get("catch_statement"), "CatchClause");
        assert_eq!(mappings.get("call_expression"), "CallExpression");
        assert_eq!(mappings.get("binary_operation"), "BinaryExpression");
        assert_eq!(mappings.get("let_statement"), "VariableDeclaration");
        assert_eq!(mappings.get("string_literal"), "StringLiteral");
        assert_eq!(mappings.get("integer_literal"), "NumberLiteral");
        assert_eq!(mappings.get("list"), "ArrayExpression");
        assert_eq!(mappings.get("dictionnary"), "ObjectExpression");
        assert_eq!(mappings.get("autocmd_statement"), "AutocmdStatement");
        assert_eq!(mappings.get("map_statement"), "MapStatement");
        assert_eq!(mappings.get("set_statement"), "SetStatement");
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_vim_aliases() {
        // Test all Vim language aliases map to the same mappings
        assert!(std::ptr::eq(get_mappings("vim"), get_mappings("viml")));
        assert!(std::ptr::eq(get_mappings("vim"), get_mappings("vimscript")));
    }

    #[test]
    fn test_reverse_lookup_vim() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "vim");
        assert!(native_types.contains(&"function_definition"));

        let native_types = get_native_types_for_uast("IfStatement", "vim");
        assert!(native_types.contains(&"if_statement"));

        let native_types = get_native_types_for_uast("ForStatement", "vim");
        assert!(native_types.contains(&"for_loop"));

        let native_types = get_native_types_for_uast("CallExpression", "vim");
        assert!(native_types.contains(&"call_expression"));

        let native_types = get_native_types_for_uast("VariableDeclaration", "vim");
        assert!(native_types.contains(&"let_statement"));
    }

    // === NEW LANGUAGE MAPPINGS TESTS ===

    #[test]
    fn test_gleam_mappings() {
        let mappings = get_mappings("gleam");
        // Gleam-specific mappings
        assert_eq!(mappings.get("function"), "FunctionDeclaration");
        assert_eq!(mappings.get("type_definition"), "TypeDeclaration");
        assert_eq!(mappings.get("type_alias"), "TypeAlias");
        assert_eq!(mappings.get("constant"), "ConstantDeclaration");
        assert_eq!(mappings.get("case"), "MatchExpression");
        assert_eq!(mappings.get("case_clause"), "MatchArm");
        assert_eq!(mappings.get("function_call"), "CallExpression");
        assert_eq!(mappings.get("anonymous_function"), "LambdaExpression");
        assert_eq!(mappings.get("let"), "VariableDeclaration");
        assert_eq!(mappings.get("import"), "ImportDeclaration");
        assert_eq!(mappings.get("tuple"), "TupleExpression");
        assert_eq!(mappings.get("list"), "ListExpression");
        assert_eq!(mappings.get("record"), "RecordExpression");
        assert_eq!(mappings.get("bit_array"), "BitArray");
        // Should also have generic mappings
        assert_eq!(mappings.get("identifier"), "Identifier");
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_reverse_lookup_gleam() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "gleam");
        assert!(native_types.contains(&"function"));

        let native_types = get_native_types_for_uast("TypeDeclaration", "gleam");
        assert!(native_types.contains(&"type_definition"));

        let native_types = get_native_types_for_uast("MatchExpression", "gleam");
        assert!(native_types.contains(&"case"));

        let native_types = get_native_types_for_uast("CallExpression", "gleam");
        assert!(native_types.contains(&"function_call"));
    }

    #[test]
    fn test_svelte_mappings() {
        let mappings = get_mappings("svelte");
        // Svelte-specific mappings
        assert_eq!(mappings.get("document"), "Document");
        assert_eq!(mappings.get("element"), "Element");
        assert_eq!(mappings.get("script_element"), "ScriptElement");
        assert_eq!(mappings.get("style_element"), "StyleElement");
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("each_statement"), "EachStatement");
        assert_eq!(mappings.get("await_statement"), "AwaitStatement");
        assert_eq!(mappings.get("key_statement"), "KeyStatement");
        assert_eq!(mappings.get("snippet_statement"), "SnippetStatement");
        assert_eq!(mappings.get("start_tag"), "StartTag");
        assert_eq!(mappings.get("end_tag"), "EndTag");
        assert_eq!(mappings.get("attribute"), "Attribute");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_reverse_lookup_svelte() {
        let native_types = get_native_types_for_uast("Element", "svelte");
        assert!(native_types.contains(&"element"));

        let native_types = get_native_types_for_uast("IfStatement", "svelte");
        assert!(native_types.contains(&"if_statement"));

        let native_types = get_native_types_for_uast("EachStatement", "svelte");
        assert!(native_types.contains(&"each_statement"));

        let native_types = get_native_types_for_uast("AwaitStatement", "svelte");
        assert!(native_types.contains(&"await_statement"));
    }

    #[test]
    fn test_wgsl_mappings() {
        let mappings = get_mappings("wgsl");
        // WGSL-specific mappings
        assert_eq!(mappings.get("function_declaration"), "FunctionDeclaration");
        assert_eq!(mappings.get("struct_declaration"), "StructDeclaration");
        assert_eq!(mappings.get("type_alias_declaration"), "TypeAlias");
        assert_eq!(mappings.get("global_variable_declaration"), "GlobalVariableDeclaration");
        assert_eq!(mappings.get("variable_declaration"), "VariableDeclaration");
        assert_eq!(mappings.get("if_statement"), "IfStatement");
        assert_eq!(mappings.get("for_statement"), "ForStatement");
        assert_eq!(mappings.get("while_statement"), "WhileStatement");
        assert_eq!(mappings.get("loop_statement"), "LoopStatement");
        assert_eq!(mappings.get("switch_statement"), "SwitchStatement");
        assert_eq!(mappings.get("return_statement"), "ReturnStatement");
        assert_eq!(mappings.get("break_statement"), "BreakStatement");
        assert_eq!(mappings.get("continue_statement"), "ContinueStatement");
        assert_eq!(mappings.get("binary_expression"), "BinaryExpression");
        assert_eq!(mappings.get("unary_expression"), "UnaryExpression");
        assert_eq!(mappings.get("int_literal"), "NumberLiteral");
        assert_eq!(mappings.get("float_literal"), "NumberLiteral");
        assert_eq!(mappings.get("bool_literal"), "BooleanLiteral");
        // Should also have generic mappings
        assert_eq!(mappings.get("identifier"), "Identifier");
    }

    #[test]
    fn test_reverse_lookup_wgsl() {
        let native_types = get_native_types_for_uast("FunctionDeclaration", "wgsl");
        assert!(native_types.contains(&"function_declaration"));

        let native_types = get_native_types_for_uast("StructDeclaration", "wgsl");
        assert!(native_types.contains(&"struct_declaration"));

        let native_types = get_native_types_for_uast("IfStatement", "wgsl");
        assert!(native_types.contains(&"if_statement"));

        let native_types = get_native_types_for_uast("LoopStatement", "wgsl");
        assert!(native_types.contains(&"loop_statement"));
    }

    #[test]
    fn test_astro_mappings() {
        let mappings = get_mappings("astro");
        // Astro-specific mappings
        assert_eq!(mappings.get("document"), "Document");
        assert_eq!(mappings.get("frontmatter"), "Frontmatter");
        assert_eq!(mappings.get("frontmatter_js_block"), "FrontmatterJsBlock");
        assert_eq!(mappings.get("element"), "Element");
        assert_eq!(mappings.get("script_element"), "ScriptElement");
        assert_eq!(mappings.get("style_element"), "StyleElement");
        assert_eq!(mappings.get("start_tag"), "StartTag");
        assert_eq!(mappings.get("end_tag"), "EndTag");
        assert_eq!(mappings.get("self_closing_tag"), "SelfClosingTag");
        assert_eq!(mappings.get("attribute"), "Attribute");
        assert_eq!(mappings.get("html_interpolation"), "HtmlInterpolation");
        assert_eq!(mappings.get("text"), "Text");
        // Should also have generic mappings
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_reverse_lookup_astro() {
        let native_types = get_native_types_for_uast("Document", "astro");
        assert!(native_types.contains(&"document"));

        let native_types = get_native_types_for_uast("Frontmatter", "astro");
        assert!(native_types.contains(&"frontmatter"));

        let native_types = get_native_types_for_uast("Element", "astro");
        assert!(native_types.contains(&"element"));

        let native_types = get_native_types_for_uast("HtmlInterpolation", "astro");
        assert!(native_types.contains(&"html_interpolation"));
    }

    #[test]
    fn test_prisma_mappings() {
        let mappings = get_mappings("prisma");
        // Prisma-specific mappings
        assert_eq!(mappings.get("program"), "SourceFile");
        assert_eq!(mappings.get("model_declaration"), "ModelDeclaration");
        assert_eq!(mappings.get("enum_declaration"), "EnumDeclaration");
        assert_eq!(mappings.get("type_declaration"), "TypeDeclaration");
        assert_eq!(mappings.get("view_declaration"), "ViewDeclaration");
        assert_eq!(mappings.get("datasource_declaration"), "DatasourceDeclaration");
        assert_eq!(mappings.get("generator_declaration"), "GeneratorDeclaration");
        assert_eq!(mappings.get("column_declaration"), "FieldDeclaration");
        assert_eq!(mappings.get("call_expression"), "CallExpression");
        assert_eq!(mappings.get("member_expression"), "MemberExpression");
        assert_eq!(mappings.get("string"), "StringLiteral");
        assert_eq!(mappings.get("number"), "NumberLiteral");
        assert_eq!(mappings.get("true"), "BooleanLiteral");
        assert_eq!(mappings.get("false"), "BooleanLiteral");
        // Should also have generic mappings
        assert_eq!(mappings.get("identifier"), "Identifier");
        assert_eq!(mappings.get("comment"), "Comment");
    }

    #[test]
    fn test_reverse_lookup_prisma() {
        let native_types = get_native_types_for_uast("ModelDeclaration", "prisma");
        assert!(native_types.contains(&"model_declaration"));

        let native_types = get_native_types_for_uast("EnumDeclaration", "prisma");
        assert!(native_types.contains(&"enum_declaration"));

        let native_types = get_native_types_for_uast("FieldDeclaration", "prisma");
        assert!(native_types.contains(&"column_declaration"));

        let native_types = get_native_types_for_uast("DatasourceDeclaration", "prisma");
        assert!(native_types.contains(&"datasource_declaration"));
    }
}
