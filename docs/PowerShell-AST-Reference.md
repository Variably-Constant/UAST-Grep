# PowerShell AST Reference

This document provides a complete reference for PowerShell AST nodes in UAST-Grep, showing both the native tree-sitter node types and their corresponding UAST mappings.

## How to Use This Reference

- **Native Type** (snake_case): Use for maximum performance with language-specific patterns
- **UAST Type** (PascalCase): Use for cross-language patterns that work across 71 languages
- **PS Extension**: Indicates PowerShell-specific UAST types (only available for PowerShell)

```bash
# Using native type (PowerShell-specific, fastest)
uast-grep run -p "function_statement" -l powershell ./scripts

# Using UAST type (cross-language, works in Python, JS, etc.)
uast-grep run -p "FunctionDeclaration" -l powershell ./scripts
```

---

## PowerShell-Specific UAST Extensions

These UAST types are **unique to PowerShell** and capture concepts not found in most other languages:

| UAST Type | Description | Example |
|-----------|-------------|---------|
| `CommandExpression` | Cmdlet/command invocation | `Get-Process -Name pwsh` |
| `PipelineExpression` | Pipeline of commands | `Get-Process \| Where-Object {...}` |
| `HashtableExpression` | Hashtable literal | `@{ Key = 'Value' }` |
| `ScriptBlockExpression` | Script block literal | `{ param($x) $x * 2 }` |
| `CommandParameter` | Named parameter | `-Name`, `-Recurse` |
| `ExpandableStringLiteral` | Double-quoted string with variables | `"Hello $name"` |
| `HereStringLiteral` | Here-string (multi-line) | `@"...\n..."@` |
| `Variable` | Variable reference | `$myVar`, `$env:PATH` |
| `FlowControlStatement` | break/continue/return with label | `break :outer` |
| `DataStatement` | DATA section | `DATA { ... }` |
| `TrapStatement` | Trap error handler | `trap { ... }` |

---

## Complete Node Type Mapping

### Program Structure

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `program` | `SourceFile` | Root node of a script |
| `script_block` | `ScriptBlock` | Script block (PS Extension) |
| `script_block_body` | `Block` | Body of a script block |
| `statement_block` | `Block` | Block of statements |
| `statement_list` | `StatementList` | List of statements |
| `named_block` | `NamedBlock` | Begin/Process/End blocks |
| `named_block_list` | `NamedBlockList` | List of named blocks |

### Functions and Parameters

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `function_statement` | `FunctionDeclaration` | Function definition |
| `function_name` | `Identifier` | Function name |
| `function_parameter_declaration` | `ParameterList` | Function parameters |
| `param_block` | `ParameterBlock` | Param() block |
| `parameter_list` | `ParameterList` | List of parameters |
| `script_parameter` | `Parameter` | Single parameter |
| `script_parameter_default` | `DefaultValue` | Default parameter value |

### Control Flow - Conditionals

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `if_statement` | `IfStatement` | If conditional |
| `elseif_clause` | `ElseIfClause` | ElseIf branch |
| `elseif_clauses` | `ElseIfClauses` | Multiple elseif branches |
| `else_clause` | `ElseClause` | Else branch |
| `switch_statement` | `SwitchStatement` | Switch statement |
| `switch_body` | `SwitchBody` | Switch body |
| `switch_clause` | `SwitchCase` | Single switch case |
| `switch_clause_condition` | `SwitchCaseCondition` | Case condition |
| `switch_clauses` | `SwitchCases` | Multiple cases |

### Control Flow - Loops

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `for_statement` | `ForStatement` | For loop |
| `for_initializer` | `ForInitializer` | Loop initializer |
| `for_condition` | `ForCondition` | Loop condition |
| `for_iterator` | `ForIterator` | Loop iterator |
| `foreach_statement` | `ForEachStatement` | ForEach loop |
| `while_statement` | `WhileStatement` | While loop |
| `while_condition` | `WhileCondition` | While condition |
| `do_statement` | `DoStatement` | Do-While/Do-Until loop |

### Control Flow - Flow Control

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `flow_control_statement` | `FlowControlStatement` | break/continue/return (PS Extension) |
| `label_expression` | `LabelExpression` | Label for loops |

### Error Handling

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `try_statement` | `TryStatement` | Try block |
| `catch_clause` | `CatchClause` | Catch block |
| `catch_clauses` | `CatchClauses` | Multiple catch blocks |
| `catch_type_list` | `CatchTypeList` | Exception types to catch |
| `finally_clause` | `FinallyClause` | Finally block |
| `trap_statement` | `TrapStatement` | Trap error handler (PS Extension) |

### Classes and Enums

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `class_statement` | `TypeDeclaration` | Class definition |
| `class_property_definition` | `PropertyDeclaration` | Class property |
| `class_method_definition` | `MethodDeclaration` | Class method |
| `class_method_parameter` | `Parameter` | Method parameter |
| `class_method_parameter_list` | `ParameterList` | Method parameters |
| `class_attribute` | `Attribute` | Class attribute |
| `enum_statement` | `EnumDeclaration` | Enum definition |
| `enum_member` | `EnumMember` | Enum member |

### Commands and Pipelines

| Native Type (tree-sitter) | UAST Type | PS Extension? |
|---------------------------|-----------|---------------|
| `pipeline` | `PipelineExpression` | Yes |
| `pipeline_chain` | `PipelineChain` | - |
| `command` | `CommandExpression` | Yes |
| `command_name` | `CommandName` | - |
| `command_name_expr` | `CommandNameExpression` | - |
| `command_elements` | `ArgumentList` | - |
| `command_parameter` | `CommandParameter` | Yes |
| `command_invokation_operator` | `InvocationOperator` | - |
| `path_command_name` | `PathCommandName` | - |

### Expressions

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `assignment_expression` | `AssignmentExpression` | `$x = 5` |
| `left_assignment_expression` | `LeftAssignmentExpression` | Left side of assignment |
| `logical_expression` | `LogicalExpression` | `-and`, `-or`, `-not` |
| `bitwise_expression` | `BitwiseExpression` | `-band`, `-bor`, `-bxor` |
| `comparison_expression` | `ComparisonExpression` | `-eq`, `-lt`, `-gt`, etc. |
| `additive_expression` | `BinaryExpression` | `+`, `-` |
| `multiplicative_expression` | `BinaryExpression` | `*`, `/`, `%` |
| `format_expression` | `FormatExpression` | `-f` format operator |
| `range_expression` | `RangeExpression` | `1..10` |
| `array_literal_expression` | `ArrayExpression` | `1, 2, 3` |
| `unary_expression` | `UnaryExpression` | `-not`, `!`, etc. |
| `expression_with_unary_operator` | `UnaryExpression` | Prefix unary |
| `pre_increment_expression` | `UpdateExpression` | `++$x` |
| `pre_decrement_expression` | `UpdateExpression` | `--$x` |
| `post_increment_expression` | `UpdateExpression` | `$x++` |
| `post_decrement_expression` | `UpdateExpression` | `$x--` |
| `cast_expression` | `CastExpression` | `[int]$x` |
| `parenthesized_expression` | `ParenthesizedExpression` | `(...)` |
| `sub_expression` | `SubExpression` | `$(...)` |
| `array_expression` | `ArrayExpression` | `@(...)` |
| `hash_literal_expression` | `HashtableExpression` | `@{...}` (PS Extension) |
| `hash_literal_body` | `HashtableBody` | Hashtable contents |
| `hash_entry` | `HashtableEntry` | Single key-value pair |
| `key_expression` | `KeyExpression` | Hashtable key |
| `member_access` | `MemberExpression` | `$obj.Property` |
| `member_name` | `PropertyIdentifier` | Property name |
| `element_access` | `IndexExpression` | `$arr[0]` |
| `invokation_expression` | `CallExpression` | Method call |
| `invokation_foreach_expression` | `ForEachMethodExpression` | `.ForEach()` method |
| `script_block_expression` | `ScriptBlockExpression` | `{...}` (PS Extension) |

### Arguments

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `argument_list` | `ArgumentList` | List of arguments |
| `argument_expression_list` | `ArgumentList` | Expression arguments |
| `argument_expression` | `Argument` | Single argument |

### Literals and Identifiers

| Native Type (tree-sitter) | UAST Type | PS Extension? |
|---------------------------|-----------|---------------|
| `integer_literal` | `NumberLiteral` | - |
| `decimal_integer_literal` | `NumberLiteral` | - |
| `hexadecimal_integer_literal` | `NumberLiteral` | - |
| `real_literal` | `NumberLiteral` | - |
| `string_literal` | `StringLiteral` | - |
| `expandable_string_literal` | `ExpandableStringLiteral` | Yes |
| `expandable_here_string_literal` | `HereStringLiteral` | Yes |
| `verbatim_string_characters` | `StringLiteral` | - |
| `verbatim_here_string_characters` | `HereStringLiteral` | Yes |
| `variable` | `Variable` | Yes |
| `braced_variable` | `Variable` | Yes |
| `simple_name` | `Identifier` | - |

### Types

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `type_name` | `TypeReference` | Type reference |
| `type_literal` | `TypeLiteral` | Type literal `[string]` |
| `type_spec` | `TypeSpec` | Type specification |
| `array_type_name` | `ArrayTypeReference` | Array type `[int[]]` |
| `generic_type_name` | `GenericTypeReference` | Generic `[List[string]]` |
| `generic_type_arguments` | `TypeArguments` | Generic type arguments |
| `type_identifier` | `TypeIdentifier` | Type name identifier |

### Operators

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `assignement_operator` | `AssignmentOperator` | `=`, `+=`, etc. |
| `comparison_operator` | `ComparisonOperator` | `-eq`, `-ne`, etc. |
| `format_operator` | `FormatOperator` | `-f` |
| `file_redirection_operator` | `RedirectionOperator` | `>`, `>>`, etc. |
| `merging_redirection_operator` | `RedirectionOperator` | `2>&1`, etc. |

### Attributes

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `attribute_list` | `AttributeList` | List of attributes |
| `attribute` | `Attribute` | Single attribute |
| `attribute_name` | `AttributeName` | Attribute name |
| `attribute_arguments` | `AttributeArguments` | Attribute arguments |
| `attribute_argument` | `AttributeArgument` | Single argument |

### Other Statements

| Native Type (tree-sitter) | UAST Type | PS Extension? |
|---------------------------|-----------|---------------|
| `data_statement` | `DataStatement` | Yes |
| `inlinescript_statement` | `InlineScriptStatement` | - |
| `parallel_statement` | `ParallelStatement` | - |
| `sequence_statement` | `SequenceStatement` | - |
| `empty_statement` | `EmptyStatement` | - |

### Redirections

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `redirections` | `Redirections` | List of redirections |
| `redirection` | `Redirection` | Single redirection |
| `redirected_file_name` | `RedirectedFileName` | Target file |

### Comments

| Native Type (tree-sitter) | UAST Type | Description |
|---------------------------|-----------|-------------|
| `comment` | `Comment` | Single-line `#` or block `<# #>` |

---

## Cross-Language Pattern Examples

### Find all function declarations (works in any language)
```bash
uast-grep run -p "FunctionDeclaration" ./src
```

### Find PowerShell pipelines (PowerShell-specific)
```bash
uast-grep run -p "PipelineExpression" -l powershell ./scripts
```

### Find try-catch blocks (cross-language)
```bash
uast-grep run -p "TryStatement" ./src
```

### Using native patterns for maximum precision
```bash
# Find functions with CmdletBinding attribute
uast-grep run -p "function_statement" -l powershell ./scripts

# Find hashtable literals
uast-grep run -p "hash_literal_expression" -l powershell ./scripts
```

---

## PowerShell AST Hierarchy (Original)

This section preserves the original PowerShell AST class hierarchy for reference.

### Base Classes (12 abstract)
- **Ast**
- **AttributeBaseAst** : Ast
- **ChainableAst** : PipelineBaseAst
- **CommandBaseAst** : StatementAst
- **CommandElementAst** : Ast
- **ExpressionAst** : CommandElementAst
- **LabeledStatementAst** : StatementAst
- **LoopStatementAst** : LabeledStatementAst
- **MemberAst** : Ast
- **PipelineBaseAst** : StatementAst
- **RedirectionAst** : Ast
- **StatementAst** : Ast

### Concrete Node Types (63 types)

#### Ast (7 types)
- CatchClauseAst: CatchTypes, IsCatchAll, Body
- NamedAttributeArgumentAst: ArgumentName, Argument, ExpressionOmitted
- NamedBlockAst: Unnamed, BlockKind, Statements, Traps
- ParamBlockAst: Attributes, Parameters
- ParameterAst: Attributes, Name, DefaultValue, StaticType
- ScriptBlockAst: Attributes, UsingStatements, ParamBlock, BeginBlock, ProcessBlock, EndBlock, CleanBlock, DynamicParamBlock, ScriptRequirements
- StatementBlockAst: Statements, Traps

#### AttributeBaseAst (2 types)
- AttributeAst: PositionalArguments, NamedArguments
- TypeConstraintAst

#### AttributedExpressionAst (1 types)
- ConvertExpressionAst: Type, StaticType

#### ChainableAst (2 types)
- PipelineAst: PipelineElements, Background
- PipelineChainAst: LhsPipelineChain, RhsPipeline, Operator, Background

#### CommandBaseAst (2 types)
- CommandAst: CommandElements, InvocationOperator, DefiningKeyword
- CommandExpressionAst: Expression

#### CommandElementAst (1 types)
- CommandParameterAst: ParameterName, Argument, ErrorPosition

#### ConstantExpressionAst (1 types)
- StringConstantExpressionAst: StringConstantType, Value, StaticType

#### ExpressionAst (18 types)
- ArrayExpressionAst: SubExpression, StaticType
- ArrayLiteralAst: Elements, StaticType
- AttributedExpressionAst: Child, Attribute
- BinaryExpressionAst: Operator, Left, Right, ErrorPosition, StaticType
- ConstantExpressionAst: Value, StaticType
- ErrorExpressionAst: NestedAst
- ExpandableStringExpressionAst: Value, StringConstantType, NestedExpressions, StaticType
- HashtableAst: KeyValuePairs, StaticType
- IndexExpressionAst: Target, Index, NullConditional
- MemberExpressionAst: Expression, Member, Static, NullConditional
- ParenExpressionAst: Pipeline
- ScriptBlockExpressionAst: ScriptBlock, StaticType
- SubExpressionAst: SubExpression
- TernaryExpressionAst: Condition, IfTrue, IfFalse
- TypeExpressionAst: TypeName, StaticType
- UnaryExpressionAst: TokenKind, Child, StaticType
- UsingExpressionAst: SubExpression
- VariableExpressionAst: VariablePath, Splatted

#### InvokeMemberExpressionAst (1 types)
- BaseCtorInvokeMemberExpressionAst

#### LabeledStatementAst (1 types)
- SwitchStatementAst: Flags, Clauses, Default

#### LoopStatementAst (5 types)
- DoUntilStatementAst
- DoWhileStatementAst
- ForEachStatementAst: Variable, ThrottleLimit, Flags
- ForStatementAst: Initializer, Iterator
- WhileStatementAst

#### MemberAst (2 types)
- FunctionMemberAst: Name, Attributes, ReturnType, Parameters, Body, MethodAttributes, IsPublic, IsPrivate, IsHidden, IsStatic, IsConstructor
- PropertyMemberAst: Name, PropertyType, Attributes, PropertyAttributes, InitialValue, IsPublic, IsPrivate, IsHidden, IsStatic

#### MemberExpressionAst (1 types)
- InvokeMemberExpressionAst: GenericTypeArguments, Arguments

#### PipelineBaseAst (2 types)
- AssignmentStatementAst: Left, Operator, Right, ErrorPosition
- ErrorStatementAst: Kind, Flags, Conditions, Bodies, NestedAst

#### RedirectionAst (2 types)
- FileRedirectionAst: Location, Append
- MergingRedirectionAst: ToStream

#### StatementAst (15 types)
- BlockStatementAst: Body, Kind
- BreakStatementAst: Label
- ConfigurationDefinitionAst: Body, ConfigurationType, InstanceName
- ContinueStatementAst: Label
- DataStatementAst: Variable, CommandsAllowed, Body
- DynamicKeywordStatementAst: CommandElements
- ExitStatementAst: Pipeline
- FunctionDefinitionAst: IsFilter, IsWorkflow, Name, Parameters, Body
- IfStatementAst: Clauses, ElseClause
- ReturnStatementAst: Pipeline
- ThrowStatementAst: Pipeline, IsRethrow
- TrapStatementAst: TrapType, Body
- TryStatementAst: Body, CatchClauses, Finally
- TypeDefinitionAst: Name, Attributes, BaseTypes, Members, TypeAttributes, IsEnum, IsClass, IsInterface
- UsingStatementAst: UsingStatementKind, Name, Alias, ModuleSpecification

---

## See Also

- [UAST Schema Reference](../native/uast_core/src/uast/schema.rs) - Full UAST type definitions
- [Language Mappings](../native/uast_core/src/uast/mappings.rs) - All 71 language mappings
- [YAML Rules Reference](./YAML-RULES-REFERENCE.md) - Rule authoring guide
