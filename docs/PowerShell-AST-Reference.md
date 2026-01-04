# PowerShell AST Structure

## Base Classes (12 abstract)
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

## Concrete Node Types (63 types)

### Ast (7 types)
- CatchClauseAst: CatchTypes, IsCatchAll, Body
- NamedAttributeArgumentAst: ArgumentName, Argument, ExpressionOmitted
- NamedBlockAst: Unnamed, BlockKind, Statements, Traps
- ParamBlockAst: Attributes, Parameters
- ParameterAst: Attributes, Name, DefaultValue, StaticType
- ScriptBlockAst: Attributes, UsingStatements, ParamBlock, BeginBlock, ProcessBlock, EndBlock, CleanBlock, DynamicParamBlock, ScriptRequirements
- StatementBlockAst: Statements, Traps

### AttributeBaseAst (2 types)
- AttributeAst: PositionalArguments, NamedArguments
- TypeConstraintAst

### AttributedExpressionAst (1 types)
- ConvertExpressionAst: Type, StaticType

### ChainableAst (2 types)
- PipelineAst: PipelineElements, Background
- PipelineChainAst: LhsPipelineChain, RhsPipeline, Operator, Background

### CommandBaseAst (2 types)
- CommandAst: CommandElements, InvocationOperator, DefiningKeyword
- CommandExpressionAst: Expression

### CommandElementAst (1 types)
- CommandParameterAst: ParameterName, Argument, ErrorPosition

### ConstantExpressionAst (1 types)
- StringConstantExpressionAst: StringConstantType, Value, StaticType

### ExpressionAst (18 types)
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

### InvokeMemberExpressionAst (1 types)
- BaseCtorInvokeMemberExpressionAst

### LabeledStatementAst (1 types)
- SwitchStatementAst: Flags, Clauses, Default

### LoopStatementAst (5 types)
- DoUntilStatementAst
- DoWhileStatementAst
- ForEachStatementAst: Variable, ThrottleLimit, Flags
- ForStatementAst: Initializer, Iterator
- WhileStatementAst

### MemberAst (2 types)
- FunctionMemberAst: Name, Attributes, ReturnType, Parameters, Body, MethodAttributes, IsPublic, IsPrivate, IsHidden, IsStatic, IsConstructor
- PropertyMemberAst: Name, PropertyType, Attributes, PropertyAttributes, InitialValue, IsPublic, IsPrivate, IsHidden, IsStatic

### MemberExpressionAst (1 types)
- InvokeMemberExpressionAst: GenericTypeArguments, Arguments

### PipelineBaseAst (2 types)
- AssignmentStatementAst: Left, Operator, Right, ErrorPosition
- ErrorStatementAst: Kind, Flags, Conditions, Bodies, NestedAst

### RedirectionAst (2 types)
- FileRedirectionAst: Location, Append
- MergingRedirectionAst: ToStream

### StatementAst (15 types)
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
