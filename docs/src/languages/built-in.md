# Built-in Languages

These 37 languages are compiled directly into UAST-Grep for instant availability.

## Language Reference

### Bash

**Extensions:** `.sh`, `.bash`, `.zsh`, `.bats`

```bash
# Find all functions
uast-grep run -p function_definition -l bash ./scripts

# Find command substitutions
uast-grep run -p command_substitution -l bash ./scripts
```

Common node types:
- `function_definition` - Function declarations
- `command` - Command invocations
- `pipeline` - Piped commands
- `if_statement`, `for_statement`, `while_statement`
- `variable_assignment` - Variable assignments
- `command_substitution` - `$(...)` or backticks

### C

**Extensions:** `.c`, `.h`

```bash
# Find all functions
uast-grep run -p function_definition -l c ./src

# Find pointer declarations
uast-grep run -p pointer_declarator -l c ./src
```

Common node types:
- `function_definition` - Function definitions
- `declaration` - Variable declarations
- `struct_specifier` - Struct definitions
- `pointer_declarator` - Pointer declarations
- `call_expression` - Function calls
- `if_statement`, `for_statement`, `while_statement`

### C++

**Extensions:** `.cpp`, `.cc`, `.cxx`, `.hpp`, `.hh`

```bash
# Find all classes
uast-grep run -p class_specifier -l cpp ./src

# Find templates
uast-grep run -p template_declaration -l cpp ./src
```

Common node types:
- `function_definition` - Function definitions
- `class_specifier` - Class definitions
- `template_declaration` - Template definitions
- `namespace_definition` - Namespace definitions
- `using_declaration` - Using declarations
- `lambda_expression` - Lambda expressions

### C#

**Extensions:** `.cs`

See dedicated [C# page](csharp.md) for full reference.

### CSS

**Extensions:** `.css`

```bash
# Find all rules
uast-grep run -p rule_set -l css ./styles

# Find media queries
uast-grep run -p media_statement -l css ./styles
```

Common node types:
- `rule_set` - CSS rules
- `selector` - Selectors
- `declaration` - Property declarations
- `media_statement` - Media queries
- `keyframes_statement` - Keyframe animations

### Dockerfile

**Extensions:** `Dockerfile`, `.dockerfile`

```bash
# Find all FROM instructions
uast-grep run -p from_instruction -l dockerfile .

# Find RUN commands
uast-grep run -p run_instruction -l dockerfile .
```

Common node types:
- `from_instruction` - FROM
- `run_instruction` - RUN
- `copy_instruction` - COPY
- `env_instruction` - ENV
- `expose_instruction` - EXPOSE
- `cmd_instruction` - CMD
- `entrypoint_instruction` - ENTRYPOINT

### Elixir

**Extensions:** `.ex`, `.exs`

```bash
# Find all functions
uast-grep run -p call -l elixir ./lib

# Find modules
uast-grep run -p call -l elixir ./lib
```

Common node types:
- `call` - Function calls and definitions
- `do_block` - Do blocks
- `atom` - Atoms
- `tuple` - Tuples
- `list` - Lists
- `map` - Maps

### Go

**Extensions:** `.go`

```bash
# Find all functions
uast-grep run -p function_declaration -l go ./src

# Find struct types
uast-grep run -p type_declaration -l go ./src
```

Common node types:
- `function_declaration` - Functions
- `method_declaration` - Methods
- `type_declaration` - Type definitions
- `struct_type` - Struct types
- `interface_type` - Interface types
- `call_expression` - Function calls
- `if_statement`, `for_statement`
- `go_statement` - Goroutines
- `defer_statement` - Defer statements

### GraphQL

**Extensions:** `.graphql`, `.gql`

```bash
# Find all queries
uast-grep run -p operation_definition -l graphql ./schema

# Find type definitions
uast-grep run -p object_type_definition -l graphql ./schema
```

Common node types:
- `operation_definition` - Query/mutation/subscription
- `object_type_definition` - Type definitions
- `field_definition` - Field definitions
- `input_object_type_definition` - Input types
- `directive_definition` - Directives

### HCL (Terraform)

**Extensions:** `.hcl`, `.tf`, `.tfvars`

```bash
# Find all resources
uast-grep run -p block -l hcl ./terraform

# Find variables
uast-grep run -p block -l hcl ./terraform
```

Common node types:
- `block` - Configuration blocks
- `attribute` - Attribute assignments
- `identifier` - Names
- `expression` - Expressions
- `function_call` - Function calls

### HTML

**Extensions:** `.html`, `.htm`

```bash
# Find all script tags
uast-grep run -p script_element -l html ./templates

# Find forms
uast-grep run -p element -l html ./templates
```

Common node types:
- `element` - HTML elements
- `start_tag`, `end_tag` - Tags
- `attribute` - Attributes
- `text` - Text content
- `script_element` - Script tags
- `style_element` - Style tags

### Java

**Extensions:** `.java`

```bash
# Find all classes
uast-grep run -p class_declaration -l java ./src

# Find methods
uast-grep run -p method_declaration -l java ./src
```

Common node types:
- `class_declaration` - Classes
- `method_declaration` - Methods
- `field_declaration` - Fields
- `constructor_declaration` - Constructors
- `interface_declaration` - Interfaces
- `annotation` - Annotations
- `if_statement`, `for_statement`, `while_statement`
- `try_statement` - Try/catch

### JavaScript

**Extensions:** `.js`, `.mjs`, `.cjs`

```bash
# Find all functions
uast-grep run -p function_declaration -l javascript ./src

# Find arrow functions
uast-grep run -p arrow_function -l javascript ./src

# Find async functions
uast-grep run -p 'async function §NAME(§§§ARGS) { §§§BODY }' -l javascript ./src
```

Common node types:
- `function_declaration` - Named functions
- `arrow_function` - Arrow functions
- `class_declaration` - Classes
- `call_expression` - Function calls
- `object` - Object literals
- `array` - Array literals
- `if_statement`, `for_statement`, `while_statement`
- `try_statement` - Try/catch
- `import_statement` - Imports
- `export_statement` - Exports

### JSON

**Extensions:** `.json`

```bash
# Find all objects
uast-grep run -p object -l json ./config

# Find arrays
uast-grep run -p array -l json ./config
```

Common node types:
- `object` - Objects
- `array` - Arrays
- `pair` - Key-value pairs
- `string` - Strings
- `number` - Numbers
- `true`, `false`, `null` - Literals

### Lua

**Extensions:** `.lua`

```bash
# Find all functions
uast-grep run -p function_definition -l lua ./scripts
```

Common node types:
- `function_definition` - Functions
- `function_call` - Function calls
- `if_statement`, `for_statement`, `while_statement`
- `table_constructor` - Table literals
- `assignment_statement` - Assignments

### Markdown

**Extensions:** `.md`, `.markdown`

```bash
# Find all headers
uast-grep run -p atx_heading -l markdown ./docs

# Find code blocks
uast-grep run -p fenced_code_block -l markdown ./docs
```

Common node types:
- `atx_heading` - Headers (#, ##, etc.)
- `paragraph` - Paragraphs
- `fenced_code_block` - Code blocks
- `link` - Links
- `image` - Images
- `list` - Lists
- `code_span` - Inline code

### PHP

**Extensions:** `.php`

```bash
# Find all functions
uast-grep run -p function_definition -l php ./src

# Find classes
uast-grep run -p class_declaration -l php ./src
```

Common node types:
- `function_definition` - Functions
- `class_declaration` - Classes
- `method_declaration` - Methods
- `call_expression` - Function calls
- `if_statement`, `for_statement`, `while_statement`
- `try_statement` - Try/catch

### PowerShell

**Extensions:** `.ps1`, `.psm1`, `.psd1`

See dedicated [PowerShell page](powershell.md) for full reference.

### Python

**Extensions:** `.py`, `.pyw`, `.pyi`

```bash
# Find all functions
uast-grep run -p function_definition -l python ./src

# Find classes
uast-grep run -p class_definition -l python ./src

# Find async functions
uast-grep run -p 'async def §NAME(§§§ARGS):' -l python ./src
```

Common node types:
- `function_definition` - Functions
- `class_definition` - Classes
- `decorated_definition` - Decorated functions/classes
- `call` - Function calls
- `if_statement`, `for_statement`, `while_statement`
- `try_statement` - Try/except
- `with_statement` - Context managers
- `import_statement`, `import_from_statement`
- `list`, `dictionary`, `set` - Collections

### Ruby

**Extensions:** `.rb`, `.rake`, `.gemspec`

```bash
# Find all methods
uast-grep run -p method -l ruby ./lib

# Find classes
uast-grep run -p class -l ruby ./lib
```

Common node types:
- `method` - Methods
- `class` - Classes
- `module` - Modules
- `call` - Method calls
- `if`, `unless` - Conditionals
- `while`, `until` - Loops
- `begin` - Exception handling

### Rust

**Extensions:** `.rs`

```bash
# Find all functions
uast-grep run -p function_item -l rust ./src

# Find impl blocks
uast-grep run -p impl_item -l rust ./src

# Find unsafe blocks
uast-grep run -p unsafe_block -l rust ./src
```

Common node types:
- `function_item` - Functions
- `impl_item` - Impl blocks
- `struct_item` - Structs
- `enum_item` - Enums
- `trait_item` - Traits
- `use_declaration` - Use statements
- `macro_invocation` - Macro calls
- `if_expression`, `match_expression`, `for_expression`
- `unsafe_block` - Unsafe blocks

### TypeScript

**Extensions:** `.ts`

```bash
# Find all functions
uast-grep run -p function_declaration -l typescript ./src

# Find interfaces
uast-grep run -p interface_declaration -l typescript ./src

# Find type aliases
uast-grep run -p type_alias_declaration -l typescript ./src
```

Common node types:
- All JavaScript node types, plus:
- `interface_declaration` - Interfaces
- `type_alias_declaration` - Type aliases
- `enum_declaration` - Enums
- `type_annotation` - Type annotations
- `type_parameters` - Generic parameters

### TSX

**Extensions:** `.tsx`

Combines TypeScript and JSX node types.

### Vue

**Extensions:** `.vue`

```bash
# Find all components
uast-grep run -p component -l vue ./src
```

Common node types:
- `component` - Vue components
- `template_element` - Template section
- `script_element` - Script section
- `style_element` - Style section

### XML

**Extensions:** `.xml`

```bash
# Find all elements
uast-grep run -p element -l xml ./config
```

Common node types:
- `element` - Elements
- `start_tag`, `end_tag` - Tags
- `attribute` - Attributes
- `text` - Text content
- `comment` - Comments

### YAML

**Extensions:** `.yaml`, `.yml`

```bash
# Find all mappings
uast-grep run -p block_mapping -l yaml ./config

# Find sequences
uast-grep run -p block_sequence -l yaml ./config
```

Common node types:
- `block_mapping` - Mappings (objects)
- `block_sequence` - Sequences (arrays)
- `block_mapping_pair` - Key-value pairs
- `flow_node` - Inline values
- `string_scalar` - Strings
- `integer_scalar`, `float_scalar` - Numbers
