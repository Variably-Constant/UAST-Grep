"""Type stubs for uast_grep native extension."""

from typing import Dict, List, Optional, Any

# ============================================================================
# SourceSpan
# ============================================================================

class SourceSpan:
    """A source location span with start and end positions."""

    start_line: int
    """Start line (1-indexed)."""

    start_column: int
    """Start column (0-indexed)."""

    end_line: int
    """End line (1-indexed)."""

    end_column: int
    """End column (0-indexed)."""

    start_byte: int
    """Start byte offset."""

    end_byte: int
    """End byte offset."""

    def __init__(
        self,
        start_line: int,
        start_column: int,
        end_line: int,
        end_column: int,
        start_byte: int,
        end_byte: int,
    ) -> None: ...

    def to_dict(self) -> Dict[str, int]:
        """Convert to a dictionary."""
        ...

# ============================================================================
# UastParser
# ============================================================================

class UastParser:
    """A parser for a specific programming language."""

    language: str
    """The language this parser is configured for."""

    def __init__(self, language: str) -> None:
        """
        Create a new parser for the specified language.

        Args:
            language: The language name (e.g., "python", "rust", "javascript").
                      Use `supported_languages()` to get the full list.

        Raises:
            ValueError: If the language is not supported.
        """
        ...

    def parse(self, source: str, path: Optional[str] = None) -> "UastTree":
        """
        Parse source code into an AST tree.

        Args:
            source: The source code to parse.
            path: Optional file path for error reporting.

        Returns:
            A `UastTree` containing the parsed AST.

        Raises:
            ValueError: If parsing fails.
        """
        ...

    def parse_to_dict(self, source: str) -> Dict[str, Any]:
        """
        Parse source code and return a Python dictionary representation.

        Args:
            source: The source code to parse.

        Returns:
            A dictionary representation of the AST.
        """
        ...

# ============================================================================
# UastTree
# ============================================================================

class UastTree:
    """A parsed AST tree."""

    path: Optional[str]
    """The file path (if set)."""

    def root(self) -> "UastNode":
        """Get the root node of the tree."""
        ...

    def walk(self) -> List["UastNode"]:
        """
        Get all nodes in the tree via depth-first traversal.

        Returns:
            A list of all nodes in pre-order traversal.
        """
        ...

    def find_by_kind(self, kind: str) -> List["UastNode"]:
        """
        Find all nodes of a specific kind.

        Args:
            kind: The node kind to search for (e.g., "function_definition").

        Returns:
            A list of matching nodes.
        """
        ...

    def query(self, query: str) -> List["QueryMatch"]:
        """
        Execute a tree-sitter query on this tree.

        Args:
            query: A tree-sitter S-expression query string.

        Returns:
            A list of query matches.
        """
        ...

    def has_error(self) -> bool:
        """Check if the tree has any parse errors."""
        ...

    def node_count(self) -> int:
        """Get the total number of nodes in the tree."""
        ...

    def source(self) -> str:
        """Get the source code."""
        ...

    def language(self) -> str:
        """Get the language name."""
        ...

    def to_json(self) -> str:
        """Convert the tree to a JSON string."""
        ...

    def to_dict(self) -> Dict[str, Any]:
        """Convert the tree to a dictionary."""
        ...

# ============================================================================
# UastNode
# ============================================================================

class UastNode:
    """An AST node."""

    def kind(self) -> str:
        """Get the node kind (type)."""
        ...

    def is_named(self) -> bool:
        """Check if this is a named node (not anonymous punctuation)."""
        ...

    def is_error(self) -> bool:
        """Check if this is an error node."""
        ...

    def text(self) -> str:
        """Get the source text for this node."""
        ...

    def span(self) -> Optional[SourceSpan]:
        """Get the source location span."""
        ...

    def start_line(self) -> int:
        """Get the start line (1-indexed)."""
        ...

    def start_column(self) -> int:
        """Get the start column (0-indexed)."""
        ...

    def end_line(self) -> int:
        """Get the end line (1-indexed)."""
        ...

    def end_column(self) -> int:
        """Get the end column (0-indexed)."""
        ...

    def parent(self) -> Optional["UastNode"]:
        """Get the parent node."""
        ...

    def children(self) -> List["UastNode"]:
        """Get the child nodes."""
        ...

    def named_children(self) -> List["UastNode"]:
        """Get named child nodes (excludes anonymous punctuation)."""
        ...

    def child_by_field(self, field: str) -> Optional["UastNode"]:
        """Get a child node by field name."""
        ...

    def child_count(self) -> int:
        """Get the number of children."""
        ...

    def named_child_count(self) -> int:
        """Get the number of named children."""
        ...

    def next_sibling(self) -> Optional["UastNode"]:
        """Get the next sibling node."""
        ...

    def prev_sibling(self) -> Optional["UastNode"]:
        """Get the previous sibling node."""
        ...

    def to_dict(self) -> Dict[str, Any]:
        """Convert to a dictionary."""
        ...

# ============================================================================
# QueryMatch
# ============================================================================

class QueryMatch:
    """A query match result."""

    pattern_index: int
    """Index of the matched pattern."""

    def captures(self) -> Dict[str, UastNode]:
        """Get all captures as a dictionary."""
        ...

    def get_capture(self, name: str) -> Optional[UastNode]:
        """Get a specific capture by name."""
        ...

    def capture_names(self) -> List[str]:
        """Get capture names."""
        ...

# ============================================================================
# PatternMatcher
# ============================================================================

class PatternMatcher:
    """A pattern matcher for UAST patterns."""

    pattern: str
    """The pattern string."""

    def __init__(self, pattern: str, language: Optional[str] = None) -> None:
        """
        Create a new pattern matcher.

        Args:
            pattern: The pattern to match. Can be:
                - UAST pattern: `FunctionDeclaration`, `$NAME`, `$$ARGS`
                - Native pattern: `function_definition`, `function_item`
                - S-expression: `(function_definition name: (identifier) @name)`
            language: Optional language hint for native patterns.
        """
        ...

    def matches(self, source: str, language: str) -> List["MatchResult"]:
        """
        Match the pattern against source code.

        Args:
            source: The source code to search.
            language: The programming language of the source.

        Returns:
            A list of `MatchResult` objects.
        """
        ...

    def matches_tree(self, tree: UastTree) -> List["MatchResult"]:
        """
        Match the pattern against a parsed tree.

        Args:
            tree: A `UastTree` to search.

        Returns:
            A list of `MatchResult` objects.
        """
        ...

    def is_uast_pattern(self) -> bool:
        """Check if this is a UAST pattern (PascalCase)."""
        ...

# ============================================================================
# MatchResult
# ============================================================================

class MatchResult:
    """A pattern match result."""

    kind: str
    """The matched node kind."""

    text: str
    """The matched text."""

    span: SourceSpan
    """The source location."""

    def captures(self) -> Dict[str, str]:
        """Get all captured metavariables as a dictionary."""
        ...

    def get_capture(self, name: str) -> Optional[str]:
        """Get a specific capture by name."""
        ...

    def capture_names(self) -> List[str]:
        """Get all capture names."""
        ...

# ============================================================================
# RuleScanner
# ============================================================================

class RuleScanner:
    """A rule-based scanner for source code."""

    def __init__(self) -> None:
        """Create a new empty scanner."""
        ...

    def load_rules(self, path: str) -> None:
        """
        Load rules from a YAML file.

        Args:
            path: Path to a YAML file containing rules.

        Raises:
            ValueError: If the file cannot be read or parsed.
        """
        ...

    def load_rules_from_string(self, yaml: str) -> None:
        """
        Load rules from a YAML string.

        Args:
            yaml: YAML string containing one or more rules.
        """
        ...

    def scan(
        self,
        source: str,
        language: str,
        file_path: Optional[str] = None,
    ) -> List["ScanResult"]:
        """
        Scan source code against loaded rules.

        Args:
            source: The source code to scan.
            language: The programming language.
            file_path: Optional file path for error reporting.

        Returns:
            A list of `ScanResult` objects.
        """
        ...

    def scan_to_sarif(
        self,
        source: str,
        language: str,
        file_path: Optional[str] = None,
    ) -> str:
        """
        Scan source code and return SARIF 2.1.0 JSON output.

        Args:
            source: The source code to scan.
            language: The programming language.
            file_path: Optional file path for SARIF output.

        Returns:
            A SARIF 2.1.0 JSON string.
        """
        ...

    def rule_count(self) -> int:
        """Get the number of loaded rules."""
        ...

    def rule_ids(self) -> List[str]:
        """Get loaded rule IDs."""
        ...

    def clear_rules(self) -> None:
        """Clear all loaded rules."""
        ...

# ============================================================================
# ScanResult
# ============================================================================

class ScanResult:
    """A scan result."""

    rule_id: str
    """The rule ID that matched."""

    severity: str
    """Severity level (error, warning, info, hint)."""

    message: str
    """The diagnostic message."""

    span: SourceSpan
    """Source location."""

    file_path: Optional[str]
    """Optional file path."""

    fix: Optional[str]
    """Optional fix text."""

    def captures(self) -> Dict[str, str]:
        """Get all captured metavariables as a dictionary."""
        ...

    def get_capture(self, name: str) -> Optional[str]:
        """Get a specific capture by name."""
        ...

    def to_dict(self) -> Dict[str, Any]:
        """Convert to a dictionary."""
        ...

# ============================================================================
# Utility Functions
# ============================================================================

def supported_languages() -> List[str]:
    """
    Get a list of all supported languages.

    Returns a list of language names that can be used with `UastParser`.
    Includes both built-in languages (37) and WASM languages (34).
    """
    ...

def is_uast_pattern(pattern: str) -> bool:
    """
    Check if a pattern string is a UAST pattern (PascalCase).

    UAST patterns use PascalCase node kinds like `FunctionDeclaration`
    which are auto-translated to native tree-sitter node types.

    Native patterns use snake_case like `function_item` or S-expressions.
    """
    ...

def language_for_extension(ext: str) -> Optional[str]:
    """
    Get the language name for a file extension.

    Args:
        ext: File extension including the dot (e.g., ".py", ".rs")

    Returns:
        Language name or None if unknown.
    """
    ...

def get_extensions(language: str) -> List[str]:
    """
    Get file extensions for a language.

    Args:
        language: Language name (e.g., "python", "rust")

    Returns:
        List of file extensions including the dot.
    """
    ...

def version() -> str:
    """Get the library version."""
    ...
