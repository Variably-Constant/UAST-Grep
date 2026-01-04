"""
UAST-Grep: Cross-language AST search and analysis.

This package provides Python bindings for UAST-Grep, a high-performance
cross-language AST search tool built on tree-sitter.

Features:
- Parse 71+ programming languages
- Cross-language pattern matching with UAST patterns
- Native tree-sitter queries for maximum performance
- YAML-based rule scanning
- SARIF 2.1.0 output for CI/CD integration

Example:
    >>> from uast_grep import UastParser, PatternMatcher, RuleScanner
    >>>
    >>> # Parse source code
    >>> parser = UastParser("python")
    >>> tree = parser.parse("def hello(): pass")
    >>>
    >>> # Pattern matching (cross-language)
    >>> matcher = PatternMatcher("FunctionDeclaration")
    >>> matches = matcher.matches("def hello(): pass", "python")
    >>> print(len(matches))
    1
    >>>
    >>> # Rule scanning
    >>> scanner = RuleScanner()
    >>> scanner.load_rules_from_string('''
    ... id: find-functions
    ... language: python
    ... severity: info
    ... message: "Found function"
    ... rule:
    ...   pattern: FunctionDeclaration
    ... ''')
    >>> results = scanner.scan("def hello(): pass", "python")
"""

__version__ = "0.1.0"

# Import from native extension
from uast_grep._native import (
    # Parser
    UastParser,
    # Tree and nodes
    UastTree,
    UastNode,
    QueryMatch,
    SourceSpan,
    # Pattern matching
    PatternMatcher,
    MatchResult,
    # Rule scanning
    RuleScanner,
    ScanResult,
    # Utility functions
    supported_languages,
    is_uast_pattern,
    language_for_extension,
    get_extensions,
    version,
)

__all__ = [
    # Version
    "__version__",
    # Parser
    "UastParser",
    # Tree and nodes
    "UastTree",
    "UastNode",
    "QueryMatch",
    "SourceSpan",
    # Pattern matching
    "PatternMatcher",
    "MatchResult",
    # Rule scanning
    "RuleScanner",
    "ScanResult",
    # Utility functions
    "supported_languages",
    "is_uast_pattern",
    "language_for_extension",
    "get_extensions",
    "version",
]
