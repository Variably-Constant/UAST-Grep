"""Tests for PatternMatcher."""

import pytest
from uast_grep import PatternMatcher, is_uast_pattern


class TestPatternMatcher:
    """Tests for the PatternMatcher class."""

    def test_create_matcher_uast_pattern(self):
        """Test creating a matcher with UAST pattern."""
        matcher = PatternMatcher("FunctionDeclaration")
        assert matcher.pattern == "FunctionDeclaration"
        assert matcher.is_uast_pattern()

    def test_create_matcher_native_pattern(self):
        """Test creating a matcher with native pattern."""
        matcher = PatternMatcher("function_definition", "python")
        assert matcher.pattern == "function_definition"
        assert not matcher.is_uast_pattern()

    def test_match_python_function_uast(self):
        """Test matching Python function with UAST pattern."""
        matcher = PatternMatcher("FunctionDeclaration")
        matches = matcher.matches("def hello(): pass", "python")

        assert len(matches) == 1
        match = matches[0]
        assert match.kind == "FunctionDeclaration"
        assert "hello" in match.text

    def test_match_rust_function_uast(self):
        """Test matching Rust function with UAST pattern."""
        matcher = PatternMatcher("FunctionDeclaration")
        matches = matcher.matches("fn hello() {}", "rust")

        assert len(matches) == 1

    def test_match_multiple_functions(self):
        """Test matching multiple functions."""
        matcher = PatternMatcher("FunctionDeclaration")
        source = """
def foo(): pass
def bar(): pass
def baz(): pass
"""
        matches = matcher.matches(source, "python")

        assert len(matches) == 3

    def test_match_with_capture(self):
        """Test matching with metavariable capture."""
        matcher = PatternMatcher("$FUNC")
        matches = matcher.matches("def hello(): pass", "python")

        # Should match something and have captures
        assert len(matches) > 0

    def test_no_matches(self):
        """Test when pattern doesn't match."""
        matcher = PatternMatcher("ClassDeclaration")
        matches = matcher.matches("x = 1", "python")

        assert len(matches) == 0

    def test_matcher_repr(self):
        """Test matcher string representation."""
        matcher = PatternMatcher("FunctionDeclaration")
        assert "FunctionDeclaration" in repr(matcher)


class TestMatchResult:
    """Tests for MatchResult class."""

    def test_match_result_properties(self):
        """Test MatchResult properties."""
        matcher = PatternMatcher("FunctionDeclaration")
        matches = matcher.matches("def hello(): pass", "python")

        assert len(matches) == 1
        match = matches[0]

        assert match.kind
        assert match.text
        assert match.span is not None
        assert match.span.start_line >= 1

    def test_match_result_span(self):
        """Test MatchResult span properties."""
        matcher = PatternMatcher("FunctionDeclaration")
        matches = matcher.matches("def hello(): pass", "python")

        span = matches[0].span
        assert span.start_line == 1
        assert span.start_column >= 0
        assert span.end_line >= span.start_line
        assert span.start_byte < span.end_byte

    def test_match_result_repr(self):
        """Test MatchResult string representation."""
        matcher = PatternMatcher("FunctionDeclaration")
        matches = matcher.matches("def hello(): pass", "python")

        repr_str = repr(matches[0])
        assert "MatchResult" in repr_str


class TestIsUastPattern:
    """Tests for is_uast_pattern function."""

    def test_pascalcase_is_uast(self):
        """Test that PascalCase patterns are UAST."""
        assert is_uast_pattern("FunctionDeclaration")
        assert is_uast_pattern("ClassDeclaration")
        assert is_uast_pattern("IfStatement")

    def test_snakecase_is_native(self):
        """Test that snake_case patterns are native."""
        assert not is_uast_pattern("function_definition")
        assert not is_uast_pattern("class_definition")
        assert not is_uast_pattern("if_statement")

    def test_sexpression_is_native(self):
        """Test that S-expressions are native."""
        assert not is_uast_pattern("(function_definition)")
        assert not is_uast_pattern("(identifier) @name")

    def test_metavar_is_uast(self):
        """Test that metavariables are UAST."""
        assert is_uast_pattern("$NAME")
        assert is_uast_pattern("$$ARGS")
        assert is_uast_pattern("$$$ITEMS")
