"""Tests for UastParser."""

import pytest
from uast_grep import UastParser, supported_languages, language_for_extension, get_extensions


class TestUastParser:
    """Tests for the UastParser class."""

    def test_create_parser_python(self):
        """Test creating a parser for Python."""
        parser = UastParser("python")
        assert parser.language == "python"

    def test_create_parser_rust(self):
        """Test creating a parser for Rust."""
        parser = UastParser("rust")
        assert parser.language == "rust"

    def test_create_parser_javascript(self):
        """Test creating a parser for JavaScript."""
        parser = UastParser("javascript")
        assert parser.language == "javascript"

    def test_create_parser_unknown_language(self):
        """Test that unknown languages raise ValueError."""
        with pytest.raises(ValueError, match="Unknown language"):
            UastParser("not_a_real_language")

    def test_parse_python_function(self):
        """Test parsing a Python function."""
        parser = UastParser("python")
        tree = parser.parse("def hello(): pass")

        assert not tree.has_error()
        assert tree.language() == "python"
        assert tree.node_count() > 0

        root = tree.root()
        assert root.kind() == "module"
        assert root.is_named()

    def test_parse_python_with_path(self):
        """Test parsing with a file path."""
        parser = UastParser("python")
        tree = parser.parse("def hello(): pass", path="/test/example.py")

        assert tree.path == "/test/example.py"

    def test_parse_rust_function(self):
        """Test parsing a Rust function."""
        parser = UastParser("rust")
        tree = parser.parse("fn hello() {}")

        assert not tree.has_error()
        root = tree.root()
        assert root.kind() == "source_file"

    def test_parse_to_dict(self):
        """Test parsing to dictionary."""
        parser = UastParser("python")
        ast = parser.parse_to_dict("x = 1")

        assert isinstance(ast, dict)
        assert ast["kind"] == "module"
        assert "children" in ast

    def test_parse_error_handling(self):
        """Test that parse errors are tracked."""
        parser = UastParser("python")
        # Invalid Python syntax
        tree = parser.parse("def hello(")

        # Parser should still return a tree, but with errors
        assert tree.has_error()

    def test_parser_repr(self):
        """Test parser string representation."""
        parser = UastParser("python")
        assert "python" in repr(parser)


class TestSupportedLanguages:
    """Tests for supported_languages function."""

    def test_returns_list(self):
        """Test that supported_languages returns a list."""
        langs = supported_languages()
        assert isinstance(langs, list)

    def test_includes_common_languages(self):
        """Test that common languages are included."""
        langs = supported_languages()
        assert "python" in langs
        assert "rust" in langs
        assert "javascript" in langs
        assert "c" in langs
        assert "cpp" in langs
        assert "java" in langs
        assert "go" in langs

    def test_includes_powershell(self):
        """Test that PowerShell is included."""
        langs = supported_languages()
        assert "powershell" in langs


class TestLanguageForExtension:
    """Tests for language_for_extension function."""

    def test_python_extension(self):
        """Test .py extension."""
        assert language_for_extension(".py") == "python"

    def test_rust_extension(self):
        """Test .rs extension."""
        assert language_for_extension(".rs") == "rust"

    def test_javascript_extension(self):
        """Test .js extension."""
        assert language_for_extension(".js") == "javascript"

    def test_unknown_extension(self):
        """Test unknown extension returns None."""
        assert language_for_extension(".xyz123") is None


class TestGetExtensions:
    """Tests for get_extensions function."""

    def test_python_extensions(self):
        """Test extensions for Python."""
        exts = get_extensions("python")
        assert ".py" in exts

    def test_rust_extensions(self):
        """Test extensions for Rust."""
        exts = get_extensions("rust")
        assert ".rs" in exts

    def test_unknown_language(self):
        """Test extensions for unknown language."""
        exts = get_extensions("not_a_language")
        assert exts == []
