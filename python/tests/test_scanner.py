"""Tests for RuleScanner."""

import json
import pytest
from uast_grep import RuleScanner


class TestRuleScanner:
    """Tests for the RuleScanner class."""

    def test_create_scanner(self):
        """Test creating an empty scanner."""
        scanner = RuleScanner()
        assert scanner.rule_count() == 0

    def test_load_rules_from_string(self):
        """Test loading rules from YAML string."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: find-functions
language: python
severity: info
message: "Found a function"
rule:
  pattern: FunctionDeclaration
""")
        assert scanner.rule_count() == 1
        assert "find-functions" in scanner.rule_ids()

    def test_load_multiple_rules(self):
        """Test loading multiple rules."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: rule1
language: python
severity: info
message: "Rule 1"
rule:
  pattern: FunctionDeclaration
""")
        scanner.load_rules_from_string("""
id: rule2
language: python
severity: warning
message: "Rule 2"
rule:
  pattern: ClassDeclaration
""")
        assert scanner.rule_count() == 2

    def test_scan_basic(self):
        """Test basic scanning."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: find-functions
language: python
severity: info
message: "Found function"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan("def hello(): pass", "python")

        assert len(results) == 1
        result = results[0]
        assert result.rule_id == "find-functions"
        assert result.severity == "info"

    def test_scan_no_matches(self):
        """Test scanning with no matches."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: find-classes
language: python
severity: info
message: "Found class"
rule:
  pattern: ClassDeclaration
""")
        results = scanner.scan("x = 1", "python")

        assert len(results) == 0

    def test_scan_multiple_matches(self):
        """Test scanning with multiple matches."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: find-functions
language: python
severity: warning
message: "Found function"
rule:
  pattern: FunctionDeclaration
""")
        source = """
def foo(): pass
def bar(): pass
def baz(): pass
"""
        results = scanner.scan(source, "python")

        assert len(results) == 3

    def test_scan_with_file_path(self):
        """Test scanning with file path."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: find-functions
language: python
severity: info
message: "Found function"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan(
            "def hello(): pass",
            "python",
            file_path="/path/to/script.py"
        )

        assert len(results) == 1
        assert results[0].file_path == "/path/to/script.py"

    def test_scan_to_sarif(self):
        """Test SARIF output."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: find-functions
language: python
severity: warning
message: "Found function"
rule:
  pattern: FunctionDeclaration
""")
        sarif = scanner.scan_to_sarif("def hello(): pass", "python")

        # Verify it's valid JSON
        data = json.loads(sarif)
        assert data["version"] == "2.1.0"
        assert "$schema" in data
        assert len(data["runs"]) == 1

    def test_clear_rules(self):
        """Test clearing rules."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: rule1
language: python
severity: info
message: "Rule 1"
rule:
  pattern: FunctionDeclaration
""")
        assert scanner.rule_count() == 1

        scanner.clear_rules()
        assert scanner.rule_count() == 0

    def test_scanner_repr(self):
        """Test scanner string representation."""
        scanner = RuleScanner()
        assert "rules=0" in repr(scanner)


class TestScanResult:
    """Tests for ScanResult class."""

    def test_scan_result_properties(self):
        """Test ScanResult properties."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: test-rule
language: python
severity: error
message: "Test message"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan("def hello(): pass", "python")

        assert len(results) == 1
        result = results[0]

        assert result.rule_id == "test-rule"
        assert result.severity == "error"
        assert result.message == "Test message"
        assert result.span is not None

    def test_scan_result_span(self):
        """Test ScanResult span properties."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: test-rule
language: python
severity: info
message: "Found"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan("def hello(): pass", "python")

        span = results[0].span
        assert span.start_line >= 1
        assert span.start_byte < span.end_byte

    def test_scan_result_to_dict(self):
        """Test ScanResult to_dict method."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: test-rule
language: python
severity: info
message: "Found"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan("def hello(): pass", "python")

        d = results[0].to_dict()
        assert isinstance(d, dict)
        assert d["rule_id"] == "test-rule"
        assert d["severity"] == "info"

    def test_scan_result_repr(self):
        """Test ScanResult string representation."""
        scanner = RuleScanner()
        scanner.load_rules_from_string("""
id: test-rule
language: python
severity: info
message: "Found"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan("def hello(): pass", "python")

        repr_str = repr(results[0])
        assert "ScanResult" in repr_str
        assert "test-rule" in repr_str


class TestRuleSeverities:
    """Tests for different severity levels."""

    @pytest.mark.parametrize("severity", ["error", "warning", "info", "hint"])
    def test_severity_levels(self, severity):
        """Test all severity levels."""
        scanner = RuleScanner()
        scanner.load_rules_from_string(f"""
id: test-{severity}
language: python
severity: {severity}
message: "Test"
rule:
  pattern: FunctionDeclaration
""")
        results = scanner.scan("def hello(): pass", "python")

        assert len(results) == 1
        assert results[0].severity == severity
