//! Python bindings for RuleScanner.

use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::collections::HashMap;
use std::path::Path;

use crate::builtin_languages;
use crate::parser::ParsedTree;
use crate::rules::{RuleYaml, ScanResult, Scanner, Severity};
use crate::sarif::SarifWriter;
use crate::uast::convert::convert_node_to_uast;
use crate::python::tree::PySourceSpan;
use crate::python::types::{to_py_err, PySeverity};

/// A rule-based scanner for source code.
///
/// The scanner loads rules from YAML files and scans source code for matches.
/// Results can be output in JSON or SARIF 2.1.0 format.
///
/// Example:
///     >>> scanner = RuleScanner()
///     >>> scanner.load_rules("rules/security.yaml")
///     >>> results = scanner.scan("Write-Host 'Hello'", "powershell")
///     >>> for r in results:
///     ...     print(f"{r.rule_id}: {r.message}")
#[pyclass(name = "RuleScanner")]
pub struct PyRuleScanner {
    scanner: Scanner,
    rules: Vec<RuleYaml>,
}

#[pymethods]
impl PyRuleScanner {
    /// Create a new empty scanner.
    #[new]
    fn new() -> Self {
        PyRuleScanner {
            scanner: Scanner::new(),
            rules: Vec::new(),
        }
    }

    /// Load rules from a YAML file.
    ///
    /// Args:
    ///     path: Path to a YAML file containing rules.
    ///
    /// Raises:
    ///     ValueError: If the file cannot be read or parsed.
    ///
    /// Example:
    ///     >>> scanner.load_rules("rules/powershell.yaml")
    fn load_rules(&mut self, path: &str) -> PyResult<()> {
        let path = Path::new(path);
        let content = std::fs::read_to_string(path).map_err(to_py_err)?;
        self.load_rules_from_string(&content)
    }

    /// Load rules from a YAML string.
    ///
    /// Args:
    ///     yaml: YAML string containing one or more rules.
    ///
    /// Example:
    ///     >>> scanner.load_rules_from_string('''
    ///     ... id: no-write-host
    ///     ... language: powershell
    ///     ... severity: warning
    ///     ... message: "Avoid Write-Host"
    ///     ... rule:
    ///     ...   pattern: "Write-Host $$$ARGS"
    ///     ... ''')
    fn load_rules_from_string(&mut self, yaml: &str) -> PyResult<()> {
        // Try to parse as a list of rules first
        let rules: Vec<RuleYaml> = if yaml.trim().starts_with('-') || yaml.contains("\n---") {
            serde_yaml::from_str(yaml).map_err(to_py_err)?
        } else {
            // Single rule
            let rule: RuleYaml = serde_yaml::from_str(yaml).map_err(to_py_err)?;
            vec![rule]
        };

        for rule in rules {
            self.rules.push(rule.clone());
            self.scanner.add_rule(rule).map_err(to_py_err)?;
        }

        Ok(())
    }

    /// Scan source code against loaded rules.
    ///
    /// Args:
    ///     source: The source code to scan.
    ///     language: The programming language.
    ///     file_path: Optional file path for error reporting.
    ///
    /// Returns:
    ///     A list of `ScanResult` objects.
    ///
    /// Example:
    ///     >>> results = scanner.scan("Write-Host 'Hello'", "powershell")
    ///     >>> len(results)
    ///     1
    #[pyo3(signature = (source, language, file_path=None))]
    fn scan(
        &self,
        source: &str,
        language: &str,
        file_path: Option<&str>,
    ) -> PyResult<Vec<PyScanResult>> {
        // Get the language from built-in grammars
        let ts_language = builtin_languages::get_builtin_language(language)
            .ok_or_else(|| to_py_err(format!("unknown language: {}", language)))?;

        // Create tree-sitter parser directly
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_language)
            .map_err(|e| to_py_err(format!("failed to set language: {}", e)))?;

        // Parse the source
        let tree = parser.parse(source, None)
            .ok_or_else(|| to_py_err("failed to parse source"))?;

        // Wrap in ParsedTree
        let parsed_tree = ParsedTree::from_raw(tree, source, ts_language, language);

        // Convert to UAST
        let uast_root = convert_node_to_uast(parsed_tree.root_node(), parsed_tree.source(), language);

        // Scan
        let results = self
            .scanner
            .scan_source(&uast_root, language, file_path.map(Path::new));

        // Convert to Python results
        Ok(results.into_iter().map(PyScanResult::from).collect())
    }

    /// Scan source code and return SARIF 2.1.0 JSON output.
    ///
    /// Args:
    ///     source: The source code to scan.
    ///     language: The programming language.
    ///     file_path: Optional file path for SARIF output.
    ///
    /// Returns:
    ///     A SARIF 2.1.0 JSON string.
    ///
    /// Example:
    ///     >>> sarif = scanner.scan_to_sarif("Write-Host 'Hello'", "powershell")
    ///     >>> import json
    ///     >>> data = json.loads(sarif)
    ///     >>> data['version']
    ///     '2.1.0'
    #[pyo3(signature = (source, language, file_path=None))]
    fn scan_to_sarif(
        &self,
        source: &str,
        language: &str,
        file_path: Option<&str>,
    ) -> PyResult<String> {
        // Get the language from built-in grammars
        let ts_language = builtin_languages::get_builtin_language(language)
            .ok_or_else(|| to_py_err(format!("unknown language: {}", language)))?;

        // Create tree-sitter parser directly
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_language)
            .map_err(|e| to_py_err(format!("failed to set language: {}", e)))?;

        // Parse the source
        let tree = parser.parse(source, None)
            .ok_or_else(|| to_py_err("failed to parse source"))?;

        // Wrap in ParsedTree
        let parsed_tree = ParsedTree::from_raw(tree, source, ts_language, language);

        // Convert to UAST
        let uast_root = convert_node_to_uast(parsed_tree.root_node(), parsed_tree.source(), language);

        // Scan
        let results = self
            .scanner
            .scan_source(&uast_root, language, file_path.map(Path::new));

        // Convert to SARIF
        let writer = SarifWriter::new("UAST-Grep", env!("CARGO_PKG_VERSION"));
        let sarif_log = writer.from_scan_results(&results, &self.rules);
        writer.to_json(&sarif_log).map_err(to_py_err)
    }

    /// Get the number of loaded rules.
    fn rule_count(&self) -> usize {
        self.scanner.rule_count()
    }

    /// Get loaded rule IDs.
    fn rule_ids(&self) -> Vec<String> {
        self.rules.iter().map(|r| r.id.clone()).collect()
    }

    /// Clear all loaded rules.
    fn clear_rules(&mut self) {
        self.scanner = Scanner::new();
        self.rules.clear();
    }

    fn __repr__(&self) -> String {
        format!("RuleScanner(rules={})", self.scanner.rule_count())
    }
}

/// A scan result.
#[pyclass(name = "ScanResult")]
#[derive(Clone)]
pub struct PyScanResult {
    /// The rule ID that matched.
    #[pyo3(get)]
    pub rule_id: String,
    /// Severity level.
    #[pyo3(get)]
    pub severity: String,
    /// The diagnostic message.
    #[pyo3(get)]
    pub message: String,
    /// Source location.
    #[pyo3(get)]
    pub span: PySourceSpan,
    /// Optional file path.
    #[pyo3(get)]
    pub file_path: Option<String>,
    /// Optional fix text.
    #[pyo3(get)]
    pub fix: Option<String>,
    /// Captured metavariables.
    captures: HashMap<String, String>,
}

impl From<ScanResult> for PyScanResult {
    fn from(result: ScanResult) -> Self {
        PyScanResult {
            rule_id: result.rule_id,
            severity: match result.severity {
                Severity::Error => "error".to_string(),
                Severity::Warning => "warning".to_string(),
                Severity::Info => "info".to_string(),
                Severity::Hint => "hint".to_string(),
            },
            message: result.message,
            span: PySourceSpan {
                start_line: result.location.start_line,
                start_column: result.location.start_column,
                end_line: result.location.end_line,
                end_column: result.location.end_column,
                start_byte: result.location.start_offset,
                end_byte: result.location.end_offset,
            },
            file_path: result.file_path,
            fix: result.fix.map(|f| f.replacement),
            captures: result.captures,
        }
    }
}

#[pymethods]
impl PyScanResult {
    /// Get all captured metavariables as a dictionary.
    fn captures(&self, py: Python<'_>) -> PyResult<PyObject> {
        let dict = PyDict::new_bound(py);
        for (name, value) in &self.captures {
            dict.set_item(name, value)?;
        }
        Ok(dict.into())
    }

    /// Get a specific capture by name.
    fn get_capture(&self, name: &str) -> Option<String> {
        self.captures.get(name).cloned()
    }

    /// Convert to a dictionary.
    fn to_dict(&self, py: Python<'_>) -> PyResult<PyObject> {
        let dict = PyDict::new_bound(py);
        dict.set_item("rule_id", &self.rule_id)?;
        dict.set_item("severity", &self.severity)?;
        dict.set_item("message", &self.message)?;
        dict.set_item("start_line", self.span.start_line)?;
        dict.set_item("start_column", self.span.start_column)?;
        dict.set_item("end_line", self.span.end_line)?;
        dict.set_item("end_column", self.span.end_column)?;
        dict.set_item("file_path", &self.file_path)?;
        dict.set_item("fix", &self.fix)?;
        dict.set_item("captures", self.captures(py)?)?;
        Ok(dict.into())
    }

    fn __repr__(&self) -> String {
        format!(
            "ScanResult(rule='{}', severity='{}', line={})",
            self.rule_id, self.severity, self.span.start_line
        )
    }
}
