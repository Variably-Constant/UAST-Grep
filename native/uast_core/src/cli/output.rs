//! Output formatters for CLI commands
//!
//! Supports multiple output formats:
//! - `text` - Human-readable with colors
//! - `json` - Machine-readable JSON
//! - `sarif` - SARIF 2.1.0 format
//! - `tree` - Indented AST tree view

use console::{style, Style};
use std::io::{self, Write};

use crate::rules::{ScanResult, Severity};
use crate::sarif::SarifWriter;
use crate::uast::schema::UastNode;

/// Output format for CLI commands
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum OutputFormat {
    #[default]
    Text,
    Json,
    Sarif,
    Tree,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "text" => Ok(OutputFormat::Text),
            "json" => Ok(OutputFormat::Json),
            "sarif" => Ok(OutputFormat::Sarif),
            "tree" => Ok(OutputFormat::Tree),
            _ => Err(format!("Unknown format: {}. Use text, json, sarif, or tree", s)),
        }
    }
}

/// Style configuration for colored output
pub struct OutputStyles {
    pub error: Style,
    pub warning: Style,
    pub info: Style,
    pub hint: Style,
    pub location: Style,
    pub rule_id: Style,
    pub fix: Style,
    pub node_kind: Style,
    pub node_name: Style,
    pub line_number: Style,
}

impl Default for OutputStyles {
    fn default() -> Self {
        Self {
            error: Style::new().red().bold(),
            warning: Style::new().yellow().bold(),
            info: Style::new().blue().bold(),
            hint: Style::new().cyan(),
            location: Style::new().white().dim(),
            rule_id: Style::new().magenta(),
            fix: Style::new().green(),
            node_kind: Style::new().cyan().bold(),
            node_name: Style::new().green(),
            line_number: Style::new().dim(),
        }
    }
}

/// Formatter for scan results
pub struct ScanResultFormatter {
    styles: OutputStyles,
    show_fixes: bool,
}

impl Default for ScanResultFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl ScanResultFormatter {
    pub fn new() -> Self {
        Self {
            styles: OutputStyles::default(),
            show_fixes: true,
        }
    }

    pub fn with_fixes(mut self, show: bool) -> Self {
        self.show_fixes = show;
        self
    }

    /// Format a single scan result as colored text
    pub fn format_text(&self, result: &ScanResult) -> String {
        let severity_style = match result.severity {
            Severity::Error => &self.styles.error,
            Severity::Warning => &self.styles.warning,
            Severity::Info => &self.styles.info,
            Severity::Hint => &self.styles.hint,
        };

        let severity_str = match result.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
            Severity::Hint => "hint",
        };

        let location = format!(
            "{}:{}:{}",
            result.file_path.as_deref().unwrap_or("<unknown>"),
            result.location.start_line,
            result.location.start_column + 1
        );

        let mut output = format!(
            "{}: {} [{}]: {}\n",
            self.styles.location.apply_to(&location),
            severity_style.apply_to(severity_str),
            self.styles.rule_id.apply_to(&result.rule_id),
            result.message
        );

        if self.show_fixes {
            if let Some(ref fix) = result.fix {
                output.push_str(&format!(
                    "  {} {}\n",
                    self.styles.fix.apply_to("fix:"),
                    fix.replacement
                ));
            }
        }

        output
    }

    /// Format multiple scan results as text
    pub fn format_text_all(&self, results: &[ScanResult]) -> String {
        let mut output = String::new();
        for result in results {
            output.push_str(&self.format_text(result));
        }
        output
    }

    /// Format scan results as JSON
    pub fn format_json(&self, results: &[ScanResult]) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(results)
    }

    /// Format scan results as SARIF
    pub fn format_sarif(&self, results: &[ScanResult]) -> Result<String, serde_json::Error> {
        let writer = SarifWriter::new("UAST-Grep", env!("CARGO_PKG_VERSION"));
        let log = writer.from_scan_results(results, &[]);
        writer.to_json_pretty(&log)
    }
}

/// Formatter for AST tree output
pub struct TreeFormatter {
    styles: OutputStyles,
    indent_size: usize,
    show_spans: bool,
    show_text: bool,
    max_text_length: usize,
}

impl Default for TreeFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl TreeFormatter {
    pub fn new() -> Self {
        Self {
            styles: OutputStyles::default(),
            indent_size: 2,
            show_spans: true,
            show_text: true,
            max_text_length: 50,
        }
    }

    pub fn with_indent(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    pub fn with_spans(mut self, show: bool) -> Self {
        self.show_spans = show;
        self
    }

    pub fn with_text(mut self, show: bool) -> Self {
        self.show_text = show;
        self
    }

    /// Format a UAST node as a tree
    pub fn format_tree(&self, node: &UastNode) -> String {
        let mut output = String::new();
        self.format_node(&mut output, node, 0);
        output
    }

    fn format_node(&self, output: &mut String, node: &UastNode, depth: usize) {
        let indent = " ".repeat(depth * self.indent_size);

        // Node kind
        output.push_str(&indent);
        output.push_str(&format!(
            "{}",
            self.styles.node_kind.apply_to(node.kind.as_str())
        ));

        // Name if present
        if let Some(ref name) = node.name {
            output.push_str(&format!(
                " {}",
                self.styles.node_name.apply_to(name)
            ));
        }

        // Span information
        if self.show_spans {
            output.push_str(&format!(
                " {}",
                self.styles.location.apply_to(format!(
                    "[{}:{}-{}:{}]",
                    node.span.start_line,
                    node.span.start_column,
                    node.span.end_line,
                    node.span.end_column
                ))
            ));
        }

        // Text preview
        if self.show_text {
            if let Some(ref text) = node.text {
                let preview = if text.len() > self.max_text_length {
                    format!("{}...", &text[..self.max_text_length])
                } else {
                    text.clone()
                };
                // Escape newlines and special characters
                let preview = preview.replace('\n', "\\n").replace('\r', "\\r").replace('\t', "\\t");
                output.push_str(&format!(
                    " {}",
                    self.styles.line_number.apply_to(format!("\"{}\"", preview))
                ));
            }
        }

        output.push('\n');

        // Recurse into children
        for child in &node.children {
            self.format_node(output, child, depth + 1);
        }
    }
}

/// Format a match result for the 'run' command
pub struct MatchFormatter {
    styles: OutputStyles,
}

impl Default for MatchFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl MatchFormatter {
    pub fn new() -> Self {
        Self {
            styles: OutputStyles::default(),
        }
    }

    /// Format a single match as a grep-like line
    pub fn format_match(
        &self,
        file_path: &str,
        line: u32,
        column: u32,
        text: &str,
    ) -> String {
        format!(
            "{}:{}:{}  {}\n",
            self.styles.location.apply_to(file_path),
            self.styles.line_number.apply_to(line),
            self.styles.line_number.apply_to(column),
            text
        )
    }

    /// Format a count-only result
    pub fn format_count(&self, file_path: &str, count: u64) -> String {
        format!("{}:{}\n", file_path, count)
    }
}

/// Print a summary of scan results
pub fn print_summary(
    results: &[ScanResult],
    files_scanned: usize,
    elapsed: std::time::Duration,
) {
    let styles = OutputStyles::default();

    let error_count = results.iter().filter(|r| r.severity == Severity::Error).count();
    let warning_count = results.iter().filter(|r| r.severity == Severity::Warning).count();
    let info_count = results.iter().filter(|r| r.severity == Severity::Info).count();
    let hint_count = results.iter().filter(|r| r.severity == Severity::Hint).count();

    eprintln!();
    eprintln!("{}", style("Summary").bold().underlined());
    eprintln!("Files scanned: {}", files_scanned);
    eprintln!("Time elapsed:  {:.2?}", elapsed);
    eprintln!();

    if results.is_empty() {
        eprintln!("{}", style("No issues found").green());
    } else {
        eprintln!("Issues found:  {}", results.len());
        if error_count > 0 {
            eprintln!("  {} errors", styles.error.apply_to(error_count));
        }
        if warning_count > 0 {
            eprintln!("  {} warnings", styles.warning.apply_to(warning_count));
        }
        if info_count > 0 {
            eprintln!("  {} info", styles.info.apply_to(info_count));
        }
        if hint_count > 0 {
            eprintln!("  {} hints", styles.hint.apply_to(hint_count));
        }
    }
}

/// Write output to stdout, handling errors gracefully
pub fn write_stdout(content: &str) -> io::Result<()> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    write!(handle, "{}", content)?;
    handle.flush()
}

/// Write output to stderr, handling errors gracefully
pub fn write_stderr(content: &str) -> io::Result<()> {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    write!(handle, "{}", content)?;
    handle.flush()
}
