//! Auto-fix support for UAST-Grep rules.
//!
//! This module provides functionality to apply fixes from rules to source code.

use crate::uast::schema::SourceSpan;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

/// Represents a fix to be applied to source code.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Fix {
    /// The span of text to replace.
    pub span: SourceSpan,

    /// The replacement text.
    pub replacement: String,
}

impl Fix {
    /// Create a new fix.
    pub fn new(span: SourceSpan, replacement: impl Into<String>) -> Self {
        Fix {
            span,
            replacement: replacement.into(),
        }
    }

    /// Create a fix that inserts text before a position.
    pub fn insert_before(line: u32, column: u32, offset: u32, text: impl Into<String>) -> Self {
        let span = SourceSpan::new(line, column, line, column, offset, offset);
        Fix {
            span,
            replacement: text.into(),
        }
    }

    /// Create a fix that inserts text after a position.
    pub fn insert_after(line: u32, column: u32, offset: u32, text: impl Into<String>) -> Self {
        let span = SourceSpan::new(line, column, line, column, offset, offset);
        Fix {
            span,
            replacement: text.into(),
        }
    }

    /// Create a fix that deletes a span.
    pub fn delete(span: SourceSpan) -> Self {
        Fix {
            span,
            replacement: String::new(),
        }
    }
}

/// Result of applying fixes to source code.
#[derive(Debug, Clone)]
pub struct FixResult {
    /// The modified source code.
    pub source: String,
    /// Number of fixes applied.
    pub fixes_applied: usize,
    /// Fixes that couldn't be applied (overlapping, etc.).
    pub skipped_fixes: Vec<(Fix, String)>,
}

/// Apply a collection of fixes to source code.
///
/// Fixes are applied from end to start to maintain correct offsets.
/// Overlapping fixes are skipped with an error message.
///
/// # Arguments
///
/// * `source` - The original source code
/// * `fixes` - The fixes to apply
///
/// # Returns
///
/// A `FixResult` containing the modified source and statistics.
pub fn apply_fixes(source: &str, mut fixes: Vec<Fix>) -> FixResult {
    let mut result = source.to_string();
    let mut applied = 0;
    let mut skipped: Vec<(Fix, String)> = Vec::new();

    // Sort fixes by offset in reverse order (end to start)
    fixes.sort_by(|a, b| {
        match b.span.start_offset.cmp(&a.span.start_offset) {
            Ordering::Equal => b.span.end_offset.cmp(&a.span.end_offset),
            other => other,
        }
    });

    let mut last_start: Option<u32> = None;

    for fix in fixes {
        // Check for overlap with previously applied fix
        if let Some(prev_start) = last_start {
            if fix.span.end_offset > prev_start {
                skipped.push((fix.clone(), "Overlapping with another fix".to_string()));
                continue;
            }
        }

        // Validate offsets
        let start = fix.span.start_offset as usize;
        let end = fix.span.end_offset as usize;

        if start > result.len() || end > result.len() || start > end {
            skipped.push((fix.clone(), format!(
                "Invalid offset: start={}, end={}, source_len={}",
                start, end, result.len()
            )));
            continue;
        }

        // Apply the fix
        result.replace_range(start..end, &fix.replacement);
        applied += 1;
        last_start = Some(fix.span.start_offset);
    }

    FixResult {
        source: result,
        fixes_applied: applied,
        skipped_fixes: skipped,
    }
}

/// Apply a single fix to source code.
pub fn apply_fix(source: &str, fix: &Fix) -> Result<String, String> {
    let start = fix.span.start_offset as usize;
    let end = fix.span.end_offset as usize;

    if start > source.len() || end > source.len() || start > end {
        return Err(format!(
            "Invalid offset: start={}, end={}, source_len={}",
            start, end, source.len()
        ));
    }

    let mut result = source.to_string();
    result.replace_range(start..end, &fix.replacement);
    Ok(result)
}

/// Interpolate metavariable values into a fix template.
///
/// # Arguments
///
/// * `template` - The fix template with metavariable placeholders ($NAME or \u{2200}NAME)
/// * `captures` - Map of variable names to their captured text values
///
/// # Returns
///
/// The interpolated fix string.
pub fn interpolate_fix(
    template: &str,
    captures: &std::collections::HashMap<String, String>,
) -> String {
    let mut result = template.to_string();

    for (name, value) in captures {
        // Replace $NAME variants
        result = result.replace(&format!("${}", name), value);
        // Replace \u{2200}NAME variants
        result = result.replace(&format!("\u{2200}{}", name), value);
    }

    result
}

/// Compute line/column positions from byte offsets.
///
/// Useful when you need to convert offset-based fixes to line-based representation.
pub fn offset_to_line_column(source: &str, offset: usize) -> (u32, u32) {
    let mut line: u32 = 1;
    let mut column: u32 = 0;
    let mut current_offset = 0;

    for ch in source.chars() {
        if current_offset >= offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
        current_offset += ch.len_utf8();
    }

    (line, column)
}

/// Compute byte offset from line/column.
///
/// Lines are 1-indexed, columns are 0-indexed.
pub fn line_column_to_offset(source: &str, line: u32, column: u32) -> Option<usize> {
    let mut current_line: u32 = 1;
    let mut current_column: u32 = 0;
    let mut offset = 0;

    for ch in source.chars() {
        if current_line == line && current_column == column {
            return Some(offset);
        }

        if ch == '\n' {
            if current_line == line {
                // Column is past end of line
                return None;
            }
            current_line += 1;
            current_column = 0;
        } else {
            current_column += 1;
        }
        offset += ch.len_utf8();
    }

    // Handle position at end of file
    if current_line == line && current_column == column {
        return Some(offset);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fix_new() {
        let span = SourceSpan::new(1, 0, 1, 5, 0, 5);
        let fix = Fix::new(span, "hello");
        assert_eq!(fix.replacement, "hello");
    }

    #[test]
    fn test_fix_delete() {
        let span = SourceSpan::new(1, 0, 1, 5, 0, 5);
        let fix = Fix::delete(span);
        assert!(fix.replacement.is_empty());
    }

    #[test]
    fn test_apply_single_fix() {
        let source = "let x = foo";
        let span = SourceSpan::new(1, 8, 1, 11, 8, 11);
        let fix = Fix::new(span, "bar");

        let result = apply_fix(source, &fix).unwrap();
        assert_eq!(result, "let x = bar");
    }

    #[test]
    fn test_apply_multiple_fixes() {
        let source = "let x = foo; let y = bar";
        let fixes = vec![
            Fix::new(SourceSpan::new(1, 8, 1, 11, 8, 11), "AAA"),
            Fix::new(SourceSpan::new(1, 21, 1, 24, 21, 24), "BBB"),
        ];

        let result = apply_fixes(source, fixes);
        assert_eq!(result.source, "let x = AAA; let y = BBB");
        assert_eq!(result.fixes_applied, 2);
    }

    #[test]
    fn test_apply_overlapping_fixes() {
        let source = "abcdefghij";
        let fixes = vec![
            Fix::new(SourceSpan::new(1, 0, 1, 5, 0, 5), "XXX"),  // overlaps
            Fix::new(SourceSpan::new(1, 3, 1, 8, 3, 8), "YYY"),  // overlaps
        ];

        let result = apply_fixes(source, fixes);
        // One should be skipped due to overlap
        assert_eq!(result.fixes_applied, 1);
        assert_eq!(result.skipped_fixes.len(), 1);
    }

    #[test]
    fn test_interpolate_fix() {
        let mut captures = std::collections::HashMap::new();
        captures.insert("VAR".to_string(), "myVar".to_string());
        captures.insert("VALUE".to_string(), "42".to_string());

        let template = "const $VAR = $VALUE";
        let result = interpolate_fix(template, &captures);
        assert_eq!(result, "const myVar = 42");
    }

    #[test]
    fn test_offset_to_line_column() {
        let source = "line1\nline2\nline3";

        assert_eq!(offset_to_line_column(source, 0), (1, 0));
        assert_eq!(offset_to_line_column(source, 5), (1, 5));  // at newline
        assert_eq!(offset_to_line_column(source, 6), (2, 0));  // start of line2
        assert_eq!(offset_to_line_column(source, 12), (3, 0)); // start of line3
    }

    #[test]
    fn test_line_column_to_offset() {
        let source = "line1\nline2\nline3";

        assert_eq!(line_column_to_offset(source, 1, 0), Some(0));
        assert_eq!(line_column_to_offset(source, 2, 0), Some(6));
        assert_eq!(line_column_to_offset(source, 3, 0), Some(12));
        assert_eq!(line_column_to_offset(source, 1, 5), Some(5));  // at newline
    }

    #[test]
    fn test_fix_serialization() {
        let span = SourceSpan::new(1, 0, 1, 5, 0, 5);
        let fix = Fix::new(span, "test");

        let json = serde_json::to_string(&fix).unwrap();
        let deserialized: Fix = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.replacement, "test");
    }
}
