//! Query compilation and execution using tree-sitter's query language.
//!
//! This module provides high-performance query execution for pattern matching
//! in parsed syntax trees. Queries use tree-sitter's S-expression syntax.
//!
//! # Usage
//!
//! ```ignore
//! // Execute a query and collect all matches
//! let matches = execute_collect(&tree, "(function_declaration name: (identifier) @name)")?;
//! for m in matches {
//!     for capture in m.captures {
//!         println!("{}: {}", capture.name, capture.text);
//!     }
//! }
//!
//! // Execute with a callback for streaming processing
//! execute(&tree, pattern, |match_| {
//!     // Process each match
//!     true // Return false to stop iteration
//! })?;
//! ```

use crate::error::Error;
use crate::ffi::{UastCapture, UastMatch, UastNode};
use crate::parser::ParsedTree;
use crate::tree::node_to_ffi;
use streaming_iterator::StreamingIterator;
use tree_sitter::{Query, QueryCursor};

// ============================================================================
// Compiled Query
// ============================================================================

/// Compiled query ready for execution.
///
/// Queries are compiled once and can be executed multiple times against
/// different trees of the same language.
pub struct CompiledQuery {
    /// The underlying tree-sitter query
    query: Query,
    /// Cache of capture names for efficient lookup
    capture_names: Vec<String>,
}

impl CompiledQuery {
    /// Compile a query pattern for the given language.
    ///
    /// # Arguments
    ///
    /// * `tree` - A parsed tree (used to get the language)
    /// * `pattern` - The query pattern in tree-sitter S-expression syntax
    ///
    /// # Errors
    ///
    /// Returns an error if the query pattern is invalid.
    pub fn new(tree: &ParsedTree, pattern: &str) -> Result<Self, Error> {
        let query = Query::new(tree.language(), pattern).map_err(|e| {
            Error::query_failed(
                format!("{:?}", e.kind),
                e.row as u32,
                e.column as u32,
            )
        })?;

        let capture_names = query
            .capture_names()
            .iter()
            .map(|s| s.to_string())
            .collect();

        Ok(Self {
            query,
            capture_names,
        })
    }

    /// Get the capture names defined in this query.
    pub fn capture_names(&self) -> &[String] {
        &self.capture_names
    }

    /// Get the number of patterns in this query.
    pub fn pattern_count(&self) -> usize {
        self.query.pattern_count()
    }

    /// Get the underlying tree-sitter query.
    pub fn inner(&self) -> &Query {
        &self.query
    }
}

// ============================================================================
// High-Level Match and Capture Types
// ============================================================================

/// High-level match result with owned data.
///
/// This struct owns all its data and is safe to return from functions
/// and store in collections.
#[derive(Debug, Clone)]
pub struct Match {
    /// Index of the pattern that matched (for multi-pattern queries)
    pub pattern_index: u32,
    /// All captures from this match
    pub captures: Vec<Capture>,
}

/// High-level capture result with owned data.
///
/// Contains the capture name, the text that was captured, and byte positions.
#[derive(Debug, Clone)]
pub struct Capture {
    /// The capture name (from @name in the query)
    pub name: String,
    /// The captured text
    pub text: String,
    /// Start byte offset in source
    pub start_byte: u32,
    /// End byte offset in source
    pub end_byte: u32,
    /// Start row (0-indexed)
    pub start_row: u32,
    /// Start column (0-indexed)
    pub start_column: u32,
    /// End row (0-indexed)
    pub end_row: u32,
    /// End column (0-indexed)
    pub end_column: u32,
}

// ============================================================================
// Query Execution Functions
// ============================================================================

/// Execute a query pattern on a tree, calling a callback for each match.
///
/// The callback receives each match and can return `false` to stop iteration.
///
/// # Arguments
///
/// * `tree` - The parsed tree to query
/// * `pattern` - The query pattern in tree-sitter S-expression syntax
/// * `callback` - Function called for each match
///
/// # Errors
///
/// Returns an error if the query pattern is invalid.
pub fn execute<F>(tree: &ParsedTree, pattern: &str, callback: F) -> Result<(), Error>
where
    F: FnMut(&Match) -> bool,
{
    let compiled = CompiledQuery::new(tree, pattern)?;
    execute_compiled(tree, &compiled, callback)
}

/// Execute a pre-compiled query on a tree.
///
/// This is more efficient than `execute` when running the same query
/// multiple times.
pub fn execute_compiled<F>(tree: &ParsedTree, query: &CompiledQuery, mut callback: F) -> Result<(), Error>
where
    F: FnMut(&Match) -> bool,
{
    let mut cursor = QueryCursor::new();
    let source = tree.source();

    let mut matches = cursor.matches(&query.query, tree.root_node(), source.as_bytes());
    while let Some(m) = matches.next() {
        // Build captures
        let captures: Vec<Capture> = m
            .captures
            .iter()
            .map(|c| {
                let start = c.node.start_position();
                let end = c.node.end_position();
                Capture {
                    name: query.capture_names[c.index as usize].clone(),
                    text: tree.node_text(c.node).to_string(),
                    start_byte: c.node.start_byte() as u32,
                    end_byte: c.node.end_byte() as u32,
                    start_row: start.row as u32,
                    start_column: start.column as u32,
                    end_row: end.row as u32,
                    end_column: end.column as u32,
                }
            })
            .collect();

        let match_result = Match {
            pattern_index: m.pattern_index as u32,
            captures,
        };

        if !callback(&match_result) {
            break;
        }
    }

    Ok(())
}

/// Execute a query and collect all matches.
///
/// This is a convenience function for cases where you want all matches
/// as a vector rather than processing them via callback.
///
/// # Arguments
///
/// * `tree` - The parsed tree to query
/// * `pattern` - The query pattern in tree-sitter S-expression syntax
///
/// # Errors
///
/// Returns an error if the query pattern is invalid.
pub fn execute_collect(tree: &ParsedTree, pattern: &str) -> Result<Vec<Match>, Error> {
    let mut matches = Vec::new();
    execute(tree, pattern, |m| {
        matches.push(m.clone());
        true
    })?;
    Ok(matches)
}

/// Execute a pre-compiled query and collect all matches.
pub fn execute_compiled_collect(tree: &ParsedTree, query: &CompiledQuery) -> Result<Vec<Match>, Error> {
    let mut matches = Vec::new();
    execute_compiled(tree, query, |m| {
        matches.push(m.clone());
        true
    })?;
    Ok(matches)
}

// ============================================================================
// FFI Support Functions
// ============================================================================

/// Execute a query with FFI-compatible callback.
///
/// This is used by the FFI layer to execute queries with C-compatible
/// callback signatures.
pub fn execute_ffi<F>(tree: &ParsedTree, pattern: &str, mut callback: F) -> Result<(), Error>
where
    F: FnMut(&UastMatch) -> bool,
{
    let compiled = CompiledQuery::new(tree, pattern)?;
    let mut cursor = QueryCursor::new();
    let source = tree.source();

    let mut matches = cursor.matches(&compiled.query, tree.root_node(), source.as_bytes());
    while let Some(m) = matches.next() {
        // Build FFI captures
        // We need to keep the names alive for the duration of the callback
        let capture_data: Vec<(String, UastNode)> = m
            .captures
            .iter()
            .map(|c| {
                let name = compiled.capture_names[c.index as usize].clone();
                let node = node_to_ffi(c.node, None);
                (name, node)
            })
            .collect();

        // Create FFI capture structs pointing to our data
        let captures: Vec<UastCapture> = capture_data
            .iter()
            .map(|(name, node)| UastCapture {
                name: name.as_ptr() as *const i8,
                name_len: name.len() as u32,
                node: node.clone(),
            })
            .collect();

        let ffi_match = UastMatch {
            pattern_index: m.pattern_index as u32,
            captures: captures.as_ptr(),
            capture_count: captures.len() as u32,
        };

        if !callback(&ffi_match) {
            break;
        }
    }

    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // NOTE: These tests require a language to be registered. They are marked
    // as ignored for standalone unit tests. Run integration tests with a
    // registered language for full coverage.

    #[test]
    fn test_match_default() {
        let m = Match {
            pattern_index: 0,
            captures: vec![],
        };
        assert_eq!(m.pattern_index, 0);
        assert!(m.captures.is_empty());
    }

    #[test]
    fn test_capture_clone() {
        let capture = Capture {
            name: "test".to_string(),
            text: "hello".to_string(),
            start_byte: 0,
            end_byte: 5,
            start_row: 0,
            start_column: 0,
            end_row: 0,
            end_column: 5,
        };

        let cloned = capture.clone();
        assert_eq!(cloned.name, "test");
        assert_eq!(cloned.text, "hello");
        assert_eq!(cloned.start_byte, 0);
        assert_eq!(cloned.end_byte, 5);
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_query_function_declarations() {
        use crate::parser::Parser;

        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser
            .parse("function foo() {} function bar(x) { return x; }")
            .unwrap();

        let matches = execute_collect(&tree, "(function_declaration name: (identifier) @name)").unwrap();

        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].captures[0].text, "foo");
        assert_eq!(matches[1].captures[0].text, "bar");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_query_with_predicates() {
        use crate::parser::Parser;

        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser
            .parse("const x = 1; let y = 2; var z = 3;")
            .unwrap();

        // Match only 'const' declarations
        let pattern = r#"
            (lexical_declaration
                kind: "const"
                (variable_declarator name: (identifier) @name))
        "#;

        let matches = execute_collect(&tree, pattern).unwrap();
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].captures[0].text, "x");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_query_early_termination() {
        use crate::parser::Parser;

        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser
            .parse("let a = 1; let b = 2; let c = 3;")
            .unwrap();

        let mut count = 0;
        execute(&tree, "(lexical_declaration) @decl", |_| {
            count += 1;
            count < 2 // Stop after 2 matches
        }).unwrap();

        assert_eq!(count, 2);
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_compiled_query_reuse() {
        use crate::parser::Parser;

        let mut parser = Parser::new("javascript").unwrap();
        let tree1 = parser.parse("function foo() {}").unwrap();
        let tree2 = parser.parse("function bar() {}").unwrap();

        let query = CompiledQuery::new(&tree1, "(function_declaration name: (identifier) @name)").unwrap();

        let matches1 = execute_compiled_collect(&tree1, &query).unwrap();
        let matches2 = execute_compiled_collect(&tree2, &query).unwrap();

        assert_eq!(matches1[0].captures[0].text, "foo");
        assert_eq!(matches2[0].captures[0].text, "bar");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_invalid_query_pattern() {
        use crate::parser::Parser;

        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let x = 1;").unwrap();

        let result = execute_collect(&tree, "(invalid_node_type_xyz");
        assert!(result.is_err());
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_capture_names() {
        use crate::parser::Parser;

        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("function test() {}").unwrap();

        let query = CompiledQuery::new(
            &tree,
            "(function_declaration name: (identifier) @func_name body: (statement_block) @body)"
        ).unwrap();

        let names = query.capture_names();
        assert!(names.contains(&"func_name".to_string()));
        assert!(names.contains(&"body".to_string()));
    }
}
