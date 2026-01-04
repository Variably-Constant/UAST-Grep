//! Query compiler for UAST patterns.
//!
//! This module compiles UAST patterns into tree-sitter queries, enabling
//! high-performance native matching using tree-sitter's query engine.

use super::pattern::{MetavarQuantifier, Pattern, PatternNode};
use crate::uast::mappings::get_native_types_for_uast;
use std::error::Error;
use std::fmt;

/// Error type for query compilation failures.
#[derive(Debug, Clone)]
pub struct QueryCompilationError {
    pub message: String,
    pub pattern: String,
}

impl QueryCompilationError {
    pub fn new(message: impl Into<String>, pattern: impl Into<String>) -> Self {
        QueryCompilationError {
            message: message.into(),
            pattern: pattern.into(),
        }
    }
}

impl fmt::Display for QueryCompilationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Query compilation error: {} (pattern: {})",
            self.message, self.pattern
        )
    }
}

impl Error for QueryCompilationError {}

/// Compile a UAST pattern to a tree-sitter S-expression query.
///
/// # Arguments
///
/// * `pattern` - The pattern to compile
///
/// # Returns
///
/// A tree-sitter query string on success, or `QueryCompilationError` on failure.
///
/// # Examples
///
/// ```text
/// FunctionDeclaration -> (function_item) @match  // for Rust
/// FunctionDeclaration -> (function_definition) @match  // for Python
/// ```
pub fn compile_to_tree_sitter_query(pattern: &Pattern) -> Result<String, QueryCompilationError> {
    let mut capture_counter = 0;
    let query = compile_node(&pattern.root, &pattern.language, &mut capture_counter)?;

    if query.is_empty() {
        return Err(QueryCompilationError::new(
            "Pattern compiled to empty query",
            &pattern.source_text,
        ));
    }

    Ok(query)
}

/// Compile a pattern node to a tree-sitter query fragment.
fn compile_node(
    node: &PatternNode,
    language: &str,
    capture_counter: &mut usize,
) -> Result<String, QueryCompilationError> {
    match node {
        PatternNode::Kind(kind) => {
            // Get native tree-sitter types for this UAST kind
            let native_types = get_native_types_for_uast(kind.as_str(), language);

            if native_types.is_empty() {
                // Fall back to using the UAST kind name as-is (might work for some languages)
                return Ok(format!("({}) @match", kind.as_str()));
            }

            if native_types.len() == 1 {
                // Single native type - simple case
                Ok(format!("({}) @match", native_types[0]))
            } else {
                // Multiple native types - create alternation
                // Tree-sitter doesn't support OR directly, so we generate multiple patterns
                // The caller should run each pattern separately
                // For now, just use the first one and log a warning
                Ok(format!("({}) @match", native_types[0]))
            }
        }

        PatternNode::Native(native_pattern) => {
            // Already a native pattern - use as-is if it's an S-expression
            if native_pattern.starts_with('(') {
                // It's an S-expression, ensure it has a capture
                if native_pattern.contains('@') {
                    Ok(native_pattern.clone())
                } else {
                    // Add a capture
                    let trimmed = native_pattern.trim_end_matches(')');
                    Ok(format!("{}) @match", trimmed))
                }
            } else {
                // It's a simple type name
                Ok(format!("({}) @match", native_pattern))
            }
        }

        PatternNode::Wildcard => {
            // Wildcard matches any node - use (_) in tree-sitter
            Ok("(_) @match".to_string())
        }

        PatternNode::Metavar {
            name,
            quantifier,
            is_anonymous,
        } => {
            // Metavariables become captures
            let capture_name = if *is_anonymous {
                *capture_counter += 1;
                format!("_anon{}", capture_counter)
            } else {
                name.clone()
            };

            // For multi-match metavars, we need to handle differently
            match quantifier {
                MetavarQuantifier::Single => Ok(format!("(_) @{}", capture_name)),
                MetavarQuantifier::OneOrMore | MetavarQuantifier::ZeroOrMore => {
                    // Tree-sitter uses + for one-or-more and * for zero-or-more
                    // but these are for repetition in patterns, not captures
                    // For now, treat as a single capture
                    Ok(format!("(_) @{}", capture_name))
                }
            }
        }

        PatternNode::Sequence(patterns) => {
            // Compile each pattern in the sequence
            let mut parts = Vec::new();
            for p in patterns {
                parts.push(compile_node(p, language, capture_counter)?);
            }
            // Sequences in tree-sitter are just space-separated
            Ok(parts.join(" "))
        }

        PatternNode::AnyOf(alternatives) => {
            // Tree-sitter doesn't have OR in queries, but we can use alternation in patterns
            // For now, just compile the first alternative
            if let Some(first) = alternatives.first() {
                compile_node(first, language, capture_counter)
            } else {
                Err(QueryCompilationError::new(
                    "Empty alternatives",
                    "AnyOf([])",
                ))
            }
        }

        PatternNode::AllOf(patterns) => {
            // All patterns must match - this is implicit in tree-sitter
            // Just compile as a sequence
            let mut parts = Vec::new();
            for p in patterns {
                parts.push(compile_node(p, language, capture_counter)?);
            }
            Ok(parts.join(" "))
        }

        PatternNode::Not(_inner) => {
            // Tree-sitter supports #not-match? predicates
            // For now, this is not fully supported
            Err(QueryCompilationError::new(
                "Negation patterns not yet supported in query compilation",
                "Not(...)",
            ))
        }

        PatternNode::Structural {
            kind,
            properties,
            children,
        } => {
            // Build a structural query
            let mut query = String::new();
            query.push('(');

            // Add the node type
            if let Some(k) = kind {
                let native_types = get_native_types_for_uast(k.as_str(), language);
                if let Some(native) = native_types.first() {
                    query.push_str(native);
                } else {
                    query.push_str(k.as_str());
                }
            } else {
                query.push('_');
            }

            // Add field patterns for properties
            for (prop_name, prop_pattern) in properties {
                let field_name = to_snake_case(prop_name);
                let prop_query = compile_node(prop_pattern, language, capture_counter)?;
                query.push_str(&format!(" {}: {}", field_name, prop_query));
            }

            // Add children patterns
            for child in children {
                let child_query = compile_node(child, language, capture_counter)?;
                query.push(' ');
                query.push_str(&child_query);
            }

            query.push_str(") @match");
            Ok(query)
        }

        PatternNode::Literal(_lit) => {
            // Literals require predicates in tree-sitter
            // For now, match any node and the caller can filter
            Ok("(_) @match".to_string())
        }
    }
}

/// Convert PascalCase or camelCase to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 5);
    let mut prev_was_upper = false;

    for (i, c) in s.chars().enumerate() {
        if c.is_ascii_uppercase() {
            if i > 0 && !prev_was_upper {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
            prev_was_upper = true;
        } else {
            result.push(c);
            prev_was_upper = false;
        }
    }

    result
}

/// Get all native tree-sitter queries for a UAST pattern.
///
/// This handles cases where a UAST type maps to multiple native types
/// by returning multiple queries.
pub fn get_all_native_queries(pattern: &Pattern) -> Result<Vec<String>, QueryCompilationError> {
    if let PatternNode::Kind(kind) = &pattern.root {
        let native_types = get_native_types_for_uast(kind.as_str(), &pattern.language);

        if native_types.is_empty() {
            // Fall back to single query with UAST name
            return Ok(vec![format!("({}) @match", kind.as_str())]);
        }

        return Ok(native_types
            .into_iter()
            .map(|t| format!("({}) @match", t))
            .collect());
    }

    // For other pattern types, compile normally
    Ok(vec![compile_to_tree_sitter_query(pattern)?])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::uast::schema::UastKind;

    #[test]
    fn test_compile_kind_pattern_rust() {
        let pattern = Pattern::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            "FunctionDeclaration".to_string(),
            "rust".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert!(query.contains("function_item"));
        assert!(query.contains("@match"));
    }

    #[test]
    fn test_compile_kind_pattern_python() {
        let pattern = Pattern::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            "FunctionDeclaration".to_string(),
            "python".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert!(query.contains("function_definition"));
        assert!(query.contains("@match"));
    }

    #[test]
    fn test_compile_native_pattern() {
        let pattern = Pattern::new(
            PatternNode::Native("function_item".to_string()),
            "function_item".to_string(),
            "rust".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert_eq!(query, "(function_item) @match");
    }

    #[test]
    fn test_compile_native_s_expression() {
        let pattern = Pattern::new(
            PatternNode::Native("(function_item name: (identifier))".to_string()),
            "(function_item name: (identifier))".to_string(),
            "rust".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert!(query.contains("function_item"));
        assert!(query.contains("@match"));
    }

    #[test]
    fn test_compile_wildcard() {
        let pattern = Pattern::new(
            PatternNode::Wildcard,
            "*".to_string(),
            "rust".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert_eq!(query, "(_) @match");
    }

    #[test]
    fn test_compile_metavar() {
        let pattern = Pattern::new(
            PatternNode::metavar("NAME", MetavarQuantifier::Single),
            "$NAME".to_string(),
            "rust".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert!(query.contains("@NAME"));
    }

    #[test]
    fn test_get_all_native_queries() {
        let pattern = Pattern::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            "FunctionDeclaration".to_string(),
            "rust".to_string(),
        );

        let queries = get_all_native_queries(&pattern).unwrap();
        assert!(!queries.is_empty());
        for q in &queries {
            assert!(q.contains("@match"));
        }
    }

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("FunctionDeclaration"), "function_declaration");
        assert_eq!(to_snake_case("name"), "name");
        assert_eq!(to_snake_case("NodeKind"), "node_kind");
        // All caps like XML become xmlparser (no separators between consecutive capitals)
        assert_eq!(to_snake_case("XMLParser"), "xmlparser");
    }

    #[test]
    fn test_compile_structural_pattern() {
        let pattern = Pattern::new(
            PatternNode::structural(
                Some(UastKind::FunctionDeclaration),
                vec![("Name".to_string(), PatternNode::metavar("FUNC_NAME", MetavarQuantifier::Single))],
                vec![],
            ),
            "FunctionDeclaration { Name: $FUNC_NAME }".to_string(),
            "rust".to_string(),
        );

        let query = compile_to_tree_sitter_query(&pattern).unwrap();
        assert!(query.contains("function_item"));
        assert!(query.contains("name:"));
        assert!(query.contains("@FUNC_NAME"));
    }
}
