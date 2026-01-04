//! Pattern string parser for UAST pattern matching.
//!
//! This module parses pattern strings into Pattern AST nodes, handling:
//! - UAST patterns (PascalCase node kinds)
//! - Native patterns (snake_case or S-expressions)
//! - Metavariables with three supported prefixes:
//!   - `§VAR` (section symbol, recommended)
//!   - `∀VAR` (forall symbol, mathematical)
//!   - `$VAR` (dollar, use single quotes in PowerShell)
//! - Wildcards (*)

use super::pattern::{LiteralPattern, MetavarQuantifier, Pattern, PatternNode, METAVAR_PREFIXES};
use crate::uast::mappings::is_uast_pattern;
use crate::uast::schema::UastKind;
use std::error::Error;
use std::fmt;

/// Error type for pattern parsing failures.
#[derive(Debug, Clone)]
pub struct PatternParseError {
    pub message: String,
    pub position: Option<usize>,
}

impl PatternParseError {
    pub fn new(message: impl Into<String>) -> Self {
        PatternParseError {
            message: message.into(),
            position: None,
        }
    }

    pub fn at_position(message: impl Into<String>, position: usize) -> Self {
        PatternParseError {
            message: message.into(),
            position: Some(position),
        }
    }
}

impl fmt::Display for PatternParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(pos) = self.position {
            write!(f, "Pattern parse error at position {}: {}", pos, self.message)
        } else {
            write!(f, "Pattern parse error: {}", self.message)
        }
    }
}

impl Error for PatternParseError {}

/// Parse a simple pattern string into a Pattern.
///
/// This function handles single-token patterns like:
/// - `FunctionDeclaration` (UAST kind)
/// - `function_item` (native tree-sitter type)
/// - `$NAME` / `$$NAME` / `$$$NAME` (metavariables)
/// - `*` (wildcard)
/// - `(function_item ...)` (S-expression)
///
/// # Arguments
///
/// * `source` - The pattern string to parse
/// * `language` - The target language (e.g., "rust", "python")
///
/// # Returns
///
/// A `Pattern` on success, or `PatternParseError` on failure.
pub fn parse_simple_pattern(source: &str, language: &str) -> Result<Pattern, PatternParseError> {
    let source = source.trim();

    if source.is_empty() {
        return Err(PatternParseError::new("Empty pattern"));
    }

    let root = parse_simple_pattern_node(source)?;
    Ok(Pattern::new(root, source.to_string(), language.to_string()))
}

/// Parse a pattern string that may contain structural patterns.
///
/// This is a more advanced parser that handles complex patterns including:
/// - Structural patterns with properties and children
/// - Nested patterns
/// - Alternative patterns (|)
/// - Conjunction patterns (&)
/// - Negation patterns (!)
///
/// # Arguments
///
/// * `source` - The pattern string to parse
/// * `language` - The target language
///
/// # Returns
///
/// A `Pattern` on success, or `PatternParseError` on failure.
pub fn parse_pattern(source: &str, language: &str) -> Result<Pattern, PatternParseError> {
    let source = source.trim();

    if source.is_empty() {
        return Err(PatternParseError::new("Empty pattern"));
    }

    // For now, delegate to simple parser
    // TODO: Implement full structural pattern parser when needed
    parse_simple_pattern(source, language)
}

/// Parse a simple pattern node (single token).
fn parse_simple_pattern_node(source: &str) -> Result<PatternNode, PatternParseError> {
    let source = source.trim();

    // Check for metavariable prefixes
    if let Some(node) = try_parse_metavar(source)? {
        return Ok(node);
    }

    // Check for wildcard
    if source == "*" || source == "..." {
        return Ok(PatternNode::Wildcard);
    }

    // Check for S-expression (native tree-sitter query)
    if source.starts_with('(') {
        return Ok(PatternNode::Native(source.to_string()));
    }

    // Check for string literal
    if (source.starts_with('"') && source.ends_with('"'))
        || (source.starts_with('\'') && source.ends_with('\''))
    {
        let inner = &source[1..source.len() - 1];
        return Ok(PatternNode::Literal(LiteralPattern::String(inner.to_string())));
    }

    // Check for numeric literals
    if let Ok(n) = source.parse::<i64>() {
        return Ok(PatternNode::Literal(LiteralPattern::Integer(n)));
    }
    if let Ok(f) = source.parse::<f64>() {
        return Ok(PatternNode::Literal(LiteralPattern::Float(f)));
    }

    // Check for boolean literals
    if source.eq_ignore_ascii_case("true") {
        return Ok(PatternNode::Literal(LiteralPattern::Boolean(true)));
    }
    if source.eq_ignore_ascii_case("false") {
        return Ok(PatternNode::Literal(LiteralPattern::Boolean(false)));
    }

    // Check for null literal
    if source.eq_ignore_ascii_case("null") || source.eq_ignore_ascii_case("nil") {
        return Ok(PatternNode::Literal(LiteralPattern::Null));
    }

    // Check if this looks like a UAST pattern (PascalCase)
    if is_uast_pattern(source) {
        let kind = UastKind::from_str(source);
        if kind != UastKind::Unknown {
            return Ok(PatternNode::Kind(kind));
        }
        // If it looks like UAST but isn't a known kind, treat as native
        return Ok(PatternNode::Native(source.to_string()));
    }

    // Default: treat as native tree-sitter type
    Ok(PatternNode::Native(source.to_string()))
}

/// Try to parse a metavariable pattern.
fn try_parse_metavar(source: &str) -> Result<Option<PatternNode>, PatternParseError> {
    let first_char = match source.chars().next() {
        Some(c) => c,
        None => return Ok(None),
    };

    // Check for metavariable prefix (§, ∀, or $)
    if !METAVAR_PREFIXES.contains(&first_char) {
        return Ok(None);
    }

    // Count prefix characters to determine quantifier
    let mut prefix_count = 0;
    let mut chars = source.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c == first_char {
            prefix_count += 1;
            chars.next();
        } else {
            break;
        }
    }

    // Collect the variable name
    let name: String = chars
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();

    if name.is_empty() {
        // Just prefix(es) without a name - not a valid metavar
        return Err(PatternParseError::new(
            "Metavariable prefix without name",
        ));
    }

    let quantifier = match prefix_count {
        1 => MetavarQuantifier::Single,
        2 => MetavarQuantifier::OneOrMore,
        _ => MetavarQuantifier::ZeroOrMore, // 3 or more
    };

    let is_anonymous = name == "_";

    Ok(Some(PatternNode::Metavar {
        name,
        quantifier,
        is_anonymous,
    }))
}

/// Preprocess a pattern string to replace metavariables with placeholders.
///
/// This is used when the pattern needs to be parsed as code in the target language
/// (like the C# implementation does).
///
/// # Arguments
///
/// * `source` - The pattern source text
///
/// # Returns
///
/// A tuple of (preprocessed source, set of metavariable names).
pub fn preprocess_metavars(source: &str) -> (String, Vec<(String, MetavarQuantifier)>) {
    const PLACEHOLDER: &str = "__UAST_METAVAR_";

    let mut result = String::with_capacity(source.len());
    let mut metavars = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(c) = chars.next() {
        if METAVAR_PREFIXES.contains(&c) {
            // Count prefix characters
            let mut prefix_count = 1;
            while let Some(&next) = chars.peek() {
                if next == c {
                    prefix_count += 1;
                    chars.next();
                } else {
                    break;
                }
            }

            // Collect variable name
            let mut name = String::new();
            while let Some(&next) = chars.peek() {
                if next.is_alphanumeric() || next == '_' {
                    name.push(next);
                    chars.next();
                } else {
                    break;
                }
            }

            if !name.is_empty() {
                let quantifier = match prefix_count {
                    1 => MetavarQuantifier::Single,
                    2 => MetavarQuantifier::OneOrMore,
                    _ => MetavarQuantifier::ZeroOrMore,
                };

                metavars.push((name.clone(), quantifier));

                // Create placeholder identifier
                let prefix = if quantifier.is_multiple() { "MULTI_" } else { "" };
                result.push_str(&format!("{}{}{}__", PLACEHOLDER, prefix, name));
            } else {
                // Not a valid metavar, keep the prefix chars
                for _ in 0..prefix_count {
                    result.push(c);
                }
            }
        } else {
            result.push(c);
        }
    }

    (result, metavars)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_uast_kind() {
        let pattern = parse_simple_pattern("FunctionDeclaration", "rust").unwrap();
        assert!(matches!(
            pattern.root,
            PatternNode::Kind(UastKind::FunctionDeclaration)
        ));
        assert!(!pattern.is_native);
    }

    #[test]
    fn test_parse_simple_metavar_dollar() {
        let pattern = parse_simple_pattern("$NAME", "rust").unwrap();
        if let PatternNode::Metavar {
            name,
            quantifier,
            is_anonymous,
        } = &pattern.root
        {
            assert_eq!(name, "NAME");
            assert_eq!(*quantifier, MetavarQuantifier::Single);
            assert!(!is_anonymous);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_simple_metavar_forall() {
        let pattern = parse_simple_pattern("\u{2200}X", "rust").unwrap();
        if let PatternNode::Metavar {
            name, quantifier, ..
        } = &pattern.root
        {
            assert_eq!(name, "X");
            assert_eq!(*quantifier, MetavarQuantifier::Single);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_simple_metavar_section() {
        // § is the primary/recommended prefix
        let pattern = parse_simple_pattern("\u{00A7}NAME", "rust").unwrap();
        if let PatternNode::Metavar {
            name, quantifier, ..
        } = &pattern.root
        {
            assert_eq!(name, "NAME");
            assert_eq!(*quantifier, MetavarQuantifier::Single);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_metavar_section_zero_or_more() {
        // §§§ = zero or more
        let pattern = parse_simple_pattern("\u{00A7}\u{00A7}\u{00A7}ARGS", "rust").unwrap();
        if let PatternNode::Metavar { quantifier, .. } = &pattern.root {
            assert_eq!(*quantifier, MetavarQuantifier::ZeroOrMore);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_metavar_one_or_more() {
        let pattern = parse_simple_pattern("$$ARGS", "rust").unwrap();
        if let PatternNode::Metavar { quantifier, .. } = &pattern.root {
            assert_eq!(*quantifier, MetavarQuantifier::OneOrMore);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_metavar_zero_or_more() {
        let pattern = parse_simple_pattern("$$$ITEMS", "rust").unwrap();
        if let PatternNode::Metavar { quantifier, .. } = &pattern.root {
            assert_eq!(*quantifier, MetavarQuantifier::ZeroOrMore);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_anonymous_metavar() {
        let pattern = parse_simple_pattern("$_", "rust").unwrap();
        if let PatternNode::Metavar {
            name, is_anonymous, ..
        } = &pattern.root
        {
            assert_eq!(name, "_");
            assert!(is_anonymous);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_parse_wildcard_star() {
        let pattern = parse_simple_pattern("*", "rust").unwrap();
        assert!(matches!(pattern.root, PatternNode::Wildcard));
    }

    #[test]
    fn test_parse_wildcard_ellipsis() {
        let pattern = parse_simple_pattern("...", "rust").unwrap();
        assert!(matches!(pattern.root, PatternNode::Wildcard));
    }

    #[test]
    fn test_parse_native_snake_case() {
        let pattern = parse_simple_pattern("function_item", "rust").unwrap();
        if let PatternNode::Native(s) = &pattern.root {
            assert_eq!(s, "function_item");
        } else {
            panic!("Expected Native");
        }
        assert!(pattern.is_native);
    }

    #[test]
    fn test_parse_s_expression() {
        let pattern =
            parse_simple_pattern("(function_item name: (identifier) @name)", "rust").unwrap();
        if let PatternNode::Native(s) = &pattern.root {
            assert!(s.starts_with('('));
        } else {
            panic!("Expected Native");
        }
    }

    #[test]
    fn test_parse_string_literal() {
        let pattern = parse_simple_pattern("\"hello\"", "rust").unwrap();
        if let PatternNode::Literal(LiteralPattern::String(s)) = &pattern.root {
            assert_eq!(s, "hello");
        } else {
            panic!("Expected String Literal");
        }
    }

    #[test]
    fn test_parse_integer_literal() {
        let pattern = parse_simple_pattern("42", "rust").unwrap();
        if let PatternNode::Literal(LiteralPattern::Integer(n)) = &pattern.root {
            assert_eq!(*n, 42);
        } else {
            panic!("Expected Integer Literal");
        }
    }

    #[test]
    fn test_parse_empty_pattern_error() {
        let result = parse_simple_pattern("", "rust");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_metavar_no_name_error() {
        let result = parse_simple_pattern("$", "rust");
        assert!(result.is_err());
    }

    #[test]
    fn test_preprocess_metavars() {
        let (processed, vars) = preprocess_metavars("func($ARG1, $$$ARGS)");
        assert!(processed.contains("__UAST_METAVAR_ARG1__"));
        assert!(processed.contains("__UAST_METAVAR_MULTI_ARGS__"));
        assert_eq!(vars.len(), 2);
        assert_eq!(vars[0], ("ARG1".to_string(), MetavarQuantifier::Single));
        assert_eq!(vars[1], ("ARGS".to_string(), MetavarQuantifier::ZeroOrMore));
    }
}
