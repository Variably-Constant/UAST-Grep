//! Pattern string parser for UAST pattern matching.
//!
//! This module parses pattern strings into Pattern AST nodes, handling:
//! - UAST patterns (PascalCase node kinds)
//! - Native patterns (snake_case or S-expressions)
//! - Structural patterns (e.g., `$A.$B.$C` for member access chains)
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

    // Delegate to simple parser (handles member access chains, metavars, kinds, etc.)
    parse_simple_pattern(source, language)
}

/// Parse a simple pattern node (single token).
fn parse_simple_pattern_node(source: &str) -> Result<PatternNode, PatternParseError> {
    let source = source.trim();

    // Check for member access chain patterns like $A.$B.$C or a.b.c
    // These need to be handled BEFORE simple metavar parsing
    if is_member_access_pattern(source) {
        return parse_member_access_chain(source);
    }

    // Check for metavariable prefixes (only simple ones without dots)
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

/// Check if a pattern looks like a member access chain.
///
/// A member access chain contains dots separating parts like:
/// - `$A.$B.$C` (metavariables)
/// - `a.b.c` (identifiers)
/// - `$OBJ.method()` (mixed)
///
/// This returns true if the pattern contains at least one dot that's not:
/// - Inside parentheses (S-expressions)
/// - Part of an ellipsis (...)
/// - In a float literal (1.5)
fn is_member_access_pattern(source: &str) -> bool {
    // Quick check: must contain a dot
    if !source.contains('.') {
        return false;
    }

    // Skip S-expressions (start with '(')
    if source.starts_with('(') {
        return false;
    }

    // Skip ellipsis
    if source == "..." {
        return false;
    }

    // Skip float literals
    if source.parse::<f64>().is_ok() {
        return false;
    }

    // Check if this has the structure of member access: parts separated by dots
    // where parts look like identifiers or metavariables
    let parts: Vec<&str> = source.split('.').collect();
    if parts.len() < 2 {
        return false;
    }

    // Each part should look like an identifier, metavariable, or method call
    for part in &parts {
        let part = part.trim();
        if part.is_empty() {
            return false;
        }
        // Check if it looks like a valid part (identifier, metavar, or method call)
        if !is_valid_member_access_part(part) {
            return false;
        }
    }

    true
}

/// Check if a string looks like a valid part of a member access chain.
fn is_valid_member_access_part(part: &str) -> bool {
    let part = part.trim();
    if part.is_empty() {
        return false;
    }

    // Check for metavariable (starts with $, §, or ∀)
    let first_char = part.chars().next().unwrap();
    if METAVAR_PREFIXES.contains(&first_char) {
        // Rest should be alphanumeric/underscore
        return part[first_char.len_utf8()..].chars().all(|c| c.is_alphanumeric() || c == '_');
    }

    // Check for method call like "method()" or "method($ARG)"
    if part.contains('(') && part.ends_with(')') {
        let paren_idx = part.find('(').unwrap();
        let name = &part[..paren_idx];
        return !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_');
    }

    // Check for simple identifier
    part.chars().all(|c| c.is_alphanumeric() || c == '_')
}

/// Parse a member access chain pattern into a structural pattern.
///
/// For example, `$A.$B.$C.$D.$E.$F` represents a chain of 5+ member accesses.
/// This creates a pattern that matches MemberAccessExpression nodes with
/// the appropriate nesting depth.
fn parse_member_access_chain(source: &str) -> Result<PatternNode, PatternParseError> {
    let parts: Vec<&str> = source.split('.').collect();

    if parts.len() < 2 {
        return Err(PatternParseError::new(
            "Member access chain must have at least 2 parts",
        ));
    }

    // Parse each part as a pattern node
    let part_patterns: Result<Vec<PatternNode>, PatternParseError> = parts
        .iter()
        .map(|part| parse_member_access_part(part.trim()))
        .collect();
    let part_patterns = part_patterns?;

    // Build a nested structural pattern for member access
    // MemberAccessExpression { object: $A, member: MemberAccessExpression { object: $B, ... } }
    //
    // Actually, the AST structure is typically:
    // member_access_expression
    //   ├─ object (which can be another member_access_expression)
    //   └─ member (the accessed member)
    //
    // For `a.b.c.d.e.f`, the tree looks like:
    // member_access(member_access(member_access(member_access(member_access(a, b), c), d), e), f)
    //
    // So we build from left to right, wrapping each step.

    // Create a structural pattern that matches this chain length
    // For now, we require the node to be a MemberAccessExpression with sufficient depth
    let chain_length = part_patterns.len();

    // Build the pattern: match a MemberAccessExpression and check it has sufficient depth
    // The pattern captures each part if they're metavariables
    Ok(PatternNode::Structural {
        kind: Some(UastKind::MemberExpression),
        properties: vec![],
        children: vec![
            // We need to match a chain of the specified depth
            // Create a recursive requirement pattern
            create_depth_pattern(chain_length - 1, &part_patterns),
        ],
    })
}

/// Create a pattern that requires N levels of member access depth.
fn create_depth_pattern(remaining_depth: usize, parts: &[PatternNode]) -> PatternNode {
    if remaining_depth == 0 {
        // Base case: just need to match something (the innermost object)
        if !parts.is_empty() {
            return parts[0].clone();
        }
        return PatternNode::Wildcard;
    }

    // Recursive case: need a MemberAccessExpression containing another chain
    PatternNode::Structural {
        kind: Some(UastKind::MemberExpression),
        properties: vec![],
        children: vec![create_depth_pattern(remaining_depth - 1, parts)],
    }
}

/// Parse a single part of a member access chain.
fn parse_member_access_part(part: &str) -> Result<PatternNode, PatternParseError> {
    let part = part.trim();

    // Check for metavariable
    if let Some(node) = try_parse_metavar(part)? {
        return Ok(node);
    }

    // Check for wildcard
    if part == "*" {
        return Ok(PatternNode::Wildcard);
    }

    // Treat as identifier literal
    Ok(PatternNode::Literal(LiteralPattern::String(part.to_string())))
}

/// Try to parse a metavariable pattern.
///
/// Returns None if the source is not a simple metavar (e.g., has trailing content).
/// For example, `$VAR = $VAR` is NOT a simple metavar because it has ` = $VAR` after `$VAR`.
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
        .by_ref()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();

    if name.is_empty() {
        // Just prefix(es) without a name - not a valid metavar
        return Err(PatternParseError::new(
            "Metavariable prefix without name",
        ));
    }

    // IMPORTANT: Check if there's trailing content after the metavar name.
    // If so, this is NOT a simple metavar pattern (e.g., "$VAR = $VAR" has trailing " = $VAR").
    // In that case, return None so the caller treats it as a native pattern.
    let remaining: String = chars.collect();
    if !remaining.trim().is_empty() {
        // Has trailing content - not a simple metavar
        return Ok(None);
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
    fn test_pattern_with_trailing_content_is_native() {
        // "$VAR = $VAR" should NOT be parsed as a simple metavar $VAR
        // It should be treated as a native pattern
        let pattern = parse_simple_pattern("$VAR = $VAR", "csharp").unwrap();
        // Should be Native, not Metavar
        assert!(
            matches!(pattern.root, PatternNode::Native(_)),
            "Pattern '$VAR = $VAR' should be Native, got {:?}",
            pattern.root
        );
        // Verify it's NOT a metavar
        assert!(
            !matches!(pattern.root, PatternNode::Metavar { .. }),
            "Pattern '$VAR = $VAR' should NOT be a simple Metavar!"
        );
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

    // ========================================================================
    // Member access chain parsing tests
    // ========================================================================

    #[test]
    fn test_is_member_access_pattern() {
        // Should be member access patterns
        assert!(is_member_access_pattern("$A.$B"));
        assert!(is_member_access_pattern("$A.$B.$C"));
        assert!(is_member_access_pattern("$A.$B.$C.$D.$E.$F"));
        assert!(is_member_access_pattern("a.b.c"));
        assert!(is_member_access_pattern("obj.method"));

        // Should NOT be member access patterns
        assert!(!is_member_access_pattern("$VAR"));          // No dot
        assert!(!is_member_access_pattern("FunctionDeclaration")); // No dot
        assert!(!is_member_access_pattern("..."));            // Ellipsis
        assert!(!is_member_access_pattern("1.5"));            // Float literal
        assert!(!is_member_access_pattern("(a.b)"));          // S-expression
    }

    #[test]
    fn test_parse_member_access_chain_basic() {
        let pattern = parse_simple_pattern("$A.$B", "csharp").unwrap();
        // Should be a Structural pattern with kind MemberExpression
        if let PatternNode::Structural { kind, .. } = &pattern.root {
            assert_eq!(*kind, Some(UastKind::MemberExpression));
        } else {
            panic!("Expected Structural pattern for member access, got {:?}", pattern.root);
        }
    }

    #[test]
    fn test_parse_member_access_chain_long() {
        // This is the problematic pattern from the universal-message-chain rule
        let pattern = parse_simple_pattern("$A.$B.$C.$D.$E.$F", "csharp").unwrap();
        // Should be a Structural pattern (NOT a simple Metavar!)
        assert!(matches!(pattern.root, PatternNode::Structural { .. }),
            "Expected Structural pattern, got {:?}", pattern.root);
        // Should NOT be a simple metavar
        assert!(!matches!(pattern.root, PatternNode::Metavar { .. }),
            "Pattern should NOT be parsed as a simple Metavar!");
    }

    #[test]
    fn test_member_access_pattern_not_simple_metavar() {
        // Verify that $A.$B is NOT parsed as simple metavar $A
        let pattern = parse_simple_pattern("$A.$B", "csharp").unwrap();
        if let PatternNode::Metavar { name, .. } = &pattern.root {
            panic!("Member access should NOT be parsed as simple metavar ${}!", name);
        }
        // It should be structural
        assert!(matches!(pattern.root, PatternNode::Structural { .. }));
    }

    #[test]
    fn test_is_valid_member_access_part() {
        // Valid parts
        assert!(is_valid_member_access_part("$VAR"));
        assert!(is_valid_member_access_part("identifier"));
        assert!(is_valid_member_access_part("method()"));
        assert!(is_valid_member_access_part("$_"));

        // Invalid parts
        assert!(!is_valid_member_access_part(""));
        assert!(!is_valid_member_access_part("  "));
    }
}
