//! High-level parser wrapper with caching and error handling.
//!
//! This module provides a safe, ergonomic wrapper around tree-sitter's parser
//! with support for incremental parsing and tree traversal.
//!
//! # Usage with Registered Languages
//!
//! ```ignore
//! // First, register the language from C# via FFI
//! // uast_register_language("kotlin", language_ptr);
//!
//! // Then create a parser
//! let parser = Parser::new("kotlin").unwrap();
//! let tree = parser.parse("fun main() {}").unwrap();
//! ```
//!
//! # Usage with Direct Language Pointer
//!
//! ```ignore
//! // Create parser directly from a language pointer (from C#)
//! let parser = Parser::from_language_ptr(language_ptr, "kotlin").unwrap();
//! let tree = parser.parse("fun main() {}").unwrap();
//! ```

use crate::error::Error;
use crate::grammars;
use std::sync::Arc;
use tree_sitter::ffi::TSLanguage;
use tree_sitter::{Language, Parser as TsParser, Tree};

// ============================================================================
// Parser
// ============================================================================

/// Parser instance that can parse source code in a specific language.
///
/// Each parser is configured for a single language. To parse multiple
/// languages, create multiple parser instances.
pub struct Parser {
    /// The underlying tree-sitter parser
    inner: TsParser,
    /// The language this parser is configured for
    language: Language,
    /// Name of the language (for debugging/display)
    language_name: String,
}

impl Parser {
    /// Create a new parser for the specified language.
    ///
    /// The language must have been previously registered via `uast_register_language`.
    ///
    /// # Arguments
    ///
    /// * `language_name` - Name of the language (e.g., "kotlin", "lua")
    ///
    /// # Errors
    ///
    /// Returns an error if the language is not registered or the parser
    /// cannot be configured.
    pub fn new(language_name: &str) -> Result<Self, Error> {
        let language = grammars::get_language(language_name)
            .ok_or_else(|| Error::unknown_language(language_name))?;

        let mut inner = TsParser::new();
        inner
            .set_language(&language)
            .map_err(|e| Error::internal(format!("Failed to set language: {}", e)))?;

        Ok(Self {
            inner,
            language,
            language_name: language_name.to_string(),
        })
    }

    /// Create a parser directly from a language pointer.
    ///
    /// This is useful when you have a language pointer from C# and don't want
    /// to register it globally.
    ///
    /// # Safety
    ///
    /// The `language_ptr` must be a valid TSLanguage* pointer obtained from
    /// a tree-sitter grammar entry point.
    ///
    /// # Arguments
    ///
    /// * `language_ptr` - Raw pointer to a TSLanguage
    /// * `language_name` - Name for display/debugging (can be any string)
    ///
    /// # Errors
    ///
    /// Returns an error if the pointer is null or invalid.
    pub unsafe fn from_language_ptr(language_ptr: *const (), language_name: &str) -> Result<Self, Error> {
        if language_ptr.is_null() {
            return Err(Error::null_pointer());
        }

        // SAFETY: Caller guarantees language_ptr is valid
        // Cast the opaque pointer to the concrete TSLanguage type
        let language = unsafe { Language::from_raw(language_ptr as *const TSLanguage) };

        // Basic validation
        if language.node_kind_count() == 0 {
            return Err(Error::internal("Language pointer appears invalid (no node kinds)"));
        }

        let mut inner = TsParser::new();
        inner
            .set_language(&language)
            .map_err(|e| Error::internal(format!("Failed to set language: {}", e)))?;

        Ok(Self {
            inner,
            language,
            language_name: language_name.to_string(),
        })
    }

    /// Create a parser by inferring language from file extension.
    ///
    /// The extension must have been previously registered via `uast_register_extensions`.
    ///
    /// # Arguments
    ///
    /// * `ext` - File extension including the dot (e.g., ".kt", ".lua")
    ///
    /// # Errors
    ///
    /// Returns an error if no language is registered for this extension.
    pub fn for_extension(ext: &str) -> Result<Self, Error> {
        let language_name = grammars::language_name_for_extension(ext)
            .ok_or_else(|| Error::unknown_language(format!("extension {}", ext)))?;

        let language = grammars::get_language(&language_name)
            .ok_or_else(|| Error::unknown_language(&language_name))?;

        let mut inner = TsParser::new();
        inner
            .set_language(&language)
            .map_err(|e| Error::internal(format!("Failed to set language: {}", e)))?;

        Ok(Self {
            inner,
            language,
            language_name,
        })
    }

    /// Parse source code, returning a parsed tree.
    ///
    /// # Arguments
    ///
    /// * `source` - The source code to parse
    ///
    /// # Errors
    ///
    /// Returns an error if parsing fails completely (the parser returns no tree).
    /// Note that parse errors in the source code do NOT cause this function to
    /// fail - instead, the tree will contain error nodes (check `has_error()`).
    pub fn parse(&mut self, source: &str) -> Result<ParsedTree, Error> {
        let tree = self
            .inner
            .parse(source, None)
            .ok_or_else(|| Error::parse_failed("Parser returned no tree"))?;

        Ok(ParsedTree {
            tree,
            source: Arc::from(source),
            language: self.language.clone(),
            language_name: self.language_name.clone(),
        })
    }

    /// Parse source code with an old tree for incremental parsing.
    ///
    /// Incremental parsing reuses parts of the old tree that haven't changed,
    /// which can significantly speed up parsing for small edits.
    ///
    /// # Arguments
    ///
    /// * `source` - The new source code to parse
    /// * `old_tree` - The previous parse tree (from a prior call to parse)
    ///
    /// # Errors
    ///
    /// Returns an error if parsing fails completely.
    pub fn parse_with_old_tree(
        &mut self,
        source: &str,
        old_tree: &Tree,
    ) -> Result<ParsedTree, Error> {
        let tree = self
            .inner
            .parse(source, Some(old_tree))
            .ok_or_else(|| Error::parse_failed("Parser returned no tree"))?;

        Ok(ParsedTree {
            tree,
            source: Arc::from(source),
            language: self.language.clone(),
            language_name: self.language_name.clone(),
        })
    }

    /// Get the language this parser uses.
    pub fn language(&self) -> &Language {
        &self.language
    }

    /// Get the language name.
    pub fn language_name(&self) -> &str {
        &self.language_name
    }

    /// Reset the parser state.
    ///
    /// This should be called if parsing was interrupted (e.g., by timeout).
    pub fn reset(&mut self) {
        self.inner.reset();
    }

    /// Set a timeout for parsing operations.
    ///
    /// If parsing takes longer than the specified duration, it will be
    /// cancelled and `parse()` will return an error.
    ///
    /// # Arguments
    ///
    /// * `timeout_micros` - Timeout in microseconds, or 0 to disable
    ///
    /// Note: In tree-sitter 0.26+, this method was removed. Use cancellation
    /// flags instead for long-running parse operations.
    pub fn set_timeout_micros(&mut self, _timeout_micros: u64) {
        // Timeout API was removed in tree-sitter 0.26
        // Keeping method signature for API compatibility
    }
}

// ============================================================================
// ParsedTree
// ============================================================================

/// A parsed syntax tree with its source code.
///
/// The tree maintains a reference to the source code so that node text
/// can be extracted without requiring the caller to keep the source alive.
pub struct ParsedTree {
    /// The tree-sitter syntax tree
    tree: Tree,
    /// The source code (shared ownership for efficiency)
    source: Arc<str>,
    /// The language used for parsing
    language: Language,
    /// Name of the language
    language_name: String,
}

impl ParsedTree {
    /// Create a ParsedTree from raw components.
    ///
    /// This is useful when you've already parsed with tree-sitter directly.
    pub fn from_raw(
        tree: Tree,
        source: &str,
        language: Language,
        language_name: &str,
    ) -> Self {
        ParsedTree {
            tree,
            source: Arc::from(source),
            language,
            language_name: language_name.to_string(),
        }
    }

    /// Get the root node of the syntax tree.
    pub fn root_node(&self) -> tree_sitter::Node<'_> {
        self.tree.root_node()
    }

    /// Get the source code.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get a shared reference to the source code.
    pub fn source_arc(&self) -> Arc<str> {
        Arc::clone(&self.source)
    }

    /// Get the language.
    pub fn language(&self) -> &Language {
        &self.language
    }

    /// Get the language name.
    pub fn language_name(&self) -> &str {
        &self.language_name
    }

    /// Get the underlying tree-sitter tree.
    pub fn tree(&self) -> &Tree {
        &self.tree
    }

    /// Check if the tree has any parse errors.
    ///
    /// Returns `true` if the root node or any descendant has an error.
    pub fn has_error(&self) -> bool {
        self.tree.root_node().has_error()
    }

    /// Get the text for a node.
    ///
    /// This extracts the source text corresponding to the node's byte range.
    pub fn node_text(&self, node: tree_sitter::Node<'_>) -> &str {
        &self.source[node.byte_range()]
    }

    /// Get the total number of nodes in the tree.
    ///
    /// This performs a full tree traversal.
    pub fn node_count(&self) -> usize {
        self.walk().count()
    }

    /// Create a depth-first tree walker.
    ///
    /// The walker visits all nodes in pre-order (parent before children).
    pub fn walk(&self) -> TreeWalker<'_> {
        TreeWalker::new(self)
    }

    /// Create a walker that only visits named nodes.
    ///
    /// Anonymous nodes (like punctuation) are skipped.
    pub fn walk_named(&self) -> impl Iterator<Item = tree_sitter::Node<'_>> {
        self.walk().filter(|node| node.is_named())
    }

    /// Get the root node kind (e.g., "program", "module").
    pub fn root_kind(&self) -> &str {
        self.tree.root_node().kind()
    }
}

// ============================================================================
// TreeWalker
// ============================================================================

/// Depth-first tree walker that visits all nodes.
///
/// Nodes are visited in pre-order (parent before children).
pub struct TreeWalker<'a> {
    /// Reference to the parsed tree (for source access)
    tree: &'a ParsedTree,
    /// The tree-sitter cursor for traversal
    cursor: tree_sitter::TreeCursor<'a>,
    /// Whether we've finished traversal
    done: bool,
}

impl<'a> TreeWalker<'a> {
    /// Create a new walker starting at the root.
    fn new(tree: &'a ParsedTree) -> Self {
        Self {
            tree,
            cursor: tree.tree.walk(),
            done: false,
        }
    }

    /// Get the parsed tree reference.
    pub fn tree(&self) -> &'a ParsedTree {
        self.tree
    }

    /// Get the current field name, if any.
    pub fn field_name(&self) -> Option<&'static str> {
        self.cursor.field_name()
    }

    /// Get the current node's depth in the tree.
    pub fn depth(&self) -> usize {
        self.cursor.depth() as usize
    }
}

impl<'a> Iterator for TreeWalker<'a> {
    type Item = tree_sitter::Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let node = self.cursor.node();

        // Try to descend to first child
        if self.cursor.goto_first_child() {
            return Some(node);
        }

        // Try to move to next sibling
        if self.cursor.goto_next_sibling() {
            return Some(node);
        }

        // Backtrack up the tree until we find a sibling
        loop {
            if !self.cursor.goto_parent() {
                // We've returned to the root and visited everything
                self.done = true;
                return Some(node);
            }
            if self.cursor.goto_next_sibling() {
                return Some(node);
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_unregistered_language() {
        // No languages are registered by default in this version
        let parser = Parser::new("javascript");
        // Should fail because no language is registered
        assert!(parser.is_err());
    }

    #[test]
    fn test_for_extension_unregistered() {
        let parser = Parser::for_extension(".xyz_unknown");
        assert!(parser.is_err());
    }

    #[test]
    fn test_from_null_ptr() {
        let result = unsafe { Parser::from_language_ptr(std::ptr::null(), "test") };
        assert!(result.is_err());
    }
}
