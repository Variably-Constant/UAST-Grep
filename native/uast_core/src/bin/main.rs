//! UAST-Grep - Cross-language AST search tool
//!
//! A fast structural code search tool using tree-sitter and UAST.
//! Supports 74+ languages via unified AST patterns.
//!
//! # Usage
//!
//! ```bash
//! # Parse a file and show AST
//! uast-grep parse src/main.rs -f tree
//!
//! # Search for pattern matches (UAST cross-language)
//! uast-grep run -p FunctionDeclaration -l rust ./src
//!
//! # Search for pattern matches (native tree-sitter)
//! uast-grep run -p function_item -l rust ./src
//!
//! # Scan with YAML rules
//! uast-grep scan -r rules/ ./src
//!
//! # List supported languages
//! uast-grep languages
//!
//! # Execute native tree-sitter query
//! uast-grep ts-query '(function_item name: (identifier) @name)' -l rust ./src
//! ```

use std::process;

fn main() {
    // Run the CLI and handle errors
    if let Err(e) = uast_core::cli::run() {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}
