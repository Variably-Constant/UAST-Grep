//! UAST Core - High-performance parsing library for UAST-Grep
//!
//! This crate provides C ABI exports for use from C#/.NET via P/Invoke.
//! All public functions are `extern "C"` and use `#[unsafe(no_mangle)]` for stable symbols.
//!
//! # Architecture
//!
//! This version accepts language pointers from C# instead of embedding grammars.
//! Languages are loaded by C# using DynamicLanguage.cs, which loads individual
//! grammar DLLs (~2-3MB each) on demand. The language pointers are then passed
//! to Rust via FFI for fast parsing.
//!
//! ## Language Registration Flow
//!
//! 1. C# loads grammar DLL (e.g., tree-sitter-kotlin.dll)
//! 2. C# gets TSLanguage* pointer from DLL entry point
//! 3. C# calls uast_register_language(name, ptr) to register with Rust
//! 4. C# can now call uast_parser_new(name) to create parsers
//!
//! Alternatively, for one-off parsing:
//! 1. C# loads grammar DLL and gets TSLanguage* pointer
//! 2. C# calls uast_parser_new_with_language(ptr) directly
//!
//! # FFI Safety
//!
//! All FFI functions follow these conventions:
//! - Null pointer checks on all input pointers
//! - Box::into_raw/Box::from_raw for handle management
//! - Thread-local error storage for detailed error retrieval
//! - Safety documentation on all unsafe functions

pub mod builtin_languages;
pub mod cli;
pub mod dynamic_loader;
pub mod error;
pub mod ffi;
pub mod ffi_matching;
pub mod ffi_rules;
pub mod ffi_sarif;
pub mod ffi_uast;
pub mod grammars;
pub mod matching;
pub mod parser;
#[cfg(feature = "python")]
pub mod python;
pub mod query;
pub mod rules;
pub mod sarif;
pub mod scanner;
pub mod tiers;
pub mod tree;
pub mod uast;
pub mod wasm_loader;

// Re-export FFI types for convenience
pub use ffi::*;
pub use ffi_matching::*;
pub use ffi_rules::*;
pub use ffi_sarif::*;
pub use ffi_uast::*;
pub use matching::*;
pub use rules::*;
pub use sarif::*;
pub use uast::*;

use crate::parser::{ParsedTree, Parser};
use crate::query::CompiledQuery;
use crate::tree::{node_from_cursor, node_to_ffi};
use std::ffi::{c_char, c_void};
use std::ptr;
use streaming_iterator::StreamingIterator;

// ============================================================================
// Version and Initialization
// ============================================================================

/// Get the library version string.
///
/// Returns a pointer to a static string, do not free.
///
/// # Safety
///
/// The returned pointer is valid for the lifetime of the program.
/// The caller must not attempt to free this pointer.
#[unsafe(no_mangle)]
pub extern "C" fn uast_version() -> *const c_char {
    // Static string with null terminator
    static VERSION: &[u8] = b"0.2.0\0";
    VERSION.as_ptr() as *const c_char
}

/// Initialize the library (call once at startup).
///
/// Returns `UastResult::Ok` on success.
///
/// # Safety
///
/// This function is safe to call multiple times. It is a no-op in this version
/// since languages are registered dynamically.
#[unsafe(no_mangle)]
pub extern "C" fn uast_init() -> UastResult {
    // Nothing to initialize - languages are registered dynamically
    UastResult::Ok
}

/// Get the number of registered languages.
///
/// Returns the count of languages that have been registered via `uast_register_language`.
#[unsafe(no_mangle)]
pub extern "C" fn uast_language_count() -> u32 {
    grammars::language_count() as u32
}

// ============================================================================
// Language Registration (NEW - for dynamic language loading)
// ============================================================================

/// Register a language from a raw pointer obtained from a grammar DLL.
///
/// This allows C# to load grammar DLLs dynamically and register them with
/// the Rust parsing engine. Once registered, the language can be used with
/// `uast_parser_new`.
///
/// # Arguments
///
/// * `name` - A null-terminated UTF-8 string with the language name (e.g., "kotlin")
/// * `language_ptr` - The TSLanguage* pointer obtained from the grammar DLL
///
/// # Returns
///
/// `UastResult::Ok` on success, error code otherwise.
///
/// # Safety
///
/// - `name` must be a valid pointer to a null-terminated UTF-8 string
/// - `language_ptr` must be a valid TSLanguage* pointer from a tree-sitter grammar
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_register_language(
    name: *const c_char,
    language_ptr: *const (),
) -> UastResult {
    error::clear_last_error();

    let name_str = match unsafe { cstr_to_str(name) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return UastResult::NullPointer;
        }
    };

    if language_ptr.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return UastResult::NullPointer;
    }

    // SAFETY: Caller guarantees language_ptr is valid
    let success = unsafe { grammars::register_language_ptr(name_str, language_ptr) };

    if success {
        UastResult::Ok
    } else {
        error::set_last_error(error::Error::internal("Failed to register language - invalid pointer"));
        UastResult::InternalError
    }
}

/// Register file extensions for a language.
///
/// This maps file extensions to a language name, allowing `uast_parser_for_extension`
/// to work correctly.
///
/// # Arguments
///
/// * `language_name` - A null-terminated UTF-8 string with the language name
/// * `extensions` - Array of null-terminated UTF-8 string pointers for extensions
/// * `extension_count` - Number of extensions in the array
///
/// # Returns
///
/// `UastResult::Ok` on success.
///
/// # Safety
///
/// - All string pointers must be valid null-terminated UTF-8 strings
/// - `extensions` must point to at least `extension_count` valid pointers
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_register_extensions(
    language_name: *const c_char,
    extensions: *const *const c_char,
    extension_count: u32,
) -> UastResult {
    error::clear_last_error();

    let name_str = match unsafe { cstr_to_str(language_name) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return UastResult::NullPointer;
        }
    };

    if extensions.is_null() && extension_count > 0 {
        error::set_last_error(error::Error::null_pointer());
        return UastResult::NullPointer;
    }

    // Collect extensions into a Vec
    let mut ext_strings: Vec<String> = Vec::with_capacity(extension_count as usize);
    for i in 0..extension_count {
        let ext_ptr = unsafe { *extensions.add(i as usize) };
        match unsafe { cstr_to_str(ext_ptr) } {
            Ok(s) => ext_strings.push(s.to_string()),
            Err(_) => {
                error::set_last_error(error::Error::invalid_utf8(format!("extension {}", i)));
                return UastResult::InvalidUtf8;
            }
        }
    }

    // Register with borrowed slices
    let ext_refs: Vec<&str> = ext_strings.iter().map(|s| s.as_str()).collect();
    grammars::register_extensions(name_str, &ext_refs);

    UastResult::Ok
}

/// Unregister a language.
///
/// Removes a language from the registry. Existing parsers using this language
/// will continue to work, but new parsers cannot be created for this language.
///
/// # Arguments
///
/// * `name` - A null-terminated UTF-8 string with the language name
///
/// # Returns
///
/// `true` if the language was found and removed, `false` otherwise.
///
/// # Safety
///
/// - `name` must be a valid pointer to a null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_unregister_language(name: *const c_char) -> bool {
    let name_str = match unsafe { cstr_to_str(name) } {
        Ok(s) => s,
        Err(_) => return false,
    };

    grammars::unregister_language(name_str)
}

// ============================================================================
// Parser Management
// ============================================================================

/// Create a new parser for the specified language.
///
/// The language must have been previously registered via `uast_register_language`.
///
/// # Arguments
///
/// * `language` - A null-terminated UTF-8 string with the language name
///   (e.g., "kotlin", "lua")
///
/// # Returns
///
/// A parser handle on success, or null on error. If null is returned,
/// call `uast_last_error` to get error details.
///
/// # Safety
///
/// - `language` must be a valid pointer to a null-terminated UTF-8 string
/// - The returned handle must be freed with `uast_parser_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parser_new(language: *const c_char) -> *mut UastParser {
    error::clear_last_error();

    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    match Parser::new(lang_str) {
        Ok(parser) => Box::into_raw(Box::new(parser)) as *mut UastParser,
        Err(e) => {
            error::set_last_error(e);
            ptr::null_mut()
        }
    }
}

/// Create a parser directly from a language pointer.
///
/// This is useful for one-off parsing without registering the language globally.
///
/// # Arguments
///
/// * `language_ptr` - The TSLanguage* pointer from a grammar DLL
/// * `language_name` - A name for the language (for debugging/display)
///
/// # Returns
///
/// A parser handle on success, or null on error.
///
/// # Safety
///
/// - `language_ptr` must be a valid TSLanguage* pointer
/// - `language_name` must be a valid pointer to a null-terminated UTF-8 string
/// - The returned handle must be freed with `uast_parser_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parser_new_with_language(
    language_ptr: *const (),
    language_name: *const c_char,
) -> *mut UastParser {
    error::clear_last_error();

    let name_str = match unsafe { cstr_to_str(language_name) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    if language_ptr.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return ptr::null_mut();
    }

    // SAFETY: Caller guarantees language_ptr is valid
    match unsafe { Parser::from_language_ptr(language_ptr, name_str) } {
        Ok(parser) => Box::into_raw(Box::new(parser)) as *mut UastParser,
        Err(e) => {
            error::set_last_error(e);
            ptr::null_mut()
        }
    }
}

/// Create a parser for a file extension.
///
/// The parser language is inferred from the file extension. The extension must
/// have been registered via `uast_register_extensions`.
///
/// # Arguments
///
/// * `extension` - A null-terminated UTF-8 string with the file extension
///   including the dot (e.g., ".kt", ".lua")
///
/// # Returns
///
/// A parser handle on success, or null on error.
///
/// # Safety
///
/// - `extension` must be a valid pointer to a null-terminated UTF-8 string
/// - The returned handle must be freed with `uast_parser_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parser_for_extension(extension: *const c_char) -> *mut UastParser {
    error::clear_last_error();

    let ext_str = match unsafe { cstr_to_str(extension) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    match Parser::for_extension(ext_str) {
        Ok(parser) => Box::into_raw(Box::new(parser)) as *mut UastParser,
        Err(e) => {
            error::set_last_error(e);
            ptr::null_mut()
        }
    }
}

/// Free a parser.
///
/// # Safety
///
/// - `parser` must have been created by `uast_parser_new`, `uast_parser_new_with_language`,
///   or `uast_parser_for_extension`
/// - `parser` must not be used after this call
/// - Passing null is safe (no-op)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parser_free(parser: *mut UastParser) {
    if !parser.is_null() {
        // SAFETY: Caller guarantees parser was created by this library
        unsafe {
            drop(Box::from_raw(parser as *mut Parser));
        }
    }
}

// ============================================================================
// Parsing
// ============================================================================

/// Parse source code, returning a tree handle.
///
/// # Arguments
///
/// * `parser` - A valid parser handle
/// * `source` - Pointer to the source code bytes (UTF-8)
/// * `source_len` - Length of the source code in bytes
///
/// # Returns
///
/// A tree handle on success, or null on error. The tree owns a copy of
/// the source code, so the caller can free their source buffer after
/// this call returns.
///
/// # Safety
///
/// - `parser` must be a valid parser handle from `uast_parser_new`
/// - `source` must be a valid pointer to at least `source_len` bytes
/// - `source` must contain valid UTF-8
/// - The returned tree must be freed with `uast_tree_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parse(
    parser: *mut UastParser,
    source: *const c_char,
    source_len: u32,
) -> *mut UastTree {
    error::clear_last_error();

    if parser.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return ptr::null_mut();
    }

    // SAFETY: Caller guarantees parser is valid
    let parser = unsafe { &mut *(parser as *mut Parser) };

    let source_str = match unsafe { cstr_to_str_len(source, source_len) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("source"));
            return ptr::null_mut();
        }
    };

    match parser.parse(source_str) {
        Ok(tree) => Box::into_raw(Box::new(tree)) as *mut UastTree,
        Err(e) => {
            error::set_last_error(e);
            ptr::null_mut()
        }
    }
}

/// Free a parsed tree.
///
/// # Safety
///
/// - `tree` must have been created by `uast_parse`
/// - `tree` must not be used after this call
/// - Passing null is safe (no-op)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_tree_free(tree: *mut UastTree) {
    if !tree.is_null() {
        // SAFETY: Caller guarantees tree was created by this library
        unsafe {
            drop(Box::from_raw(tree as *mut ParsedTree));
        }
    }
}

/// Get the root node of a tree.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `out_node` - Pointer to a `UastNode` struct to fill with the root node
///
/// # Returns
///
/// `UastResult::Ok` on success, error code otherwise.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `out_node` must be a valid pointer to a `UastNode` struct
/// - The node data is valid only while the tree is alive
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_tree_root_node(
    tree: *const UastTree,
    out_node: *mut UastNode,
) -> UastResult {
    if tree.is_null() {
        return UastResult::NullPointer;
    }
    if out_node.is_null() {
        return UastResult::NullPointer;
    }

    // SAFETY: Caller guarantees tree and out_node are valid
    unsafe {
        let tree = &*(tree as *const ParsedTree);
        let root = tree.root_node();
        *out_node = node_to_ffi(root, None);
    }

    UastResult::Ok
}

/// Check if the tree has any parse errors.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
///
/// # Returns
///
/// `true` if the tree has errors, `false` otherwise.
/// Returns `true` if `tree` is null (defensive).
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`, or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_tree_has_error(tree: *const UastTree) -> bool {
    if tree.is_null() {
        return true;
    }

    // SAFETY: Caller guarantees tree is valid
    let tree = unsafe { &*(tree as *const ParsedTree) };
    tree.has_error()
}

/// Get the total number of nodes in the tree.
///
/// This performs a full tree traversal and may be slow for large trees.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
///
/// # Returns
///
/// The number of nodes, or 0 if `tree` is null.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`, or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_tree_node_count(tree: *const UastTree) -> u32 {
    if tree.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees tree is valid
    let tree = unsafe { &*(tree as *const ParsedTree) };
    tree.node_count() as u32
}

// ============================================================================
// Tree Traversal
// ============================================================================

/// Walk all nodes in the tree depth-first, calling a callback for each.
///
/// The callback receives each node and can return `false` to stop traversal.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `callback` - Function to call for each node
/// * `user_data` - Opaque pointer passed to the callback
///
/// # Returns
///
/// `UastResult::Ok` if traversal completed, error code otherwise.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `callback` must be a valid function pointer
/// - `user_data` is passed to the callback and must remain valid during traversal
/// - Node data passed to callback is valid only during the callback invocation
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_tree_walk(
    tree: *const UastTree,
    callback: NodeCallback,
    user_data: *mut c_void,
) -> UastResult {
    if tree.is_null() {
        return UastResult::NullPointer;
    }

    // SAFETY: Caller guarantees tree is valid
    let tree = unsafe { &*(tree as *const ParsedTree) };
    let mut cursor = tree.tree().walk();

    loop {
        let ffi_node = node_from_cursor(&cursor);

        // Call the callback
        let continue_walk = callback(&ffi_node, user_data);
        if !continue_walk {
            break;
        }

        // Navigate the tree
        if cursor.goto_first_child() {
            continue;
        }

        if cursor.goto_next_sibling() {
            continue;
        }

        // Backtrack until we find a sibling or reach root
        loop {
            if !cursor.goto_parent() {
                return UastResult::Ok;
            }
            if cursor.goto_next_sibling() {
                break;
            }
        }
    }

    UastResult::Ok
}

/// Walk only named nodes in the tree depth-first.
///
/// Anonymous nodes (like punctuation) are skipped.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `callback` - Function to call for each named node
/// * `user_data` - Opaque pointer passed to the callback
///
/// # Returns
///
/// `UastResult::Ok` if traversal completed, error code otherwise.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `callback` must be a valid function pointer
/// - `user_data` is passed to the callback and must remain valid during traversal
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_tree_walk_named(
    tree: *const UastTree,
    callback: NodeCallback,
    user_data: *mut c_void,
) -> UastResult {
    if tree.is_null() {
        return UastResult::NullPointer;
    }

    // SAFETY: Caller guarantees tree is valid
    let tree = unsafe { &*(tree as *const ParsedTree) };
    let mut cursor = tree.tree().walk();

    loop {
        let node = cursor.node();

        // Only call callback for named nodes
        if node.is_named() {
            let ffi_node = node_from_cursor(&cursor);

            let continue_walk = callback(&ffi_node, user_data);
            if !continue_walk {
                break;
            }
        }

        // Navigate the tree
        if cursor.goto_first_child() {
            continue;
        }

        if cursor.goto_next_sibling() {
            continue;
        }

        // Backtrack until we find a sibling or reach root
        loop {
            if !cursor.goto_parent() {
                return UastResult::Ok;
            }
            if cursor.goto_next_sibling() {
                break;
            }
        }
    }

    UastResult::Ok
}

/// Get the source text for a node.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `node` - A valid node from this tree
/// * `out_text` - Pointer to receive the text pointer (not null-terminated)
/// * `out_len` - Pointer to receive the text length
///
/// # Returns
///
/// `UastResult::Ok` on success, error code otherwise.
/// The text pointer points into the tree's source buffer and is valid
/// only while the tree is alive.
///
/// # Safety
///
/// - `tree` must be a valid tree handle
/// - `node` must be a valid node from this tree
/// - `out_text` and `out_len` must be valid pointers
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_node_text(
    tree: *const UastTree,
    node: *const UastNode,
    out_text: *mut *const c_char,
    out_len: *mut u32,
) -> UastResult {
    if tree.is_null() || node.is_null() || out_text.is_null() || out_len.is_null() {
        return UastResult::NullPointer;
    }

    // SAFETY: Caller guarantees all pointers are valid
    unsafe {
        let tree = &*(tree as *const ParsedTree);
        let node = &*node;

        let source = tree.source();
        let start = node.range.start_byte as usize;
        let end = node.range.end_byte as usize;

        if end <= source.len() && start <= end {
            let text = &source[start..end];
            *out_text = text.as_ptr() as *const c_char;
            *out_len = text.len() as u32;
            UastResult::Ok
        } else {
            *out_text = ptr::null();
            *out_len = 0;
            UastResult::InternalError
        }
    }
}

// ============================================================================
// Language Query Functions
// ============================================================================

/// Check if a language is registered.
///
/// # Arguments
///
/// * `language` - A null-terminated UTF-8 string with the language name
///
/// # Returns
///
/// `true` if the language is registered, `false` otherwise.
///
/// # Safety
///
/// - `language` must be a valid pointer to a null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_has_language(language: *const c_char) -> bool {
    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => return false,
    };
    grammars::has_language(lang_str)
}

/// Get the language name for a file extension.
///
/// # Arguments
///
/// * `extension` - A null-terminated UTF-8 string with the extension (e.g., ".kt")
/// * `out_name` - Pointer to receive the language name pointer
/// * `out_len` - Pointer to receive the language name length
///
/// # Returns
///
/// `UastResult::Ok` if found, `UastResult::UnknownLanguage` if not.
///
/// Note: Unlike the embedded version, the returned name may not be static.
/// The caller should copy the string if needed beyond the current call.
///
/// # Safety
///
/// - `extension` must be a valid pointer to a null-terminated UTF-8 string
/// - `out_name` and `out_len` must be valid pointers
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_language_for_extension(
    extension: *const c_char,
    out_name: *mut *const c_char,
    out_len: *mut u32,
) -> UastResult {
    if out_name.is_null() || out_len.is_null() {
        return UastResult::NullPointer;
    }

    let ext_str = match unsafe { cstr_to_str(extension) } {
        Ok(s) => s,
        Err(_) => return UastResult::NullPointer,
    };

    match grammars::language_name_for_extension(ext_str) {
        Some(name) => {
            // SAFETY: out_name and out_len are valid
            // Note: We need to allocate a new string since the name is not static
            let name_cstring = ffi::str_to_cstring(&name);
            if name_cstring.is_null() {
                unsafe {
                    *out_name = ptr::null();
                    *out_len = 0;
                }
                return UastResult::InternalError;
            }
            unsafe {
                *out_name = name_cstring;
                *out_len = name.len() as u32;
            }
            UastResult::Ok
        }
        None => {
            unsafe {
                *out_name = ptr::null();
                *out_len = 0;
            }
            UastResult::UnknownLanguage
        }
    }
}

// ============================================================================
// Query Compilation and Execution
// ============================================================================

/// Compile a query pattern for a tree's language.
///
/// The query can be reused across multiple trees of the same language.
///
/// # Arguments
///
/// * `tree` - A valid tree handle (used to get the language)
/// * `pattern` - Query pattern in tree-sitter S-expression syntax
/// * `pattern_len` - Length of the pattern in bytes
///
/// # Returns
///
/// A query handle on success, or null on error.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `pattern` must be a valid pointer to at least `pattern_len` bytes of UTF-8
/// - The returned handle must be freed with `uast_query_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_new(
    tree: *const UastTree,
    pattern: *const c_char,
    pattern_len: u32,
) -> *mut UastQuery {
    error::clear_last_error();

    if tree.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return ptr::null_mut();
    }

    let pattern_str = match unsafe { cstr_to_str_len(pattern, pattern_len) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("pattern"));
            return ptr::null_mut();
        }
    };

    // SAFETY: Caller guarantees tree is valid
    let tree = unsafe { &*(tree as *const ParsedTree) };

    match CompiledQuery::new(tree, pattern_str) {
        Ok(query) => Box::into_raw(Box::new(query)) as *mut UastQuery,
        Err(e) => {
            error::set_last_error(e);
            ptr::null_mut()
        }
    }
}

/// Free a compiled query.
///
/// # Safety
///
/// - `query` must have been created by `uast_query_new`
/// - `query` must not be used after this call
/// - Passing null is safe (no-op)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_free(query: *mut UastQuery) {
    if !query.is_null() {
        // SAFETY: Caller guarantees query was created by this library
        unsafe {
            drop(Box::from_raw(query as *mut CompiledQuery));
        }
    }
}

/// Get the number of patterns in a compiled query.
///
/// # Arguments
///
/// * `query` - A valid query handle
///
/// # Returns
///
/// The number of patterns, or 0 if query is null.
///
/// # Safety
///
/// - `query` must be a valid query handle from `uast_query_new`, or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_pattern_count(query: *const UastQuery) -> u32 {
    if query.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees query is valid
    let query = unsafe { &*(query as *const CompiledQuery) };
    query.pattern_count() as u32
}

/// Get the number of capture names in a compiled query.
///
/// # Arguments
///
/// * `query` - A valid query handle
///
/// # Returns
///
/// The number of capture names, or 0 if query is null.
///
/// # Safety
///
/// - `query` must be a valid query handle from `uast_query_new`, or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_capture_count(query: *const UastQuery) -> u32 {
    if query.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees query is valid
    let query = unsafe { &*(query as *const CompiledQuery) };
    query.capture_names().len() as u32
}

/// Execute a compiled query on a tree, calling a callback for each match.
///
/// The callback receives each match and can return `false` to stop iteration.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `query` - A valid compiled query handle
/// * `callback` - Function to call for each match
/// * `user_data` - Opaque pointer passed to the callback
///
/// # Returns
///
/// `UastResult::Ok` if execution completed successfully.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `query` must be a valid query handle from `uast_query_new`
/// - `callback` must be a valid function pointer
/// - `user_data` is passed to the callback and must remain valid during execution
/// - Match and capture data passed to callback is valid only during callback
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_execute(
    tree: *const UastTree,
    query: *const UastQuery,
    callback: MatchCallback,
    user_data: *mut c_void,
) -> UastResult {
    error::clear_last_error();

    if tree.is_null() || query.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return UastResult::NullPointer;
    }

    // SAFETY: Caller guarantees both pointers are valid
    let tree = unsafe { &*(tree as *const ParsedTree) };
    let compiled_query = unsafe { &*(query as *const CompiledQuery) };

    let mut cursor = tree_sitter::QueryCursor::new();
    let source = tree.source();

    let mut matches = cursor.matches(compiled_query.inner(), tree.root_node(), source.as_bytes());
    while let Some(m) = matches.next() {
        // Build FFI captures - need to keep names alive for callback
        let capture_names = compiled_query.capture_names();
        let capture_data: Vec<(String, UastNode)> = m
            .captures
            .iter()
            .map(|c| {
                let name = capture_names[c.index as usize].clone();
                let node = node_to_ffi(c.node, None);
                (name, node)
            })
            .collect();

        // Create FFI capture structs
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

        // Call the callback
        let continue_iteration = callback(&ffi_match, user_data);
        if !continue_iteration {
            break;
        }
    }

    UastResult::Ok
}

/// Execute a query pattern directly on a tree (compile and run).
///
/// This is a convenience function that compiles the pattern and executes it
/// in one call. For repeated queries, use `uast_query_new` + `uast_query_execute`.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `pattern` - Query pattern in tree-sitter S-expression syntax
/// * `pattern_len` - Length of the pattern in bytes
/// * `callback` - Function to call for each match
/// * `user_data` - Opaque pointer passed to the callback
///
/// # Returns
///
/// `UastResult::Ok` if execution completed successfully.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `pattern` must be a valid pointer to at least `pattern_len` bytes of UTF-8
/// - `callback` must be a valid function pointer
/// - `user_data` is passed to the callback and must remain valid during execution
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_execute_simple(
    tree: *const UastTree,
    pattern: *const c_char,
    pattern_len: u32,
    callback: MatchCallback,
    user_data: *mut c_void,
) -> UastResult {
    error::clear_last_error();

    if tree.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return UastResult::NullPointer;
    }

    let pattern_str = match unsafe { cstr_to_str_len(pattern, pattern_len) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("pattern"));
            return UastResult::InvalidUtf8;
        }
    };

    // SAFETY: Caller guarantees tree is valid
    let tree = unsafe { &*(tree as *const ParsedTree) };

    // Compile the query
    let compiled_query = match CompiledQuery::new(tree, pattern_str) {
        Ok(q) => q,
        Err(e) => {
            error::set_last_error(e);
            return UastResult::QueryFailed;
        }
    };

    // Execute
    let mut cursor = tree_sitter::QueryCursor::new();
    let source = tree.source();

    let mut matches = cursor.matches(compiled_query.inner(), tree.root_node(), source.as_bytes());
    while let Some(m) = matches.next() {
        // Build FFI captures
        let capture_names = compiled_query.capture_names();
        let capture_data: Vec<(String, UastNode)> = m
            .captures
            .iter()
            .map(|c| {
                let name = capture_names[c.index as usize].clone();
                let node = node_to_ffi(c.node, None);
                (name, node)
            })
            .collect();

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

        let continue_iteration = callback(&ffi_match, user_data);
        if !continue_iteration {
            break;
        }
    }

    UastResult::Ok
}

/// Count total matches without calling a callback.
///
/// This is useful for getting a count before allocating storage.
///
/// # Arguments
///
/// * `tree` - A valid tree handle
/// * `query` - A valid compiled query handle
///
/// # Returns
///
/// The number of matches, or 0 on error.
///
/// # Safety
///
/// - `tree` must be a valid tree handle from `uast_parse`
/// - `query` must be a valid query handle from `uast_query_new`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_query_match_count(
    tree: *const UastTree,
    query: *const UastQuery,
) -> u32 {
    if tree.is_null() || query.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees both pointers are valid
    let tree = unsafe { &*(tree as *const ParsedTree) };
    let compiled_query = unsafe { &*(query as *const CompiledQuery) };

    let mut cursor = tree_sitter::QueryCursor::new();
    let source = tree.source();

    let mut matches = cursor.matches(compiled_query.inner(), tree.root_node(), source.as_bytes());
    let mut count: u32 = 0;
    while matches.next().is_some() {
        count += 1;
    }
    count
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_version() {
        let version = uast_version();
        assert!(!version.is_null());

        // SAFETY: We know the version string is valid UTF-8
        let version_str = unsafe { std::ffi::CStr::from_ptr(version).to_str().unwrap() };
        assert_eq!(version_str, "0.2.0");
    }

    #[test]
    fn test_init() {
        let result = uast_init();
        assert_eq!(result, UastResult::Ok);
    }

    #[test]
    fn test_language_count_empty() {
        // In this version, no languages are embedded - count starts at 0
        // (unless tests have registered some)
        let count = uast_language_count();
        // Just verify it doesn't crash
        assert!(count >= 0);
    }

    #[test]
    fn test_register_null_language() {
        let result = unsafe { uast_register_language(ptr::null(), ptr::null()) };
        assert_eq!(result, UastResult::NullPointer);
    }

    #[test]
    fn test_register_null_ptr() {
        let lang = CString::new("test_null").unwrap();
        let result = unsafe { uast_register_language(lang.as_ptr(), ptr::null()) };
        assert_eq!(result, UastResult::NullPointer);
    }

    #[test]
    fn test_parser_new_unregistered() {
        let lang = CString::new("nonexistent_language").unwrap();
        let parser = unsafe { uast_parser_new(lang.as_ptr()) };
        assert!(parser.is_null());
        assert!(error::has_last_error());
    }

    #[test]
    fn test_parser_with_null_language() {
        let name = CString::new("test").unwrap();
        let parser = unsafe { uast_parser_new_with_language(ptr::null(), name.as_ptr()) };
        assert!(parser.is_null());
    }

    #[test]
    fn test_has_language_unregistered() {
        let lang = CString::new("not_registered_xyz").unwrap();
        assert!(!unsafe { uast_has_language(lang.as_ptr()) });
    }

    #[test]
    fn test_null_safety() {
        // All functions should handle null gracefully
        unsafe {
            assert!(uast_parser_new(ptr::null()).is_null());
            assert!(uast_parser_for_extension(ptr::null()).is_null());
            uast_parser_free(ptr::null_mut()); // Should not crash

            assert!(uast_parse(ptr::null_mut(), ptr::null(), 0).is_null());
            uast_tree_free(ptr::null_mut()); // Should not crash

            assert!(uast_tree_has_error(ptr::null()));
            assert_eq!(uast_tree_node_count(ptr::null()), 0);

            let mut node = UastNode::default();
            assert_eq!(
                uast_tree_root_node(ptr::null(), &mut node),
                UastResult::NullPointer
            );
        }
    }

    #[test]
    fn test_query_null_safety() {
        // Query functions should handle null gracefully
        unsafe {
            // uast_query_new with null tree
            let pattern = CString::new("(identifier)").unwrap();
            let query = uast_query_new(ptr::null(), pattern.as_ptr(), pattern.as_bytes().len() as u32);
            assert!(query.is_null());
            assert!(error::has_last_error());
            error::clear_last_error();

            // uast_query_free with null - should not crash
            uast_query_free(ptr::null_mut());

            // uast_query_pattern_count with null
            assert_eq!(uast_query_pattern_count(ptr::null()), 0);

            // uast_query_capture_count with null
            assert_eq!(uast_query_capture_count(ptr::null()), 0);

            // uast_query_match_count with null tree/query
            assert_eq!(uast_query_match_count(ptr::null(), ptr::null()), 0);
        }
    }

    // Callback for test_query_execute_null_safety
    extern "C" fn dummy_match_callback(_: *const UastMatch, _: *mut c_void) -> bool {
        true
    }

    #[test]
    fn test_query_execute_null_safety() {
        unsafe {
            let pattern = CString::new("(identifier)").unwrap();

            // uast_query_execute with null tree
            let result = uast_query_execute(
                ptr::null(),
                ptr::null(),
                dummy_match_callback,
                ptr::null_mut(),
            );
            assert_eq!(result, UastResult::NullPointer);

            // uast_query_execute_simple with null tree
            let result = uast_query_execute_simple(
                ptr::null(),
                pattern.as_ptr(),
                pattern.as_bytes().len() as u32,
                dummy_match_callback,
                ptr::null_mut(),
            );
            assert_eq!(result, UastResult::NullPointer);
        }
    }
}
