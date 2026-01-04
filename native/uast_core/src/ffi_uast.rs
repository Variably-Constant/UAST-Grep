//! FFI exports for UAST functionality.
//!
//! This module provides C ABI exports for UAST parsing, allowing C#/.NET
//! to call into the Rust UAST mapper via P/Invoke.
//!
//! # API Overview
//!
//! - `uast_parse_uast` - Parse to JSON (original API, for backward compatibility)
//! - `uast_parse_to_typed_json` - Parse to typed UAST schema as JSON
//! - `uast_free_json` - Free JSON strings returned by parse functions

use crate::error;
use crate::ffi::cstr_to_str;
use crate::parser::Parser;
use crate::uast::builtin::get_builtin_language;
use crate::uast::{parse_to_uast_json, ConvertOptions};
use std::ffi::{c_char, c_int, CString};
use std::ptr;
use tree_sitter::Parser as TsParser;

/// Parse source code to UAST JSON.
///
/// # Arguments
///
/// * `language` - Null-terminated UTF-8 language name (must be registered)
/// * `source` - Null-terminated UTF-8 source code
/// * `source_path` - Optional null-terminated UTF-8 file path (can be null)
/// * `out_json` - Pointer to receive the allocated JSON string
///
/// # Returns
///
/// 0 on success, non-zero error code on failure.
/// On success, `out_json` points to an allocated null-terminated string
/// that must be freed with `uast_free_json`.
///
/// # Safety
///
/// - `language` and `source` must be valid null-terminated UTF-8 strings
/// - `source_path` can be null or a valid null-terminated UTF-8 string
/// - `out_json` must be a valid pointer
/// - The caller must free the returned string with `uast_free_json`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parse_uast(
    language: *const c_char,
    source: *const c_char,
    source_path: *const c_char,
    out_json: *mut *mut c_char,
) -> c_int {
    error::clear_last_error();

    if out_json.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return 1;
    }

    // Initialize output to null
    unsafe {
        *out_json = ptr::null_mut();
    }

    // Parse language string
    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return 1;
        }
    };

    // Parse source string
    let source_str = match unsafe { cstr_to_str(source) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("source"));
            return 2;
        }
    };

    // Parse optional source path
    let path_str: Option<&str> = if source_path.is_null() {
        None
    } else {
        match unsafe { cstr_to_str(source_path) } {
            Ok(s) => Some(s),
            Err(_) => None, // Treat invalid path as no path
        }
    };

    // Perform the parsing
    match parse_to_uast_json(lang_str, source_str, path_str) {
        Ok(json_string) => {
            // Allocate and copy the result
            match CString::new(json_string) {
                Ok(cstring) => {
                    unsafe {
                        *out_json = cstring.into_raw();
                    }
                    0 // Success
                }
                Err(_) => {
                    error::set_last_error(error::Error::internal(
                        "JSON contains null byte",
                    ));
                    3
                }
            }
        }
        Err(err_msg) => {
            error::set_last_error(error::Error::internal(&err_msg));
            4
        }
    }
}

/// Free a JSON string allocated by `uast_parse_uast`.
///
/// # Safety
///
/// - `json_ptr` must have been returned by `uast_parse_uast`
/// - `json_ptr` must not be used after this call
/// - Passing null is safe (no-op)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_free_json(json_ptr: *mut c_char) {
    if !json_ptr.is_null() {
        // SAFETY: Caller guarantees this pointer came from uast_parse_uast
        unsafe {
            drop(CString::from_raw(json_ptr));
        }
    }
}

/// Parse source code to typed UAST JSON using the new schema.
///
/// This function parses source code and returns a JSON representation using
/// the strongly-typed UAST schema (UastDocument with UastNode hierarchy).
///
/// # Arguments
///
/// * `language` - Null-terminated UTF-8 language name (must be registered or builtin)
/// * `source` - Null-terminated UTF-8 source code
/// * `source_path` - Optional null-terminated UTF-8 file path (can be null)
/// * `include_all_text` - If true, include source text for all nodes (not just leaves)
/// * `include_native_types` - If true, include native tree-sitter type names
/// * `out_json` - Pointer to receive the allocated JSON string
///
/// # Returns
///
/// 0 on success, non-zero error code on failure.
/// On success, `out_json` points to an allocated null-terminated string
/// that must be freed with `uast_free_json`.
///
/// # Safety
///
/// - `language` and `source` must be valid null-terminated UTF-8 strings
/// - `source_path` can be null or a valid null-terminated UTF-8 string
/// - `out_json` must be a valid pointer
/// - The caller must free the returned string with `uast_free_json`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_parse_to_typed_json(
    language: *const c_char,
    source: *const c_char,
    source_path: *const c_char,
    include_all_text: bool,
    include_native_types: bool,
    out_json: *mut *mut c_char,
) -> c_int {
    error::clear_last_error();

    if out_json.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return 1;
    }

    // Initialize output to null
    unsafe {
        *out_json = ptr::null_mut();
    }

    // Parse language string
    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return 1;
        }
    };

    // Parse source string
    let source_str = match unsafe { cstr_to_str(source) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("source"));
            return 2;
        }
    };

    // Parse optional source path
    let path_str: Option<&str> = if source_path.is_null() {
        None
    } else {
        match unsafe { cstr_to_str(source_path) } {
            Ok(s) => Some(s),
            Err(_) => None, // Treat invalid path as no path
        }
    };

    // Try builtin grammar first
    let (tree, grammar_source) = if let Some(builtin_lang) = get_builtin_language(lang_str) {
        let mut ts_parser = TsParser::new();
        if let Err(e) = ts_parser.set_language(&builtin_lang) {
            error::set_last_error(error::Error::internal(&format!(
                "Failed to set builtin language {}: {}",
                lang_str, e
            )));
            return 4;
        }

        match ts_parser.parse(source_str, None) {
            Some(tree) => (tree, "builtin"),
            None => {
                error::set_last_error(error::Error::internal(
                    "Failed to parse source with builtin grammar",
                ));
                return 4;
            }
        }
    } else {
        // Try registered language
        let mut parser = match Parser::new(lang_str) {
            Ok(p) => p,
            Err(e) => {
                error::set_last_error(e);
                return 4;
            }
        };

        match parser.parse(source_str) {
            Ok(parsed) => {
                // We need to get the tree from the parsed result
                // The ParsedTree owns the tree, so we can use its tree() method
                (parsed.tree().clone(), "registered")
            }
            Err(e) => {
                error::set_last_error(e);
                return 4;
            }
        }
    };

    // Create conversion options
    let options = ConvertOptions {
        include_all_text,
        include_anonymous: false,
        max_depth: 0,
        include_native_types,
    };

    // Create the UAST document with options
    let doc = crate::uast::create_uast_document_with_options(
        &tree,
        source_str,
        lang_str,
        path_str,
        grammar_source,
        options,
    );

    // Serialize to JSON
    match serde_json::to_string_pretty(&doc) {
        Ok(json_string) => match CString::new(json_string) {
            Ok(cstring) => {
                unsafe {
                    *out_json = cstring.into_raw();
                }
                0 // Success
            }
            Err(_) => {
                error::set_last_error(error::Error::internal("JSON contains null byte"));
                3
            }
        },
        Err(e) => {
            error::set_last_error(error::Error::internal(&format!(
                "Failed to serialize UAST: {}",
                e
            )));
            3
        }
    }
}

/// Get the UAST kind name for a native tree-sitter node type.
///
/// This performs the forward mapping from native types to UAST kinds.
///
/// # Arguments
///
/// * `language` - Null-terminated UTF-8 language name
/// * `native_type` - Null-terminated UTF-8 native tree-sitter node type
/// * `out_kind` - Pointer to receive the UAST kind name pointer
/// * `out_len` - Pointer to receive the kind name length
///
/// # Returns
///
/// 0 on success. The returned string is a static string that does not need to be freed.
///
/// # Safety
///
/// - `language` and `native_type` must be valid null-terminated UTF-8 strings
/// - `out_kind` and `out_len` must be valid pointers
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_get_kind_for_native(
    language: *const c_char,
    native_type: *const c_char,
    out_kind: *mut *const c_char,
    out_len: *mut u32,
) -> c_int {
    if out_kind.is_null() || out_len.is_null() {
        return 1;
    }

    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => return 1,
    };

    let native_str = match unsafe { cstr_to_str(native_type) } {
        Ok(s) => s,
        Err(_) => return 1,
    };

    let mappings = crate::uast::get_mappings(lang_str);
    let kind_str = mappings.get(native_str);

    unsafe {
        *out_kind = kind_str.as_ptr() as *const c_char;
        *out_len = kind_str.len() as u32;
    }

    0
}

/// Get native tree-sitter types for a UAST kind.
///
/// This performs the reverse mapping from UAST kinds to native types.
/// Returns a comma-separated list of native types.
///
/// # Arguments
///
/// * `language` - Null-terminated UTF-8 language name
/// * `uast_kind` - Null-terminated UTF-8 UAST kind name (e.g., "FunctionDeclaration")
/// * `out_types` - Pointer to receive the allocated comma-separated types string
///
/// # Returns
///
/// 0 on success. The caller must free the returned string with `uast_free_json`.
///
/// # Safety
///
/// - `language` and `uast_kind` must be valid null-terminated UTF-8 strings
/// - `out_types` must be a valid pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_get_native_types_for_kind(
    language: *const c_char,
    uast_kind: *const c_char,
    out_types: *mut *mut c_char,
) -> c_int {
    if out_types.is_null() {
        return 1;
    }

    unsafe {
        *out_types = ptr::null_mut();
    }

    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => return 1,
    };

    let kind_str = match unsafe { cstr_to_str(uast_kind) } {
        Ok(s) => s,
        Err(_) => return 1,
    };

    let native_types = crate::uast::get_native_types_for_uast(kind_str, lang_str);
    let result = native_types.join(",");

    match CString::new(result) {
        Ok(cstring) => {
            unsafe {
                *out_types = cstring.into_raw();
            }
            0
        }
        Err(_) => 2,
    }
}

/// Check if a pattern is a UAST pattern (PascalCase) vs native (snake_case).
///
/// # Arguments
///
/// * `pattern` - Null-terminated UTF-8 pattern string
///
/// # Returns
///
/// `true` if the pattern appears to be a UAST type (PascalCase), `false` otherwise.
///
/// # Safety
///
/// - `pattern` must be a valid null-terminated UTF-8 string, or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_is_uast_pattern(pattern: *const c_char) -> bool {
    let pattern_str = match unsafe { cstr_to_str(pattern) } {
        Ok(s) => s,
        Err(_) => return false,
    };

    crate::uast::is_uast_pattern(pattern_str)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_null_output_pointer() {
        let lang = CString::new("test").unwrap();
        let source = CString::new("test").unwrap();
        let result = unsafe {
            uast_parse_uast(
                lang.as_ptr(),
                source.as_ptr(),
                ptr::null(),
                ptr::null_mut(),
            )
        };
        assert_ne!(result, 0);
    }

    #[test]
    fn test_null_language() {
        let source = CString::new("test").unwrap();
        let mut out: *mut c_char = ptr::null_mut();
        let result = unsafe {
            uast_parse_uast(
                ptr::null(),
                source.as_ptr(),
                ptr::null(),
                &mut out,
            )
        };
        assert_ne!(result, 0);
        assert!(out.is_null());
    }

    #[test]
    fn test_null_source() {
        let lang = CString::new("test").unwrap();
        let mut out: *mut c_char = ptr::null_mut();
        let result = unsafe {
            uast_parse_uast(
                lang.as_ptr(),
                ptr::null(),
                ptr::null(),
                &mut out,
            )
        };
        assert_ne!(result, 0);
        assert!(out.is_null());
    }

    #[test]
    fn test_free_null() {
        // Should not crash
        unsafe {
            uast_free_json(ptr::null_mut());
        }
    }
}
