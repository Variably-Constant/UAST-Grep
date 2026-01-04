//! FFI exports for pattern matching functionality.
//!
//! This module provides C ABI exports for pattern matching, allowing C#/.NET
//! to call into the Rust pattern matcher via P/Invoke.

use crate::error;
use crate::ffi::cstr_to_str;
use crate::matching::{
    parse_simple_pattern, compile_to_tree_sitter_query, MatchResult, Pattern, PatternMatcher,
};
use crate::uast::schema::UastNode;
use std::collections::HashMap;
use std::ffi::{c_char, c_int, CString};
use std::ptr;
use std::sync::Mutex;

// Handle storage for compiled patterns
lazy_static::lazy_static! {
    static ref PATTERN_HANDLES: Mutex<HashMap<usize, Pattern>> = Mutex::new(HashMap::new());
    static ref NEXT_PATTERN_HANDLE: Mutex<usize> = Mutex::new(1);
}

/// Parse and compile a pattern string.
///
/// # Arguments
///
/// * `pattern` - Null-terminated UTF-8 pattern string
/// * `language` - Null-terminated UTF-8 language name
/// * `out_handle` - Pointer to receive the pattern handle
///
/// # Returns
///
/// 0 on success, non-zero error code on failure.
/// On success, `out_handle` contains a handle that can be used with other pattern functions.
/// The handle must be freed with `uast_pattern_free`.
///
/// # Safety
///
/// - `pattern` and `language` must be valid null-terminated UTF-8 strings
/// - `out_handle` must be a valid pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_pattern_compile(
    pattern: *const c_char,
    language: *const c_char,
    out_handle: *mut usize,
) -> c_int {
    error::clear_last_error();

    if out_handle.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return 1;
    }

    unsafe {
        *out_handle = 0;
    }

    let pattern_str = match unsafe { cstr_to_str(pattern) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return 1;
        }
    };

    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("language"));
            return 2;
        }
    };

    // Parse the pattern
    match parse_simple_pattern(pattern_str, lang_str) {
        Ok(compiled) => {
            // Store the pattern and return handle
            let mut handles = PATTERN_HANDLES.lock().unwrap();
            let mut next = NEXT_PATTERN_HANDLE.lock().unwrap();
            let handle = *next;
            *next += 1;
            handles.insert(handle, compiled);

            unsafe {
                *out_handle = handle;
            }
            0
        }
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            3
        }
    }
}

/// Free a compiled pattern.
///
/// # Arguments
///
/// * `handle` - Pattern handle from `uast_pattern_compile`
///
/// # Safety
///
/// The handle must be valid (from a successful `uast_pattern_compile` call).
#[unsafe(no_mangle)]
pub extern "C" fn uast_pattern_free(handle: usize) {
    if handle != 0 {
        let mut handles = PATTERN_HANDLES.lock().unwrap();
        handles.remove(&handle);
    }
}

/// Compile a UAST pattern to a tree-sitter query string.
///
/// # Arguments
///
/// * `pattern` - Null-terminated UTF-8 pattern string
/// * `language` - Null-terminated UTF-8 language name
/// * `out_query` - Pointer to receive the query string
///
/// # Returns
///
/// 0 on success, non-zero error code on failure.
/// On success, `out_query` points to an allocated null-terminated string.
/// The string must be freed with `uast_free_string`.
///
/// # Safety
///
/// - `pattern` and `language` must be valid null-terminated UTF-8 strings
/// - `out_query` must be a valid pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_pattern_to_query(
    pattern: *const c_char,
    language: *const c_char,
    out_query: *mut *mut c_char,
) -> c_int {
    error::clear_last_error();

    if out_query.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return 1;
    }

    unsafe {
        *out_query = ptr::null_mut();
    }

    let pattern_str = match unsafe { cstr_to_str(pattern) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return 1;
        }
    };

    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("language"));
            return 2;
        }
    };

    // Parse and compile the pattern
    let compiled = match parse_simple_pattern(pattern_str, lang_str) {
        Ok(p) => p,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            return 3;
        }
    };

    // Compile to tree-sitter query
    match compile_to_tree_sitter_query(&compiled) {
        Ok(query_str) => match CString::new(query_str) {
            Ok(cstring) => {
                unsafe {
                    *out_query = cstring.into_raw();
                }
                0
            }
            Err(_) => {
                error::set_last_error(error::Error::internal("Query contains null bytes"));
                4
            }
        },
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            5
        }
    }
}

/// Match a pattern against UAST JSON and return matches as JSON.
///
/// # Arguments
///
/// * `pattern` - Null-terminated UTF-8 pattern string
/// * `language` - Null-terminated UTF-8 language name
/// * `uast_json` - Null-terminated UTF-8 UAST JSON string
/// * `out_matches_json` - Pointer to receive the matches JSON string
///
/// # Returns
///
/// 0 on success, non-zero error code on failure.
/// On success, `out_matches_json` points to a JSON array of match results.
/// The string must be freed with `uast_free_string`.
///
/// # Safety
///
/// - All string pointers must be valid null-terminated UTF-8 strings
/// - `out_matches_json` must be a valid pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_pattern_match(
    pattern: *const c_char,
    language: *const c_char,
    uast_json: *const c_char,
    out_matches_json: *mut *mut c_char,
) -> c_int {
    error::clear_last_error();

    if out_matches_json.is_null() {
        error::set_last_error(error::Error::null_pointer());
        return 1;
    }

    unsafe {
        *out_matches_json = ptr::null_mut();
    }

    let pattern_str = match unsafe { cstr_to_str(pattern) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return 1;
        }
    };

    let lang_str = match unsafe { cstr_to_str(language) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("language"));
            return 2;
        }
    };

    let uast_str = match unsafe { cstr_to_str(uast_json) } {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::invalid_utf8("uast_json"));
            return 3;
        }
    };

    // Parse the pattern
    let compiled = match parse_simple_pattern(pattern_str, lang_str) {
        Ok(p) => p,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            return 4;
        }
    };

    // Parse the UAST JSON
    let root: UastNode = match serde_json::from_str(uast_str) {
        Ok(node) => node,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("Invalid UAST JSON: {}", e)));
            return 5;
        }
    };

    // Find matches
    let matcher = PatternMatcher::new();
    let matches = matcher.find_all(&root, &compiled);

    // Serialize matches to JSON
    let match_results: Vec<MatchResultJson> = matches
        .into_iter()
        .map(|m| MatchResultJson::from(m))
        .collect();

    match serde_json::to_string(&match_results) {
        Ok(json) => match CString::new(json) {
            Ok(cstring) => {
                unsafe {
                    *out_matches_json = cstring.into_raw();
                }
                0
            }
            Err(_) => {
                error::set_last_error(error::Error::internal("JSON contains null bytes"));
                6
            }
        },
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("JSON serialization failed: {}", e)));
            7
        }
    }
}

// Note: Use uast_free_string from ffi.rs to free strings returned by these functions
// Note: Use uast_is_uast_pattern from ffi_uast.rs to check pattern types

// Simplified match result for JSON serialization
#[derive(serde::Serialize)]
struct MatchResultJson {
    #[serde(rename = "nodeKind")]
    node_kind: String,
    span: SpanJson,
    captures: HashMap<String, CaptureJson>,
}

#[derive(serde::Serialize)]
struct SpanJson {
    #[serde(rename = "startLine")]
    start_line: u32,
    #[serde(rename = "startColumn")]
    start_column: u32,
    #[serde(rename = "endLine")]
    end_line: u32,
    #[serde(rename = "endColumn")]
    end_column: u32,
}

#[derive(serde::Serialize)]
struct CaptureJson {
    count: usize,
    nodes: Vec<CapturedNodeJson>,
}

#[derive(serde::Serialize)]
struct CapturedNodeJson {
    kind: String,
    text: Option<String>,
    name: Option<String>,
}

impl From<MatchResult> for MatchResultJson {
    fn from(m: MatchResult) -> Self {
        let captures: HashMap<String, CaptureJson> = m
            .captures
            .into_iter()
            .map(|(name, value)| {
                let nodes: Vec<CapturedNodeJson> = value
                    .as_vec()
                    .into_iter()
                    .map(|n| CapturedNodeJson {
                        kind: n.kind.as_str().to_string(),
                        text: n.text.clone(),
                        name: n.name.clone(),
                    })
                    .collect();
                (
                    name,
                    CaptureJson {
                        count: nodes.len(),
                        nodes,
                    },
                )
            })
            .collect();

        MatchResultJson {
            node_kind: m.node.kind.as_str().to_string(),
            span: SpanJson {
                start_line: m.span.start_line,
                start_column: m.span.start_column,
                end_line: m.span.end_line,
                end_column: m.span.end_column,
            },
            captures,
        }
    }
}
