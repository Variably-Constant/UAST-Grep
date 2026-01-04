//! FFI type definitions and helper functions for C interop.
//!
//! All types here use `#[repr(C)]` for stable ABI and are designed
//! to be safely passed across the C#/Rust boundary.
//!
//! # Design Principles
//!
//! - All structs use `#[repr(C)]` for predictable memory layout
//! - Opaque handles use zero-sized arrays to prevent direct access
//! - String pointers include length fields to avoid null-terminator scanning
//! - All public FFI functions use `extern "C"` and `#[unsafe(no_mangle)]`

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

// ============================================================================
// Result Codes
// ============================================================================

/// Result code for all FFI functions.
///
/// These codes must match the corresponding enum in C#.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UastResult {
    /// Operation succeeded
    Ok = 0,
    /// Null pointer passed where non-null required
    NullPointer = 1,
    /// Invalid UTF-8 in string argument
    InvalidUtf8 = 2,
    /// Unknown or unsupported language
    UnknownLanguage = 3,
    /// Parse failed (check error message)
    ParseFailed = 4,
    /// Query compilation failed
    QueryFailed = 5,
    /// Internal error
    InternalError = 99,
}

// ============================================================================
// Opaque Handle Types
// ============================================================================

/// Opaque handle to a parser instance.
///
/// The zero-sized array prevents construction outside this crate
/// while maintaining a stable C ABI.
#[repr(C)]
pub struct UastParser {
    _private: [u8; 0],
}

/// Opaque handle to a parsed tree.
///
/// Trees own their source text and provide node access.
#[repr(C)]
pub struct UastTree {
    _private: [u8; 0],
}

/// Opaque handle to a compiled query.
///
/// Queries can be reused across multiple trees of the same language.
#[repr(C)]
pub struct UastQuery {
    _private: [u8; 0],
}

/// Opaque handle to a query cursor (for iteration).
///
/// Cursors maintain iteration state during query execution.
#[repr(C)]
pub struct UastQueryCursor {
    _private: [u8; 0],
}

// ============================================================================
// Location Types
// ============================================================================

/// Source location (line/column, both 0-indexed).
///
/// Matches tree-sitter's Point type.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct UastPoint {
    /// 0-indexed row (line number)
    pub row: u32,
    /// 0-indexed column (byte offset within line)
    pub column: u32,
}

/// Byte range in source with line/column endpoints.
///
/// Provides both byte offsets (for slicing) and points (for display).
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct UastRange {
    /// Start byte offset (inclusive)
    pub start_byte: u32,
    /// End byte offset (exclusive)
    pub end_byte: u32,
    /// Start position (line/column)
    pub start_point: UastPoint,
    /// End position (line/column)
    pub end_point: UastPoint,
}

// ============================================================================
// Node Types
// ============================================================================

/// Node information returned from tree traversal.
///
/// This struct is designed for efficient marshaling across the FFI boundary.
/// String pointers are valid only while the owning tree is alive.
#[repr(C)]
#[derive(Clone)]
pub struct UastNode {
    /// Node type name (e.g., "function_declaration").
    ///
    /// Pointer valid until tree is freed. Do not free this pointer.
    pub kind: *const c_char,
    /// Length of kind string in bytes (not including null terminator)
    pub kind_len: u32,

    /// Field name if this node is a named field (e.g., "name", "body").
    ///
    /// Null if not a named field. Do not free this pointer.
    pub field_name: *const c_char,
    /// Length of field_name in bytes, 0 if field_name is null
    pub field_name_len: u32,

    /// Source range of this node
    pub range: UastRange,

    /// Total number of children (named and anonymous)
    pub child_count: u32,
    /// Number of named children only
    pub named_child_count: u32,

    /// True if this is a named node (not syntax like punctuation)
    pub is_named: bool,
    /// True if this node was inserted by error recovery
    pub is_missing: bool,
    /// True if this node or any descendant has a parse error
    pub has_error: bool,

    /// Internal ID for parent/child navigation.
    ///
    /// This is an opaque identifier, not a pointer.
    pub id: u64,
    /// Parent node ID, or 0 if this is the root
    pub parent_id: u64,
}

impl Default for UastNode {
    fn default() -> Self {
        Self {
            kind: ptr::null(),
            kind_len: 0,
            field_name: ptr::null(),
            field_name_len: 0,
            range: UastRange::default(),
            child_count: 0,
            named_child_count: 0,
            is_named: false,
            is_missing: false,
            has_error: false,
            id: 0,
            parent_id: 0,
        }
    }
}

// ============================================================================
// Query Match Types
// ============================================================================

/// Match result from query execution.
///
/// Contains the pattern that matched and all captured nodes.
#[repr(C)]
pub struct UastMatch {
    /// Index of the pattern that matched (for multi-pattern queries)
    pub pattern_index: u32,
    /// Array of captures from this match
    pub captures: *const UastCapture,
    /// Number of captures in the array
    pub capture_count: u32,
}

impl Default for UastMatch {
    fn default() -> Self {
        Self {
            pattern_index: 0,
            captures: ptr::null(),
            capture_count: 0,
        }
    }
}

/// Single capture within a match.
///
/// Associates a capture name (from @name in the query) with the captured node.
#[repr(C)]
pub struct UastCapture {
    /// Capture name (e.g., "name" from @name in the query).
    ///
    /// Do not free this pointer.
    pub name: *const c_char,
    /// Length of name in bytes
    pub name_len: u32,

    /// The captured node
    pub node: UastNode,
}

impl Default for UastCapture {
    fn default() -> Self {
        Self {
            name: ptr::null(),
            name_len: 0,
            node: UastNode::default(),
        }
    }
}

// ============================================================================
// Error Types
// ============================================================================

/// Error information for detailed diagnostics.
///
/// Used to communicate error details across the FFI boundary.
#[repr(C)]
pub struct UastError {
    /// Error code indicating the type of error
    pub code: UastResult,
    /// Human-readable error message.
    ///
    /// If non-null, must be freed with `uast_free_string`.
    pub message: *const c_char,
    /// Length of message in bytes
    pub message_len: u32,
    /// Location of error (for parse/query errors)
    pub location: UastPoint,
}

impl Default for UastError {
    fn default() -> Self {
        Self {
            code: UastResult::Ok,
            message: ptr::null(),
            message_len: 0,
            location: UastPoint::default(),
        }
    }
}

// ============================================================================
// Language Information
// ============================================================================

/// Language information for discovery and documentation.
#[repr(C)]
pub struct UastLanguageInfo {
    /// Language name (e.g., "javascript", "python")
    pub name: *const c_char,
    /// Length of name in bytes
    pub name_len: u32,
    /// Array of file extensions this language handles
    pub extensions: *const *const c_char,
    /// Number of extensions
    pub extension_count: u32,
    /// Number of node types in this language's grammar
    pub node_type_count: u32,
}

impl Default for UastLanguageInfo {
    fn default() -> Self {
        Self {
            name: ptr::null(),
            name_len: 0,
            extensions: ptr::null(),
            extension_count: 0,
            node_type_count: 0,
        }
    }
}

// ============================================================================
// Callback Types
// ============================================================================

/// Callback type for node iteration.
///
/// Return `true` to continue iteration, `false` to stop.
pub type NodeCallback = extern "C" fn(node: *const UastNode, user_data: *mut c_void) -> bool;

/// Callback type for query matches.
///
/// Return `true` to continue iteration, `false` to stop.
pub type MatchCallback = extern "C" fn(match_: *const UastMatch, user_data: *mut c_void) -> bool;

// ============================================================================
// Scanner Types (re-exported from scanner module)
// ============================================================================

// Scanner types (FileMatch, ScanOptions, ScanStats, FileMatchCallback) are defined
// in the scanner module with #[repr(C)] for FFI compatibility. They are re-exported
// via `pub mod scanner` in lib.rs.

// ============================================================================
// Helper Functions for C String Handling
// ============================================================================

/// Convert a C string pointer to a Rust `&str`.
///
/// # Safety
///
/// - `ptr` must be a valid pointer to a null-terminated UTF-8 string
/// - The string must remain valid for the lifetime `'a`
///
/// # Errors
///
/// Returns `Err(UastResult::NullPointer)` if `ptr` is null.
/// Returns `Err(UastResult::InvalidUtf8)` if the string is not valid UTF-8.
pub unsafe fn cstr_to_str<'a>(ptr: *const c_char) -> Result<&'a str, UastResult> {
    if ptr.is_null() {
        return Err(UastResult::NullPointer);
    }
    // SAFETY: Caller guarantees ptr is valid and null-terminated
    unsafe {
        CStr::from_ptr(ptr)
            .to_str()
            .map_err(|_| UastResult::InvalidUtf8)
    }
}

/// Convert a C string with known length to a Rust `&str`.
///
/// This is more efficient than `cstr_to_str` when the length is known,
/// as it avoids scanning for the null terminator.
///
/// # Safety
///
/// - `ptr` must be a valid pointer to at least `len` bytes
/// - The bytes must be valid UTF-8
/// - The memory must remain valid for the lifetime `'a`
///
/// # Errors
///
/// Returns `Err(UastResult::NullPointer)` if `ptr` is null.
/// Returns `Err(UastResult::InvalidUtf8)` if the bytes are not valid UTF-8.
pub unsafe fn cstr_to_str_len<'a>(ptr: *const c_char, len: u32) -> Result<&'a str, UastResult> {
    if ptr.is_null() {
        return Err(UastResult::NullPointer);
    }
    // SAFETY: Caller guarantees ptr is valid for len bytes
    let slice = unsafe { std::slice::from_raw_parts(ptr as *const u8, len as usize) };
    std::str::from_utf8(slice).map_err(|_| UastResult::InvalidUtf8)
}

/// Allocate a C string that must be freed with `uast_free_string`.
///
/// # Returns
///
/// A pointer to a newly allocated null-terminated C string,
/// or null if the string contains interior null bytes.
pub fn str_to_cstring(s: &str) -> *mut c_char {
    match CString::new(s) {
        Ok(cs) => cs.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

/// Free a string allocated by this library.
///
/// # Safety
///
/// - `ptr` must have been allocated by `str_to_cstring` or another
///   function in this library that documents returning an owned string.
/// - `ptr` must not be used after this call.
/// - Passing a null pointer is safe (no-op).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_free_string(ptr: *mut c_char) {
    if !ptr.is_null() {
        // SAFETY: Caller guarantees ptr was allocated by str_to_cstring
        // Reconstruct the CString and drop it to free the memory
        unsafe {
            drop(CString::from_raw(ptr));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uast_result_values() {
        // Verify enum values match C# expectations
        assert_eq!(UastResult::Ok as i32, 0);
        assert_eq!(UastResult::NullPointer as i32, 1);
        assert_eq!(UastResult::InvalidUtf8 as i32, 2);
        assert_eq!(UastResult::UnknownLanguage as i32, 3);
        assert_eq!(UastResult::ParseFailed as i32, 4);
        assert_eq!(UastResult::QueryFailed as i32, 5);
        assert_eq!(UastResult::InternalError as i32, 99);
    }

    #[test]
    fn test_cstr_to_str() {
        let test_str = CString::new("hello").unwrap();
        unsafe {
            let result = cstr_to_str(test_str.as_ptr());
            assert_eq!(result, Ok("hello"));
        }
    }

    #[test]
    fn test_cstr_to_str_null() {
        unsafe {
            let result = cstr_to_str(ptr::null());
            assert_eq!(result, Err(UastResult::NullPointer));
        }
    }

    #[test]
    fn test_cstr_to_str_len() {
        let test_bytes = b"hello world";
        unsafe {
            let result = cstr_to_str_len(test_bytes.as_ptr() as *const c_char, 5);
            assert_eq!(result, Ok("hello"));
        }
    }

    #[test]
    fn test_str_to_cstring() {
        let ptr = str_to_cstring("test");
        assert!(!ptr.is_null());
        unsafe {
            let result = CString::from_raw(ptr);
            assert_eq!(result.to_str().unwrap(), "test");
        }
    }

    #[test]
    fn test_str_to_cstring_with_null() {
        let ptr = str_to_cstring("test\0embedded");
        assert!(ptr.is_null());
    }

    #[test]
    fn test_default_structs() {
        // Verify all default implementations work
        let _point = UastPoint::default();
        let _range = UastRange::default();
        let _node = UastNode::default();
        let _match = UastMatch::default();
        let _capture = UastCapture::default();
        let _error = UastError::default();
        let _info = UastLanguageInfo::default();
    }

    #[test]
    fn test_struct_sizes() {
        // These tests help catch ABI breakage
        // The exact sizes depend on padding, but they should be stable

        // UastPoint: 2 * u32 = 8 bytes
        assert_eq!(std::mem::size_of::<UastPoint>(), 8);

        // UastRange: 2 * u32 + 2 * UastPoint = 8 + 16 = 24 bytes
        assert_eq!(std::mem::size_of::<UastRange>(), 24);
    }
}
