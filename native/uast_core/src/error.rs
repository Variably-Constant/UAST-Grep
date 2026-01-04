//! Error handling types for UAST Core.
//!
//! This module provides error types using `thiserror` for ergonomic
//! error handling in Rust, with conversion to FFI-safe error types.

use crate::ffi::{UastError, UastPoint, UastResult};
use std::ffi::c_char;
use thiserror::Error;

/// Main error type for UAST operations.
///
/// This enum covers all error cases that can occur during parsing,
/// query compilation, and tree traversal.
#[derive(Error, Debug)]
pub enum Error {
    /// A null pointer was passed where non-null was required
    #[error("null pointer")]
    NullPointer,

    /// A string argument contained invalid UTF-8
    #[error("invalid UTF-8: {0}")]
    InvalidUtf8(String),

    /// The requested language is not supported or not compiled in
    #[error("unknown language: {0}")]
    UnknownLanguage(String),

    /// Parsing failed
    #[error("parse failed: {0}")]
    ParseFailed(String),

    /// Query compilation failed
    #[error("query failed at row {row}, column {column}: {message}")]
    QueryFailed {
        /// Error message from tree-sitter
        message: String,
        /// Row (0-indexed) where error occurred
        row: u32,
        /// Column (0-indexed) where error occurred
        column: u32,
    },

    /// An internal error occurred (should not happen in normal operation)
    #[error("internal error: {0}")]
    Internal(String),
}

impl Error {
    /// Create a null pointer error.
    pub fn null_pointer() -> Self {
        Error::NullPointer
    }

    /// Create an invalid UTF-8 error with context.
    pub fn invalid_utf8(context: impl Into<String>) -> Self {
        Error::InvalidUtf8(context.into())
    }

    /// Create an unknown language error.
    pub fn unknown_language(lang: impl Into<String>) -> Self {
        Error::UnknownLanguage(lang.into())
    }

    /// Create a parse failed error.
    pub fn parse_failed(message: impl Into<String>) -> Self {
        Error::ParseFailed(message.into())
    }

    /// Create a query failed error with location.
    pub fn query_failed(message: impl Into<String>, row: u32, column: u32) -> Self {
        Error::QueryFailed {
            message: message.into(),
            row,
            column,
        }
    }

    /// Create an internal error.
    pub fn internal(message: impl Into<String>) -> Self {
        Error::Internal(message.into())
    }

    /// Get the result code for this error.
    pub fn result_code(&self) -> UastResult {
        match self {
            Error::NullPointer => UastResult::NullPointer,
            Error::InvalidUtf8(_) => UastResult::InvalidUtf8,
            Error::UnknownLanguage(_) => UastResult::UnknownLanguage,
            Error::ParseFailed(_) => UastResult::ParseFailed,
            Error::QueryFailed { .. } => UastResult::QueryFailed,
            Error::Internal(_) => UastResult::InternalError,
        }
    }

    /// Get the error location, if applicable.
    pub fn location(&self) -> UastPoint {
        match self {
            Error::QueryFailed { row, column, .. } => UastPoint {
                row: *row,
                column: *column,
            },
            _ => UastPoint::default(),
        }
    }

    /// Convert this error to an FFI-safe `UastError`.
    ///
    /// The returned error's message pointer must be freed with `uast_free_string`.
    pub fn to_ffi(&self) -> UastError {
        let message = self.to_string();
        let message_ptr = crate::ffi::str_to_cstring(&message);
        let message_len = if message_ptr.is_null() {
            0
        } else {
            message.len() as u32
        };

        UastError {
            code: self.result_code(),
            message: message_ptr,
            message_len,
            location: self.location(),
        }
    }
}

/// Result type alias using the UAST error type.
pub type Result<T> = std::result::Result<T, Error>;

// ============================================================================
// Thread-Local Last Error Storage
// ============================================================================

use std::cell::RefCell;

thread_local! {
    /// Thread-local storage for the last error.
    ///
    /// This allows C# code to retrieve error details after a function returns
    /// an error code. The error is stored per-thread to be thread-safe.
    static LAST_ERROR: RefCell<Option<Error>> = const { RefCell::new(None) };
}

/// Store an error as the last error for this thread.
///
/// This is called internally when an FFI function encounters an error.
pub fn set_last_error(error: Error) {
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = Some(error);
    });
}

/// Clear the last error for this thread.
///
/// Called at the start of FFI functions to ensure stale errors are not returned.
pub fn clear_last_error() {
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = None;
    });
}

/// Take the last error, leaving `None` in its place.
///
/// This is used by `uast_last_error` to retrieve and clear the error.
pub fn take_last_error() -> Option<Error> {
    LAST_ERROR.with(|cell| cell.borrow_mut().take())
}

/// Check if there is a last error stored.
pub fn has_last_error() -> bool {
    LAST_ERROR.with(|cell| cell.borrow().is_some())
}

// ============================================================================
// FFI Functions for Error Retrieval
// ============================================================================

/// Get the last error that occurred on this thread.
///
/// Returns a `UastError` structure with error details. If there is no error,
/// returns a structure with `code = UastResult::Ok`.
///
/// The error is cleared after this call.
///
/// # Safety
///
/// - `out_error` must be a valid pointer to a `UastError` structure.
/// - If the returned error has a non-null message, it must be freed with
///   `uast_free_string`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_last_error(out_error: *mut UastError) -> UastResult {
    if out_error.is_null() {
        return UastResult::NullPointer;
    }

    match take_last_error() {
        Some(error) => {
            // SAFETY: Caller guarantees out_error is valid
            unsafe {
                *out_error = error.to_ffi();
            }
            error.result_code()
        }
        None => {
            // SAFETY: Caller guarantees out_error is valid
            unsafe {
                *out_error = UastError::default();
            }
            UastResult::Ok
        }
    }
}

/// Check if there is an error stored for this thread.
///
/// Returns `true` if there is an error, `false` otherwise.
/// Does not clear the error.
#[unsafe(no_mangle)]
pub extern "C" fn uast_has_error() -> bool {
    has_last_error()
}

/// Clear the last error for this thread.
///
/// This is useful to reset error state before a series of operations.
#[unsafe(no_mangle)]
pub extern "C" fn uast_clear_error() {
    clear_last_error();
}

/// Free a `UastError` structure's message.
///
/// This is a convenience function that frees the message pointer if non-null.
///
/// # Safety
///
/// - `error` must be a valid pointer to a `UastError` structure.
/// - The message pointer must have been allocated by this library.
/// - The message pointer must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_error_free(error: *mut UastError) {
    if error.is_null() {
        return;
    }

    // SAFETY: Caller guarantees error is valid
    unsafe {
        let error = &mut *error;
        if !error.message.is_null() {
            // Free the message string
            crate::ffi::uast_free_string(error.message as *mut c_char);
            error.message = std::ptr::null();
            error.message_len = 0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = Error::null_pointer();
        assert_eq!(err.result_code(), UastResult::NullPointer);

        let err = Error::unknown_language("foo");
        assert_eq!(err.result_code(), UastResult::UnknownLanguage);
        assert!(err.to_string().contains("foo"));

        let err = Error::query_failed("syntax error", 10, 5);
        assert_eq!(err.result_code(), UastResult::QueryFailed);
        let loc = err.location();
        assert_eq!(loc.row, 10);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_last_error_storage() {
        // Clear any existing error
        clear_last_error();
        assert!(!has_last_error());

        // Set an error
        set_last_error(Error::unknown_language("test"));
        assert!(has_last_error());

        // Take the error
        let err = take_last_error();
        assert!(err.is_some());
        assert!(!has_last_error());

        // Take again should return None
        let err = take_last_error();
        assert!(err.is_none());
    }

    #[test]
    fn test_error_to_ffi() {
        let err = Error::parse_failed("unexpected token");
        let ffi_err = err.to_ffi();

        assert_eq!(ffi_err.code, UastResult::ParseFailed);
        assert!(!ffi_err.message.is_null());

        // Clean up
        unsafe {
            uast_error_free(&mut { ffi_err } as *mut _);
        }
    }

    #[test]
    fn test_uast_last_error() {
        clear_last_error();

        let mut out_error = UastError::default();
        unsafe {
            // No error case
            let result = uast_last_error(&mut out_error);
            assert_eq!(result, UastResult::Ok);
            assert!(out_error.message.is_null());

            // Set and retrieve error
            set_last_error(Error::internal("test error"));
            let result = uast_last_error(&mut out_error);
            assert_eq!(result, UastResult::InternalError);
            assert!(!out_error.message.is_null());

            // Clean up
            uast_error_free(&mut out_error);

            // Error should be cleared
            assert!(!has_last_error());
        }
    }

    #[test]
    fn test_uast_clear_error() {
        set_last_error(Error::null_pointer());
        assert!(has_last_error());

        uast_clear_error();
        assert!(!has_last_error());
    }
}
