//! FFI exports for SARIF output.
//!
//! This module provides C ABI exports for converting scan results to SARIF format
//! from C#/.NET via P/Invoke.

use crate::error;
use crate::ffi::{cstr_to_str, cstr_to_str_len, str_to_cstring};
use crate::rules::{RuleYaml, ScanResult};
use crate::sarif::SarifWriter;
use std::ffi::c_char;
use std::ptr;

// ============================================================================
// SARIF Conversion Functions
// ============================================================================

/// Convert scan results JSON to SARIF JSON.
///
/// # Arguments
///
/// * `results_json` - JSON array of scan results
/// * `results_json_len` - Length of the results JSON string
/// * `rules_json` - JSON array of rule definitions (optional, can be empty "[]")
/// * `rules_json_len` - Length of the rules JSON string
/// * `tool_name` - The tool name for the SARIF output
/// * `tool_version` - The tool version for the SARIF output
///
/// # Returns
///
/// A SARIF JSON string. Caller must free with `uast_free_string`.
///
/// # Safety
///
/// - All string parameters must be valid pointers to at least the specified lengths
/// - `tool_name` and `tool_version` must be valid null-terminated UTF-8 strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scan_results_to_sarif(
    results_json: *const c_char,
    results_json_len: u32,
    rules_json: *const c_char,
    rules_json_len: u32,
    tool_name: *const c_char,
    tool_version: *const c_char,
) -> *mut c_char {
    error::clear_last_error();

    // Parse results JSON
    let results_str = match cstr_to_str_len(results_json, results_json_len) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let results: Vec<ScanResult> = match serde_json::from_str(results_str) {
        Ok(r) => r,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!(
                "Invalid results JSON: {}",
                e
            )));
            return ptr::null_mut();
        }
    };

    // Parse rules JSON
    let rules_str = match cstr_to_str_len(rules_json, rules_json_len) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let rules: Vec<RuleYaml> = match serde_json::from_str(rules_str) {
        Ok(r) => r,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("Invalid rules JSON: {}", e)));
            return ptr::null_mut();
        }
    };

    // Parse tool name and version
    let name_str = match cstr_to_str(tool_name) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let version_str = match cstr_to_str(tool_version) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    // Create SARIF output
    let writer = SarifWriter::new(name_str, version_str);
    let log = writer.from_scan_results(&results, &rules);

    // Serialize to JSON
    match writer.to_json_pretty(&log) {
        Ok(json) => str_to_cstring(&json),
        Err(e) => {
            error::set_last_error(error::Error::internal(format!(
                "Failed to serialize SARIF: {}",
                e
            )));
            ptr::null_mut()
        }
    }
}

/// Convert scan results JSON to SARIF JSON with default tool info.
///
/// Uses "UAST-Grep" as the tool name and "1.0.0" as the version.
///
/// # Arguments
///
/// * `results_json` - JSON array of scan results
/// * `results_json_len` - Length of the results JSON string
///
/// # Returns
///
/// A SARIF JSON string. Caller must free with `uast_free_string`.
///
/// # Safety
///
/// - `results_json` must be a valid pointer to at least `results_json_len` bytes
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scan_results_to_sarif_simple(
    results_json: *const c_char,
    results_json_len: u32,
) -> *mut c_char {
    error::clear_last_error();

    // Parse results JSON
    let results_str = match cstr_to_str_len(results_json, results_json_len) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let results: Vec<ScanResult> = match serde_json::from_str(results_str) {
        Ok(r) => r,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!(
                "Invalid results JSON: {}",
                e
            )));
            return ptr::null_mut();
        }
    };

    // Create SARIF output with defaults
    let writer = SarifWriter::new("UAST-Grep", "1.0.0");
    let log = writer.from_scan_results(&results, &[]);

    // Serialize to JSON
    match writer.to_json_pretty(&log) {
        Ok(json) => str_to_cstring(&json),
        Err(e) => {
            error::set_last_error(error::Error::internal(format!(
                "Failed to serialize SARIF: {}",
                e
            )));
            ptr::null_mut()
        }
    }
}

/// Scan with a scanner and return results directly as SARIF.
///
/// This is a convenience function that scans a UAST tree and returns
/// the results in SARIF format in one call.
///
/// # Arguments
///
/// * `scanner` - The scanner handle
/// * `uast_json` - JSON representation of the UAST tree
/// * `uast_json_len` - Length of the UAST JSON string
/// * `language` - The source language
/// * `file_path` - The file path for the SARIF output (optional, can be null)
/// * `tool_name` - The tool name for the SARIF output
/// * `tool_version` - The tool version for the SARIF output
///
/// # Returns
///
/// A SARIF JSON string. Caller must free with `uast_free_string`.
///
/// # Safety
///
/// - `scanner` must be a valid handle from `uast_scanner_new`
/// - All string parameters must be valid pointers as specified
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scanner_scan_to_sarif(
    scanner: *const crate::ffi_rules::UastScanner,
    uast_json: *const c_char,
    uast_json_len: u32,
    language: *const c_char,
    file_path: *const c_char,
    tool_name: *const c_char,
    tool_version: *const c_char,
) -> *mut c_char {
    error::clear_last_error();

    let id = scanner as usize;

    // Parse UAST JSON
    let json_str = match cstr_to_str_len(uast_json, uast_json_len) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let lang_str = match cstr_to_str(language) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    // Parse optional file path
    let path_str: Option<&str> = if file_path.is_null() {
        None
    } else {
        match cstr_to_str(file_path) {
            Ok(s) => Some(s),
            Err(_) => None,
        }
    };

    // Parse UAST tree
    let tree: crate::uast::schema::UastNode = match serde_json::from_str(json_str) {
        Ok(t) => t,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("Invalid UAST JSON: {}", e)));
            return ptr::null_mut();
        }
    };

    // Get scanner
    let scanners = crate::ffi_rules::SCANNERS.read();
    let scanner = match scanners.get(&id) {
        Some(s) => s,
        None => {
            error::set_last_error(error::Error::internal("Invalid scanner handle"));
            return ptr::null_mut();
        }
    };

    // Scan
    let mut results = scanner.scan_tree(&tree, lang_str);

    // Add file path to results if provided
    if let Some(path) = path_str {
        for result in &mut results {
            result.file_path = Some(path.to_string());
        }
    }

    // Parse tool info
    let name_str = match cstr_to_str(tool_name) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let version_str = match cstr_to_str(tool_version) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    // Convert to SARIF
    let writer = SarifWriter::new(name_str, version_str);
    let log = writer.from_scan_results(&results, &[]);

    // Serialize to JSON
    match writer.to_json_pretty(&log) {
        Ok(json) => str_to_cstring(&json),
        Err(e) => {
            error::set_last_error(error::Error::internal(format!(
                "Failed to serialize SARIF: {}",
                e
            )));
            ptr::null_mut()
        }
    }
}

/// Get the SARIF schema version supported by this library.
///
/// Returns "2.1.0".
///
/// # Safety
///
/// The returned pointer is valid for the lifetime of the program.
/// The caller must not attempt to free this pointer.
#[unsafe(no_mangle)]
pub extern "C" fn uast_sarif_version() -> *const c_char {
    static VERSION: &[u8] = b"2.1.0\0";
    VERSION.as_ptr() as *const c_char
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_sarif_version() {
        let version = uast_sarif_version();
        assert!(!version.is_null());

        let version_str = unsafe { std::ffi::CStr::from_ptr(version).to_str().unwrap() };
        assert_eq!(version_str, "2.1.0");
    }

    #[test]
    fn test_scan_results_to_sarif_simple() {
        let results_json = CString::new("[]").unwrap();

        let sarif = unsafe {
            uast_scan_results_to_sarif_simple(
                results_json.as_ptr(),
                results_json.as_bytes().len() as u32,
            )
        };

        assert!(!sarif.is_null());

        // Verify output
        let sarif_str = unsafe { std::ffi::CStr::from_ptr(sarif).to_str().unwrap() };
        assert!(sarif_str.contains("\"version\": \"2.1.0\""));
        assert!(sarif_str.contains("\"name\": \"UAST-Grep\""));

        // Free the string
        unsafe { crate::ffi::uast_free_string(sarif) };
    }

    #[test]
    fn test_scan_results_to_sarif_with_results() {
        // Test JSON parsing - requires captures field (even if empty)
        let results_str = r#"[
            {
                "rule_id": "test-rule",
                "severity": "warning",
                "message": "Test message",
                "location": {
                    "startLine": 10,
                    "startColumn": 5,
                    "endLine": 10,
                    "endColumn": 20,
                    "startOffset": 100,
                    "endOffset": 115
                },
                "file_path": "/path/to/file.ps1",
                "captures": {}
            }
        ]"#;

        // Parse directly in Rust to verify JSON structure
        let results: Vec<crate::rules::ScanResult> = serde_json::from_str(results_str)
            .expect("Failed to parse results JSON");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].rule_id, "test-rule");

        // Now test via FFI
        let results_json = CString::new(results_str).unwrap();
        let rules_json = CString::new("[]").unwrap();
        let tool_name = CString::new("Test-Tool").unwrap();
        let tool_version = CString::new("2.0.0").unwrap();

        let sarif = unsafe {
            uast_scan_results_to_sarif(
                results_json.as_ptr(),
                results_json.as_bytes().len() as u32,
                rules_json.as_ptr(),
                rules_json.as_bytes().len() as u32,
                tool_name.as_ptr(),
                tool_version.as_ptr(),
            )
        };

        if sarif.is_null() {
            // Get the error message
            let err = crate::error::take_last_error();
            panic!("SARIF conversion failed: {:?}", err);
        }

        let sarif_str = unsafe { std::ffi::CStr::from_ptr(sarif).to_str().unwrap() };
        assert!(sarif_str.contains("\"ruleId\": \"test-rule\""));
        assert!(sarif_str.contains("\"level\": \"warning\""));
        assert!(sarif_str.contains("\"name\": \"Test-Tool\""));

        unsafe { crate::ffi::uast_free_string(sarif) };
    }

    #[test]
    fn test_invalid_json() {
        let invalid_json = CString::new("not valid json").unwrap();

        let sarif = unsafe {
            uast_scan_results_to_sarif_simple(
                invalid_json.as_ptr(),
                invalid_json.as_bytes().len() as u32,
            )
        };

        assert!(sarif.is_null());
        assert!(error::has_last_error());
    }

    #[test]
    fn test_null_pointer_handling() {
        let sarif = unsafe { uast_scan_results_to_sarif_simple(ptr::null(), 0) };
        assert!(sarif.is_null());
    }
}
