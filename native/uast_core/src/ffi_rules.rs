//! FFI exports for the YAML Rules Engine.
//!
//! This module provides C ABI exports for loading and executing YAML rules
//! from C#/.NET via P/Invoke.

use crate::error;
use crate::ffi::{cstr_to_str, cstr_to_str_len, str_to_cstring, UastResult};
use crate::rules::{
    apply_fixes, parse_rules_from_string, Fix, RuleLoader, RuleYaml, Scanner,
};
use crate::uast::schema::UastNode as SchemaUastNode;
use lazy_static::lazy_static;
use parking_lot::RwLock;
use serde_json;
use std::collections::HashMap;
use std::ffi::c_char;
use std::ptr;

// ============================================================================
// Handle Types
// ============================================================================

/// Opaque handle to a rule loader.
#[repr(C)]
pub struct UastRuleLoader {
    _private: [u8; 0],
}

/// Opaque handle to a scanner.
#[repr(C)]
pub struct UastScanner {
    _private: [u8; 0],
}

// ============================================================================
// Global Handle Storage (for FFI safety)
// ============================================================================

lazy_static! {
    static ref RULE_LOADERS: RwLock<HashMap<usize, RuleLoader>> = RwLock::new(HashMap::new());
    /// Global scanner storage - made pub(crate) for access from ffi_sarif
    pub(crate) static ref SCANNERS: RwLock<HashMap<usize, Scanner>> = RwLock::new(HashMap::new());
    static ref NEXT_LOADER_ID: RwLock<usize> = RwLock::new(1);
    static ref NEXT_SCANNER_ID: RwLock<usize> = RwLock::new(1);
}

fn next_loader_id() -> usize {
    let mut id = NEXT_LOADER_ID.write();
    let current = *id;
    *id += 1;
    current
}

fn next_scanner_id() -> usize {
    let mut id = NEXT_SCANNER_ID.write();
    let current = *id;
    *id += 1;
    current
}

// ============================================================================
// Rule Loader Functions
// ============================================================================

/// Create a new rule loader.
///
/// Returns a handle to the loader, or null on error.
///
/// # Safety
///
/// The returned handle must be freed with `uast_rule_loader_free`.
#[unsafe(no_mangle)]
pub extern "C" fn uast_rule_loader_new() -> *mut UastRuleLoader {
    let loader = RuleLoader::new();
    let id = next_loader_id();

    RULE_LOADERS.write().insert(id, loader);

    id as *mut UastRuleLoader
}

/// Load rules from a YAML string.
///
/// # Arguments
///
/// * `loader` - The rule loader handle
/// * `yaml` - The YAML content (null-terminated UTF-8 string)
///
/// # Returns
///
/// The number of rules loaded, or -1 on error.
///
/// # Safety
///
/// - `loader` must be a valid handle from `uast_rule_loader_new`
/// - `yaml` must be a valid null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_rule_loader_load_string(
    loader: *mut UastRuleLoader,
    yaml: *const c_char,
) -> i32 {
    error::clear_last_error();

    let id = loader as usize;
    let yaml_str = match cstr_to_str(yaml) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return -1;
        }
    };

    let mut loaders = RULE_LOADERS.write();
    let loader = match loaders.get_mut(&id) {
        Some(l) => l,
        None => {
            error::set_last_error(error::Error::internal("Invalid loader handle"));
            return -1;
        }
    };

    match loader.load_string(yaml_str) {
        Ok(count) => count as i32,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            -1
        }
    }
}

/// Load rules from a file path.
///
/// # Arguments
///
/// * `loader` - The rule loader handle
/// * `path` - The file path (null-terminated UTF-8 string)
///
/// # Returns
///
/// The number of rules loaded, or -1 on error.
///
/// # Safety
///
/// - `loader` must be a valid handle from `uast_rule_loader_new`
/// - `path` must be a valid null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_rule_loader_load_file(
    loader: *mut UastRuleLoader,
    path: *const c_char,
) -> i32 {
    error::clear_last_error();

    let id = loader as usize;
    let path_str = match cstr_to_str(path) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return -1;
        }
    };

    let mut loaders = RULE_LOADERS.write();
    let loader = match loaders.get_mut(&id) {
        Some(l) => l,
        None => {
            error::set_last_error(error::Error::internal("Invalid loader handle"));
            return -1;
        }
    };

    match loader.load_file(std::path::Path::new(path_str)) {
        Ok(count) => count as i32,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            -1
        }
    }
}

/// Load rules from a directory (recursively).
///
/// # Arguments
///
/// * `loader` - The rule loader handle
/// * `path` - The directory path (null-terminated UTF-8 string)
///
/// # Returns
///
/// The number of rules loaded, or -1 on error.
///
/// # Safety
///
/// - `loader` must be a valid handle from `uast_rule_loader_new`
/// - `path` must be a valid null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_rule_loader_load_directory(
    loader: *mut UastRuleLoader,
    path: *const c_char,
) -> i32 {
    error::clear_last_error();

    let id = loader as usize;
    let path_str = match cstr_to_str(path) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return -1;
        }
    };

    let mut loaders = RULE_LOADERS.write();
    let loader = match loaders.get_mut(&id) {
        Some(l) => l,
        None => {
            error::set_last_error(error::Error::internal("Invalid loader handle"));
            return -1;
        }
    };

    match loader.load_directory(std::path::Path::new(path_str)) {
        Ok(count) => count as i32,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            -1
        }
    }
}

/// Get the total number of loaded rules.
///
/// # Arguments
///
/// * `loader` - The rule loader handle
///
/// # Returns
///
/// The number of rules, or 0 if the handle is invalid.
///
/// # Safety
///
/// - `loader` must be a valid handle from `uast_rule_loader_new`
#[unsafe(no_mangle)]
pub extern "C" fn uast_rule_loader_count(loader: *const UastRuleLoader) -> u32 {
    let id = loader as usize;

    let loaders = RULE_LOADERS.read();
    match loaders.get(&id) {
        Some(l) => l.total_rules() as u32,
        None => 0,
    }
}

/// Get all rules for a specific language as JSON.
///
/// # Arguments
///
/// * `loader` - The rule loader handle
/// * `language` - The language name (null-terminated UTF-8 string)
///
/// # Returns
///
/// A JSON string containing the rules array. Caller must free with `uast_free_string`.
///
/// # Safety
///
/// - `loader` must be a valid handle from `uast_rule_loader_new`
/// - `language` must be a valid null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_rule_loader_rules_json(
    loader: *const UastRuleLoader,
    language: *const c_char,
) -> *mut c_char {
    let id = loader as usize;
    let lang_str = match cstr_to_str(language) {
        Ok(s) => s,
        Err(_) => return ptr::null_mut(),
    };

    let loaders = RULE_LOADERS.read();
    let loader = match loaders.get(&id) {
        Some(l) => l,
        None => return ptr::null_mut(),
    };

    let rules: Vec<&RuleYaml> = loader.rules_for_language(lang_str);
    let json = match serde_json::to_string(&rules) {
        Ok(s) => s,
        Err(_) => return ptr::null_mut(),
    };

    str_to_cstring(&json)
}

/// Free a rule loader.
///
/// # Safety
///
/// - `loader` must be a valid handle from `uast_rule_loader_new`, or null
/// - The loader must not be used after this call
#[unsafe(no_mangle)]
pub extern "C" fn uast_rule_loader_free(loader: *mut UastRuleLoader) {
    if loader.is_null() {
        return;
    }

    let id = loader as usize;
    RULE_LOADERS.write().remove(&id);
}

// ============================================================================
// Scanner Functions
// ============================================================================

/// Create a new scanner.
///
/// Returns a handle to the scanner, or null on error.
///
/// # Safety
///
/// The returned handle must be freed with `uast_scanner_free`.
#[unsafe(no_mangle)]
pub extern "C" fn uast_scanner_new() -> *mut UastScanner {
    let scanner = Scanner::new();
    let id = next_scanner_id();

    SCANNERS.write().insert(id, scanner);

    id as *mut UastScanner
}

/// Add rules to the scanner from a YAML string.
///
/// # Arguments
///
/// * `scanner` - The scanner handle
/// * `yaml` - The YAML content (null-terminated UTF-8 string)
///
/// # Returns
///
/// `UastResult::Ok` on success, error code otherwise.
///
/// # Safety
///
/// - `scanner` must be a valid handle from `uast_scanner_new`
/// - `yaml` must be a valid null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scanner_add_rules(
    scanner: *mut UastScanner,
    yaml: *const c_char,
) -> UastResult {
    error::clear_last_error();

    let id = scanner as usize;
    let yaml_str = match cstr_to_str(yaml) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return UastResult::NullPointer;
        }
    };

    // Parse YAML
    let rules = match parse_rules_from_string(yaml_str) {
        Ok(r) => r,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            return UastResult::InternalError;
        }
    };

    let mut scanners = SCANNERS.write();
    let scanner = match scanners.get_mut(&id) {
        Some(s) => s,
        None => {
            error::set_last_error(error::Error::internal("Invalid scanner handle"));
            return UastResult::InternalError;
        }
    };

    match scanner.add_rules(rules) {
        Ok(_) => UastResult::Ok,
        Err(e) => {
            error::set_last_error(error::Error::internal(e.to_string()));
            UastResult::InternalError
        }
    }
}

/// Get the number of rules in the scanner.
///
/// # Arguments
///
/// * `scanner` - The scanner handle
///
/// # Returns
///
/// The number of rules, or 0 if the handle is invalid.
///
/// # Safety
///
/// - `scanner` must be a valid handle from `uast_scanner_new`
#[unsafe(no_mangle)]
pub extern "C" fn uast_scanner_rule_count(scanner: *const UastScanner) -> u32 {
    let id = scanner as usize;

    let scanners = SCANNERS.read();
    match scanners.get(&id) {
        Some(s) => s.rule_count() as u32,
        None => 0,
    }
}

/// Scan source code (provided as UAST JSON) against loaded rules.
///
/// # Arguments
///
/// * `scanner` - The scanner handle
/// * `uast_json` - JSON representation of the UAST tree
/// * `uast_json_len` - Length of the JSON string
/// * `language` - The source language (null-terminated UTF-8 string)
///
/// # Returns
///
/// A JSON string containing the scan results array. Caller must free with `uast_free_string`.
///
/// # Safety
///
/// - `scanner` must be a valid handle from `uast_scanner_new`
/// - `uast_json` must be a valid pointer to at least `uast_json_len` bytes
/// - `language` must be a valid null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scanner_scan_json(
    scanner: *const UastScanner,
    uast_json: *const c_char,
    uast_json_len: u32,
    language: *const c_char,
) -> *mut c_char {
    error::clear_last_error();

    let id = scanner as usize;

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

    // Parse UAST JSON
    let tree: SchemaUastNode = match serde_json::from_str(json_str) {
        Ok(t) => t,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("Invalid UAST JSON: {}", e)));
            return ptr::null_mut();
        }
    };

    let scanners = SCANNERS.read();
    let scanner = match scanners.get(&id) {
        Some(s) => s,
        None => {
            error::set_last_error(error::Error::internal("Invalid scanner handle"));
            return ptr::null_mut();
        }
    };

    // Scan
    let results = scanner.scan_tree(&tree, lang_str);

    // Serialize results to JSON
    let results_json = match serde_json::to_string(&results) {
        Ok(s) => s,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("Failed to serialize results: {}", e)));
            return ptr::null_mut();
        }
    };

    str_to_cstring(&results_json)
}

/// Free a scanner.
///
/// # Safety
///
/// - `scanner` must be a valid handle from `uast_scanner_new`, or null
/// - The scanner must not be used after this call
#[unsafe(no_mangle)]
pub extern "C" fn uast_scanner_free(scanner: *mut UastScanner) {
    if scanner.is_null() {
        return;
    }

    let id = scanner as usize;
    SCANNERS.write().remove(&id);
}

// ============================================================================
// Fix Functions
// ============================================================================

/// Apply fixes to source code.
///
/// # Arguments
///
/// * `source` - The original source code (null-terminated UTF-8 string)
/// * `fixes_json` - JSON array of fixes to apply
/// * `fixes_json_len` - Length of the fixes JSON string
///
/// # Returns
///
/// The fixed source code. Caller must free with `uast_free_string`.
///
/// # Safety
///
/// - `source` must be a valid null-terminated UTF-8 string
/// - `fixes_json` must be a valid pointer to at least `fixes_json_len` bytes
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_apply_fixes(
    source: *const c_char,
    fixes_json: *const c_char,
    fixes_json_len: u32,
) -> *mut c_char {
    error::clear_last_error();

    let source_str = match cstr_to_str(source) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    let fixes_str = match cstr_to_str_len(fixes_json, fixes_json_len) {
        Ok(s) => s,
        Err(_) => {
            error::set_last_error(error::Error::null_pointer());
            return ptr::null_mut();
        }
    };

    // Parse fixes JSON
    let fixes: Vec<Fix> = match serde_json::from_str(fixes_str) {
        Ok(f) => f,
        Err(e) => {
            error::set_last_error(error::Error::internal(format!("Invalid fixes JSON: {}", e)));
            return ptr::null_mut();
        }
    };

    // Apply fixes
    let result = apply_fixes(source_str, fixes);

    str_to_cstring(&result.source)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_rule_loader_lifecycle() {
        let loader = uast_rule_loader_new();
        assert!(!loader.is_null());

        assert_eq!(uast_rule_loader_count(loader), 0);

        uast_rule_loader_free(loader);
    }

    #[test]
    fn test_rule_loader_load_string() {
        let loader = uast_rule_loader_new();

        let yaml = CString::new(r#"
id: test
language: rust
message: "Test"
rule:
  pattern: "test"
"#).unwrap();

        let count = unsafe { uast_rule_loader_load_string(loader, yaml.as_ptr()) };
        assert_eq!(count, 1);

        assert_eq!(uast_rule_loader_count(loader), 1);

        uast_rule_loader_free(loader);
    }

    #[test]
    fn test_scanner_lifecycle() {
        let scanner = uast_scanner_new();
        assert!(!scanner.is_null());

        assert_eq!(uast_scanner_rule_count(scanner), 0);

        uast_scanner_free(scanner);
    }

    #[test]
    fn test_scanner_add_rules() {
        let scanner = uast_scanner_new();

        let yaml = CString::new(r#"
id: test
language: rust
message: "Test"
rule:
  pattern: FunctionDeclaration
"#).unwrap();

        let result = unsafe { uast_scanner_add_rules(scanner, yaml.as_ptr()) };
        assert_eq!(result, UastResult::Ok);
        assert_eq!(uast_scanner_rule_count(scanner), 1);

        uast_scanner_free(scanner);
    }

    #[test]
    fn test_free_null_handles() {
        // Should not crash
        uast_rule_loader_free(ptr::null_mut());
        uast_scanner_free(ptr::null_mut());
    }
}
