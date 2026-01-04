//! Grammar verification module for UAST-Grep.
//!
//! Provides functionality to verify that grammar files (WASM and native DLLs)
//! are valid and loadable before use.

use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::builtin_languages::get_builtin_language;
use crate::dynamic_loader;
use crate::tiers::{BUILTIN_LANGUAGES, WASM_LANGUAGES};
use crate::wasm_loader;

// ============================================================================
// Verification Result Types
// ============================================================================

/// The source/type of a grammar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GrammarSource {
    /// Built-in grammar (compiled into the binary).
    Builtin,
    /// WASM grammar (loaded from .wasm file).
    Wasm,
    /// Native DLL grammar (loaded from .dll file).
    Native,
    /// Unknown source.
    Unknown,
}

impl fmt::Display for GrammarSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GrammarSource::Builtin => write!(f, "builtin"),
            GrammarSource::Wasm => write!(f, "wasm"),
            GrammarSource::Native => write!(f, "native"),
            GrammarSource::Unknown => write!(f, "unknown"),
        }
    }
}

/// Status of a grammar verification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationStatus {
    /// Grammar is valid and loadable.
    Ok,
    /// Grammar file is missing.
    Missing,
    /// Grammar file exists but is invalid.
    Invalid(String),
    /// Grammar could not be loaded.
    LoadError(String),
}

impl VerificationStatus {
    pub fn is_ok(&self) -> bool {
        matches!(self, VerificationStatus::Ok)
    }
}

impl fmt::Display for VerificationStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VerificationStatus::Ok => write!(f, "OK"),
            VerificationStatus::Missing => write!(f, "MISSING"),
            VerificationStatus::Invalid(msg) => write!(f, "INVALID: {}", msg),
            VerificationStatus::LoadError(msg) => write!(f, "ERROR: {}", msg),
        }
    }
}

/// Result of verifying a single grammar.
#[derive(Debug, Clone)]
pub struct GrammarVerification {
    /// Name of the grammar/language.
    pub name: String,
    /// Source type of the grammar.
    pub source: GrammarSource,
    /// Verification status.
    pub status: VerificationStatus,
    /// File path (if applicable).
    pub path: Option<PathBuf>,
    /// File size in bytes (if applicable).
    pub size_bytes: Option<u64>,
    /// Additional details.
    pub details: Option<String>,
}

impl GrammarVerification {
    pub fn new(name: impl Into<String>, source: GrammarSource, status: VerificationStatus) -> Self {
        Self {
            name: name.into(),
            source,
            status,
            path: None,
            size_bytes: None,
            details: None,
        }
    }

    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = Some(path);
        self
    }

    pub fn with_size(mut self, size: u64) -> Self {
        self.size_bytes = Some(size);
        self
    }

    pub fn with_details(mut self, details: impl Into<String>) -> Self {
        self.details = Some(details.into());
        self
    }

    /// Format size as human-readable string.
    pub fn size_display(&self) -> String {
        match self.size_bytes {
            Some(bytes) if bytes >= 1_000_000 => format!("{:.1} MB", bytes as f64 / 1_000_000.0),
            Some(bytes) if bytes >= 1_000 => format!("{:.1} KB", bytes as f64 / 1_000.0),
            Some(bytes) => format!("{} B", bytes),
            None => String::new(),
        }
    }
}

/// Summary of all grammar verifications.
#[derive(Debug, Clone)]
pub struct VerificationSummary {
    pub results: Vec<GrammarVerification>,
    pub builtin_count: usize,
    pub wasm_count: usize,
    pub native_count: usize,
    pub ok_count: usize,
    pub missing_count: usize,
    pub error_count: usize,
}

impl VerificationSummary {
    pub fn new(results: Vec<GrammarVerification>) -> Self {
        let builtin_count = results.iter().filter(|r| r.source == GrammarSource::Builtin).count();
        let wasm_count = results.iter().filter(|r| r.source == GrammarSource::Wasm).count();
        let native_count = results.iter().filter(|r| r.source == GrammarSource::Native).count();
        let ok_count = results.iter().filter(|r| r.status.is_ok()).count();
        let missing_count = results.iter().filter(|r| matches!(r.status, VerificationStatus::Missing)).count();
        let error_count = results.iter().filter(|r| matches!(r.status, VerificationStatus::Invalid(_) | VerificationStatus::LoadError(_))).count();

        Self {
            results,
            builtin_count,
            wasm_count,
            native_count,
            ok_count,
            missing_count,
            error_count,
        }
    }

    pub fn total(&self) -> usize {
        self.results.len()
    }

    pub fn all_ok(&self) -> bool {
        self.error_count == 0 && self.missing_count == 0
    }
}

// ============================================================================
// WASM Verification
// ============================================================================

/// WASM magic bytes: \0asm
const WASM_MAGIC: &[u8] = b"\x00asm";

/// Verify a WASM file by checking its magic bytes.
pub fn verify_wasm_magic(path: &Path) -> Result<(), String> {
    if !path.exists() {
        return Err("File does not exist".to_string());
    }

    let bytes = fs::read(path).map_err(|e| format!("Failed to read file: {}", e))?;

    if bytes.len() < 4 {
        return Err("File too small to be valid WASM".to_string());
    }

    if &bytes[0..4] != WASM_MAGIC {
        return Err(format!(
            "Invalid WASM magic bytes: got {:02x} {:02x} {:02x} {:02x}, expected 00 61 73 6d",
            bytes[0], bytes[1], bytes[2], bytes[3]
        ));
    }

    Ok(())
}

/// Verify a WASM grammar file.
pub fn verify_wasm_file(name: &str, path: &Path) -> GrammarVerification {
    let mut result = GrammarVerification::new(name, GrammarSource::Wasm, VerificationStatus::Ok);

    if !path.exists() {
        result.status = VerificationStatus::Missing;
        return result;
    }

    result = result.with_path(path.to_path_buf());

    // Get file size
    if let Ok(metadata) = fs::metadata(path) {
        result = result.with_size(metadata.len());
    }

    // Verify magic bytes
    if let Err(e) = verify_wasm_magic(path) {
        result.status = VerificationStatus::Invalid(e);
        return result;
    }

    // Try to load it (if WASM feature is enabled)
    #[cfg(feature = "wasm")]
    {
        match fs::read(path) {
            Ok(bytes) => {
                // We can't easily test the full load without the WASM store
                // Just verify the file is readable and has valid header
                result.details = Some(format!("{} bytes", bytes.len()));
            }
            Err(e) => {
                result.status = VerificationStatus::LoadError(format!("Failed to read: {}", e));
            }
        }
    }

    result
}

/// Verify a WASM grammar by name, searching in standard locations.
pub fn verify_wasm_grammar(name: &str) -> GrammarVerification {
    // Check if it's a known WASM language
    let name_lower = name.to_lowercase();
    if !crate::tiers::is_wasm(&name_lower) {
        return GrammarVerification::new(
            name,
            GrammarSource::Wasm,
            VerificationStatus::Invalid(format!("'{}' is not a known WASM language", name)),
        );
    }

    // Search for the WASM file
    if let Some(path) = find_wasm_file(&name_lower) {
        verify_wasm_file(name, &path)
    } else {
        // Check if it can be downloaded
        let mut result = GrammarVerification::new(name, GrammarSource::Wasm, VerificationStatus::Missing);
        result.details = Some("Not cached locally (can be downloaded)".to_string());
        result
    }
}

/// Find a WASM file in standard search locations.
fn find_wasm_file(name: &str) -> Option<PathBuf> {
    let filename = format!("{}.wasm", name);

    // Check standard locations
    for search_dir in wasm_search_paths() {
        let path = search_dir.join(&filename);
        if path.exists() {
            return Some(path);
        }
    }

    None
}

/// Get WASM search paths.
fn wasm_search_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // Next to executable
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            paths.push(exe_dir.join("grammars-wasm"));
        }
    }

    // Environment variable
    if let Ok(wasm_path) = std::env::var("UAST_WASM_PATH") {
        paths.push(PathBuf::from(wasm_path));
    }

    // User cache
    if let Some(cache_dir) = wasm_loader::get_cache_dir() {
        paths.push(cache_dir);
    }

    // Working directory
    paths.push(PathBuf::from("grammars-wasm"));

    paths
}

// ============================================================================
// Native DLL Verification
// ============================================================================

/// Verify a native DLL grammar file.
pub fn verify_native_dll(name: &str, path: &Path) -> GrammarVerification {
    let mut result = GrammarVerification::new(name, GrammarSource::Native, VerificationStatus::Ok);

    if !path.exists() {
        result.status = VerificationStatus::Missing;
        return result;
    }

    result = result.with_path(path.to_path_buf());

    // Get file size
    if let Ok(metadata) = fs::metadata(path) {
        result = result.with_size(metadata.len());
    }

    // Try to load the DLL and find the entry point
    match dynamic_loader::load_grammar_dll(name, path) {
        Ok(lang) => {
            // Verify the language is usable
            let node_kinds = lang.node_kind_count();
            result.details = Some(format!("{} node kinds", node_kinds));
        }
        Err(e) => {
            result.status = VerificationStatus::LoadError(e.to_string());
        }
    }

    result
}

/// Verify a native grammar by name, searching in standard locations.
pub fn verify_native_grammar(name: &str) -> GrammarVerification {
    // Search for the DLL
    if let Some(path) = dynamic_loader::find_grammar_dll(name) {
        verify_native_dll(name, &path)
    } else {
        GrammarVerification::new(name, GrammarSource::Native, VerificationStatus::Missing)
    }
}

// ============================================================================
// Built-in Verification
// ============================================================================

/// Verify a built-in grammar.
pub fn verify_builtin_grammar(name: &str) -> GrammarVerification {
    match get_builtin_language(name) {
        Some(lang) => {
            let mut result = GrammarVerification::new(name, GrammarSource::Builtin, VerificationStatus::Ok);
            let node_kinds = lang.node_kind_count();
            result.details = Some(format!("{} node kinds", node_kinds));
            result
        }
        None => GrammarVerification::new(
            name,
            GrammarSource::Builtin,
            VerificationStatus::Invalid(format!("'{}' is not a built-in language", name)),
        ),
    }
}

// ============================================================================
// Unified Verification
// ============================================================================

/// Verify a grammar by name, auto-detecting its type.
pub fn verify_grammar(name: &str) -> GrammarVerification {
    let name_lower = name.to_lowercase();

    // Check built-in first
    if crate::tiers::is_builtin(&name_lower) {
        return verify_builtin_grammar(name);
    }

    // Check WASM
    if crate::tiers::is_wasm(&name_lower) {
        return verify_wasm_grammar(name);
    }

    // Check if there's a native DLL
    if dynamic_loader::has_grammar_dll(&name_lower) {
        return verify_native_grammar(name);
    }

    // Unknown grammar
    GrammarVerification::new(name, GrammarSource::Unknown, VerificationStatus::Missing)
}

/// Verify all known grammars (built-in + WASM + discovered native).
pub fn verify_all_grammars() -> VerificationSummary {
    let mut results = Vec::new();

    // Verify all built-in grammars
    for &name in BUILTIN_LANGUAGES {
        results.push(verify_builtin_grammar(name));
    }

    // Verify all WASM grammars
    for &name in WASM_LANGUAGES {
        results.push(verify_wasm_grammar(name));
    }

    // Discover and verify native DLLs
    let native_grammars = dynamic_loader::available_dynamic_grammars();
    for name in native_grammars {
        // Skip if already covered by built-in or WASM
        if crate::tiers::is_builtin(&name) || crate::tiers::is_wasm(&name) {
            continue;
        }
        results.push(verify_native_grammar(&name));
    }

    // Sort by name
    results.sort_by(|a, b| a.name.cmp(&b.name));

    VerificationSummary::new(results)
}

/// Verify only built-in grammars.
pub fn verify_builtin_grammars() -> VerificationSummary {
    let results: Vec<_> = BUILTIN_LANGUAGES
        .iter()
        .map(|&name| verify_builtin_grammar(name))
        .collect();

    VerificationSummary::new(results)
}

/// Verify only WASM grammars (that are locally cached).
pub fn verify_wasm_grammars() -> VerificationSummary {
    let results: Vec<_> = WASM_LANGUAGES
        .iter()
        .map(|&name| verify_wasm_grammar(name))
        .collect();

    VerificationSummary::new(results)
}

/// Verify only native DLL grammars.
pub fn verify_native_grammars() -> VerificationSummary {
    let native_grammars = dynamic_loader::available_dynamic_grammars();
    let results: Vec<_> = native_grammars
        .iter()
        .map(|name| verify_native_grammar(name))
        .collect();

    VerificationSummary::new(results)
}

// ============================================================================
// Display Formatting
// ============================================================================

/// Format verification results for console output.
pub fn format_verification_report(summary: &VerificationSummary, verbose: bool) -> String {
    use std::fmt::Write;
    let mut output = String::new();

    writeln!(output, "Grammar Verification:").unwrap();
    writeln!(output).unwrap();

    for result in &summary.results {
        let status_indicator = if result.status.is_ok() {
            "\u{2713}" // checkmark
        } else {
            "\u{2717}" // X mark
        };

        let source_str = format!("[{}]", result.source);
        let size_str = result.size_display();

        let detail_str = if verbose {
            match (&result.status, &result.details) {
                (VerificationStatus::Ok, Some(details)) => format!(" ({})", details),
                (VerificationStatus::Ok, None) if !size_str.is_empty() => format!(" ({})", size_str),
                (VerificationStatus::Missing, Some(details)) => format!(" - {}", details),
                (VerificationStatus::Invalid(msg), _) => format!(" - {}", msg),
                (VerificationStatus::LoadError(msg), _) => format!(" - {}", msg),
                _ => String::new(),
            }
        } else {
            match &result.status {
                VerificationStatus::Ok if !size_str.is_empty() => format!(" ({})", size_str),
                VerificationStatus::Invalid(msg) => format!(" - {}", msg),
                VerificationStatus::LoadError(msg) => format!(" - {}", msg),
                _ => String::new(),
            }
        };

        writeln!(
            output,
            "  {:12} {:10} {} {}{}",
            result.name,
            source_str,
            status_indicator,
            result.status,
            detail_str
        )
        .unwrap();
    }

    writeln!(output).unwrap();
    writeln!(
        output,
        "Summary: {} total ({} builtin, {} wasm, {} native)",
        summary.total(),
        summary.builtin_count,
        summary.wasm_count,
        summary.native_count
    )
    .unwrap();
    writeln!(
        output,
        "Status: {} OK, {} missing, {} errors",
        summary.ok_count, summary.missing_count, summary.error_count
    )
    .unwrap();

    output
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verification_status_display() {
        assert_eq!(format!("{}", VerificationStatus::Ok), "OK");
        assert_eq!(format!("{}", VerificationStatus::Missing), "MISSING");
        assert!(format!("{}", VerificationStatus::Invalid("test".to_string())).contains("INVALID"));
    }

    #[test]
    fn test_grammar_source_display() {
        assert_eq!(format!("{}", GrammarSource::Builtin), "builtin");
        assert_eq!(format!("{}", GrammarSource::Wasm), "wasm");
        assert_eq!(format!("{}", GrammarSource::Native), "native");
    }

    #[test]
    fn test_verify_builtin_grammar() {
        // Python should be a valid built-in grammar
        let result = verify_builtin_grammar("python");
        assert_eq!(result.source, GrammarSource::Builtin);
        assert!(result.status.is_ok());
    }

    #[test]
    fn test_verify_builtin_grammar_unknown() {
        let result = verify_builtin_grammar("not_a_real_language");
        assert!(!result.status.is_ok());
    }

    #[test]
    fn test_verify_wasm_grammar_unknown() {
        let result = verify_wasm_grammar("not_a_real_language");
        assert!(!result.status.is_ok());
    }

    #[test]
    fn test_size_display() {
        let mut result = GrammarVerification::new("test", GrammarSource::Builtin, VerificationStatus::Ok);

        result.size_bytes = Some(100);
        assert_eq!(result.size_display(), "100 B");

        result.size_bytes = Some(1500);
        assert_eq!(result.size_display(), "1.5 KB");

        result.size_bytes = Some(2_500_000);
        assert_eq!(result.size_display(), "2.5 MB");
    }

    #[test]
    fn test_verify_all_builtin() {
        let summary = verify_builtin_grammars();
        // We should have all 37 built-in grammars
        assert_eq!(summary.builtin_count, BUILTIN_LANGUAGES.len());
        // All should be OK since they're compiled in
        assert_eq!(summary.ok_count, BUILTIN_LANGUAGES.len());
    }

    #[test]
    fn test_wasm_magic_verification() {
        // Test with valid WASM bytes
        let valid_wasm = b"\x00asm\x01\x00\x00\x00"; // WASM magic + version
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_valid.wasm");
        fs::write(&test_file, valid_wasm).unwrap();

        assert!(verify_wasm_magic(&test_file).is_ok());
        fs::remove_file(&test_file).ok();
    }

    #[test]
    fn test_wasm_magic_invalid() {
        let invalid_data = b"not wasm";
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_invalid.wasm");
        fs::write(&test_file, invalid_data).unwrap();

        assert!(verify_wasm_magic(&test_file).is_err());
        fs::remove_file(&test_file).ok();
    }

    #[test]
    fn test_verification_summary() {
        let results = vec![
            GrammarVerification::new("python", GrammarSource::Builtin, VerificationStatus::Ok),
            GrammarVerification::new("sql", GrammarSource::Wasm, VerificationStatus::Missing),
            GrammarVerification::new("test", GrammarSource::Native, VerificationStatus::Invalid("bad".to_string())),
        ];

        let summary = VerificationSummary::new(results);
        assert_eq!(summary.total(), 3);
        assert_eq!(summary.ok_count, 1);
        assert_eq!(summary.missing_count, 1);
        assert_eq!(summary.error_count, 1);
        assert!(!summary.all_ok());
    }
}
