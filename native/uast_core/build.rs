//! Build script for uast_core - compiles Tier 1+2 grammar sources.
//!
//! Only the 37 "built-in" languages are compiled into the binary.
//! Tier 3 languages (34 niche/large grammars) are loaded as WASM at runtime.
//!
//! # Prerequisites
//!
//! Grammar sources must be downloaded first using the Build-NativeGrammars.ps1 script.
//! Each grammar should have at minimum a parser.c file in grammars/{name}/src/.

use std::fs;
use std::path::PathBuf;

/// Tier 1+2: Languages compiled into the binary
const BUILTIN_LANGUAGES: &[&str] = &[
    // Core
    "powershell", "c_sharp",
    // Data formats
    "json", "yaml", "xml", "toml", "csv",
    // DevOps
    "dockerfile", "hcl", "bicep", "nix", "bash",
    // Web
    "html", "css", "javascript", "typescript", "tsx", "vue", "angular",
    // Documentation
    "markdown",
    // Backend
    "python", "go", "java", "c", "cpp", "rust",
    // Scripting
    "ruby", "php", "lua",
    // Build/Config
    "cmake", "make", "proto", "graphql",
    // Functional
    "elixir", "erlang", "clojure", "elm",
];

fn main() {
    let grammars_dir = PathBuf::from("grammars");
    let include_dir = PathBuf::from("include");

    if !grammars_dir.exists() {
        println!("cargo:warning=No grammars directory found. Run Build-NativeGrammars.ps1 first.");
        return;
    }

    let mut compiled = 0;
    let mut skipped = 0;
    let mut failed = Vec::new();

    // Process each grammar directory
    let entries: Vec<_> = match fs::read_dir(&grammars_dir) {
        Ok(entries) => entries.filter_map(|e| e.ok()).collect(),
        Err(e) => {
            println!("cargo:warning=Failed to read grammars directory: {}", e);
            return;
        }
    };

    for entry in entries {
        let name = entry.file_name().to_string_lossy().to_string();

        if entry.file_type().map(|t| t.is_dir()).unwrap_or(false) {
            // Skip non-builtin grammars (Tier 3 WASM-only)
            if !BUILTIN_LANGUAGES.contains(&name.as_str()) {
                println!("cargo:warning=Skipping WASM-only grammar: {}", name);
                skipped += 1;
                continue;
            }

            match compile_grammar(&name, &entry.path(), &include_dir) {
                Ok(_) => {
                    compiled += 1;
                    println!("cargo:warning=Compiled grammar: {}", name);
                }
                Err(e) => {
                    failed.push(format!("{}: {}", name, e));
                    println!("cargo:warning=Failed to compile {}: {}", name, e);
                }
            }
        }
    }

    println!("cargo:warning=Successfully compiled {} grammars ({} skipped as WASM-only)", compiled, skipped);
    if !failed.is_empty() {
        println!("cargo:warning=Failed grammars ({}):", failed.len());
        for f in &failed {
            println!("cargo:warning=  - {}", f);
        }
    }
}

fn compile_grammar(name: &str, path: &PathBuf, include_dir: &PathBuf) -> Result<(), String> {
    let src_dir = path.join("src");
    let parser_c = src_dir.join("parser.c");

    if !parser_c.exists() {
        return Err("parser.c not found".to_string());
    }

    // Build the main parser.c file
    let mut build = cc::Build::new();
    build
        .include(&src_dir)
        .include(include_dir)  // tree_sitter/parser.h location
        .file(&parser_c)
        .opt_level(3)
        .warnings(false)
        // Consistent symbol naming
        .define("TREE_SITTER_HIDE_SYMBOLS", None);

    // Check for C scanner
    let scanner_c = src_dir.join("scanner.c");
    if scanner_c.exists() {
        build.file(&scanner_c);
    }

    // Compile the main grammar (C files)
    let lib_name = format!("tree_sitter_{}", name.replace("-", "_"));
    build.compile(&lib_name);

    // Check for C++ scanner (some grammars like Swift, Kotlin use C++)
    let scanner_cc = src_dir.join("scanner.cc");
    if scanner_cc.exists() {
        let mut cpp_build = cc::Build::new();
        cpp_build
            .cpp(true)
            .include(&src_dir)
            .include(include_dir)  // tree_sitter/parser.h location
            .file(&scanner_cc)
            .opt_level(3)
            .warnings(false);

        // Compile C++ scanner separately
        let scanner_lib_name = format!("{}_scanner", lib_name);
        cpp_build.compile(&scanner_lib_name);
    }

    // Tell Cargo to rerun if sources change
    println!("cargo:rerun-if-changed={}", src_dir.display());

    Ok(())
}
