//! Language bindings for tree-sitter grammars.
//!
//! # Two-Tier Architecture
//!
//! - **Tier 1+2 (Built-in)**: 37 commonly-used languages compiled into the binary
//! - **Tier 3 (WASM)**: 34 niche/large languages loaded on-demand from .wasm files
//!
//! The `get_language` function tries built-in first, then falls back to WASM.
//! This provides fast access for common languages while supporting all 71.
//!
//! # Built-in Languages (Tier 1+2)
//!
//! Core: powershell, c_sharp
//! Data: json, yaml, xml, toml, csv
//! DevOps: dockerfile, hcl, bicep, nix, bash
//! Web: html, css, javascript, typescript, tsx, vue, angular
//! Docs: markdown
//! Backend: python, go, java, c, cpp, rust
//! Scripting: ruby, php, lua
//! Build: cmake, make, proto, graphql
//! Functional: elixir, erlang, clojure, elm
//!
//! # WASM Languages (Tier 3)
//!
//! verilog, latex, sql, fortran, fsharp, kotlin, cobol, scala, objc, julia,
//! d, crystal, cuda, haskell, swift, perl, arduino, agda, ocaml, apex, dart,
//! groovy, commonlisp, zig, awk, vim, r, bitbake, ada, cairo, dhall, cue,
//! doxygen, comment

use tree_sitter::Language;
use crate::tiers::{is_builtin, is_wasm};
use crate::wasm_loader;

// ============================================================================
// External Language Declarations (Tier 1+2: 37 built-in grammars)
// ============================================================================
// Only commonly-used languages are compiled into the binary.
// Tier 3 languages (34 niche/large grammars) are loaded via WASM at runtime.

// Core
extern "C" { fn tree_sitter_powershell() -> *const (); }
extern "C" { fn tree_sitter_c_sharp() -> *const (); }

// Data formats
extern "C" { fn tree_sitter_json() -> *const (); }
extern "C" { fn tree_sitter_yaml() -> *const (); }
extern "C" { fn tree_sitter_xml() -> *const (); }
extern "C" { fn tree_sitter_toml() -> *const (); }
extern "C" { fn tree_sitter_csv() -> *const (); }

// DevOps
extern "C" { fn tree_sitter_dockerfile() -> *const (); }
extern "C" { fn tree_sitter_hcl() -> *const (); }
extern "C" { fn tree_sitter_bicep() -> *const (); }
extern "C" { fn tree_sitter_nix() -> *const (); }
extern "C" { fn tree_sitter_bash() -> *const (); }

// Web
extern "C" { fn tree_sitter_html() -> *const (); }
extern "C" { fn tree_sitter_css() -> *const (); }
extern "C" { fn tree_sitter_javascript() -> *const (); }
extern "C" { fn tree_sitter_typescript() -> *const (); }
extern "C" { fn tree_sitter_tsx() -> *const (); }
extern "C" { fn tree_sitter_vue() -> *const (); }
extern "C" { fn tree_sitter_angular() -> *const (); }

// Documentation
extern "C" { fn tree_sitter_markdown() -> *const (); }

// Backend
extern "C" { fn tree_sitter_python() -> *const (); }
extern "C" { fn tree_sitter_go() -> *const (); }
extern "C" { fn tree_sitter_java() -> *const (); }
extern "C" { fn tree_sitter_c() -> *const (); }
extern "C" { fn tree_sitter_cpp() -> *const (); }
extern "C" { fn tree_sitter_rust() -> *const (); }

// Scripting
extern "C" { fn tree_sitter_ruby() -> *const (); }
extern "C" { fn tree_sitter_php() -> *const (); }
extern "C" { fn tree_sitter_lua() -> *const (); }

// Build/Config
extern "C" { fn tree_sitter_cmake() -> *const (); }
extern "C" { fn tree_sitter_make() -> *const (); }
extern "C" { fn tree_sitter_proto() -> *const (); }
extern "C" { fn tree_sitter_graphql() -> *const (); }

// Functional
extern "C" { fn tree_sitter_elixir() -> *const (); }
extern "C" { fn tree_sitter_erlang() -> *const (); }
extern "C" { fn tree_sitter_clojure() -> *const (); }
extern "C" { fn tree_sitter_elm() -> *const (); }

// ============================================================================
// Language Registry Functions
// ============================================================================

/// Get a built-in (Tier 1+2) language by name or alias.
///
/// Returns None if the language is not compiled in. For Tier 3 languages
/// (WASM-only), use `get_language` which falls back to WASM loading.
///
/// # Safety
///
/// The returned Language is safe to use as long as the binary is loaded.
pub fn get_builtin_language(name: &str) -> Option<Language> {
    let ptr = match name.to_lowercase().as_str() {
        // === Core ===
        "powershell" | "ps1" | "pwsh" => unsafe { tree_sitter_powershell() },
        "c-sharp" | "csharp" | "cs" | "c_sharp" => unsafe { tree_sitter_c_sharp() },

        // === Data formats ===
        "json" => unsafe { tree_sitter_json() },
        "yaml" | "yml" => unsafe { tree_sitter_yaml() },
        "xml" => unsafe { tree_sitter_xml() },
        "toml" => unsafe { tree_sitter_toml() },
        "csv" => unsafe { tree_sitter_csv() },

        // === DevOps ===
        "dockerfile" | "docker" => unsafe { tree_sitter_dockerfile() },
        "hcl" | "terraform" | "tf" => unsafe { tree_sitter_hcl() },
        "bicep" => unsafe { tree_sitter_bicep() },
        "nix" => unsafe { tree_sitter_nix() },
        "bash" | "sh" | "shell" => unsafe { tree_sitter_bash() },

        // === Web ===
        "html" => unsafe { tree_sitter_html() },
        "css" => unsafe { tree_sitter_css() },
        "javascript" | "js" => unsafe { tree_sitter_javascript() },
        "typescript" | "ts" => unsafe { tree_sitter_typescript() },
        "tsx" => unsafe { tree_sitter_tsx() },
        "vue" => unsafe { tree_sitter_vue() },
        "angular" => unsafe { tree_sitter_angular() },

        // === Documentation ===
        "markdown" | "md" => unsafe { tree_sitter_markdown() },

        // === Backend ===
        "python" | "py" => unsafe { tree_sitter_python() },
        "go" | "golang" => unsafe { tree_sitter_go() },
        "java" => unsafe { tree_sitter_java() },
        "c" => unsafe { tree_sitter_c() },
        "cpp" | "c++" | "cxx" => unsafe { tree_sitter_cpp() },
        "rust" | "rs" => unsafe { tree_sitter_rust() },

        // === Scripting ===
        "ruby" | "rb" => unsafe { tree_sitter_ruby() },
        "php" => unsafe { tree_sitter_php() },
        "lua" => unsafe { tree_sitter_lua() },

        // === Build/Config ===
        "cmake" => unsafe { tree_sitter_cmake() },
        "make" | "makefile" => unsafe { tree_sitter_make() },
        "proto" | "protobuf" => unsafe { tree_sitter_proto() },
        "graphql" | "gql" => unsafe { tree_sitter_graphql() },

        // === Functional ===
        "elixir" | "ex" | "exs" => unsafe { tree_sitter_elixir() },
        "erlang" | "erl" => unsafe { tree_sitter_erlang() },
        "clojure" | "clj" => unsafe { tree_sitter_clojure() },
        "elm" => unsafe { tree_sitter_elm() },

        _ => return None,
    };

    Some(unsafe { Language::from_raw(ptr as *const _) })
}

/// Get a language by name, trying built-in first then WASM.
///
/// This is the main entry point for language lookup. It:
/// 1. Checks built-in languages (Tier 1+2) - instant, no loading
/// 2. Falls back to WASM languages (Tier 3) - loads from disk on first use
///
/// # Example
///
/// ```ignore
/// // Built-in language - instant
/// let rust = get_language("rust").unwrap();
///
/// // WASM language - loads sql.wasm on first use
/// let sql = get_language("sql").unwrap();
/// ```
pub fn get_language(name: &str) -> Option<Language> {
    // Try built-in first (fast path)
    if let Some(lang) = get_builtin_language(name) {
        return Some(lang);
    }

    // Fall back to WASM (Tier 3 languages)
    wasm_loader::load_wasm_grammar(name)
}

/// List all built-in (Tier 1+2) language names.
pub fn available_builtin_languages() -> Vec<&'static str> {
    vec![
        // Core
        "powershell", "c-sharp",
        // Data
        "json", "yaml", "xml", "toml", "csv",
        // DevOps
        "dockerfile", "hcl", "bicep", "nix", "bash",
        // Web
        "html", "css", "javascript", "typescript", "tsx", "vue", "angular",
        // Docs
        "markdown",
        // Backend
        "python", "go", "java", "c", "cpp", "rust",
        // Scripting
        "ruby", "php", "lua",
        // Build
        "cmake", "make", "proto", "graphql",
        // Functional
        "elixir", "erlang", "clojure", "elm",
    ]
}

/// List all available languages (built-in + WASM).
pub fn available_languages() -> Vec<String> {
    let mut langs: Vec<String> = available_builtin_languages()
        .into_iter()
        .map(|s| s.to_string())
        .collect();

    // Add WASM languages from tiers module
    for &lang in crate::tiers::WASM_LANGUAGES {
        if !langs.contains(&lang.to_string()) {
            langs.push(lang.to_string());
        }
    }

    langs.sort();
    langs
}

/// Check if a language is available (either built-in or WASM).
pub fn is_language_available(name: &str) -> bool {
    is_builtin(name) || is_wasm(name) || wasm_loader::is_wasm_available(name)
}

/// Get file extensions for a language.
pub fn get_extensions(name: &str) -> Vec<&'static str> {
    match name.to_lowercase().as_str() {
        "agda" => vec![".agda", ".lagda"],
        "bash" | "sh" => vec![".sh", ".bash", ".bashrc", ".zsh"],
        "c" => vec![".c", ".h"],
        "cpp" | "c++" => vec![".cpp", ".cc", ".cxx", ".hpp", ".hh", ".hxx", ".h"],
        "c-sharp" | "csharp" => vec![".cs"],
        "css" => vec![".css"],
        "go" => vec![".go"],
        "haskell" => vec![".hs", ".lhs"],
        "html" => vec![".html", ".htm"],
        "java" => vec![".java"],
        "javascript" | "js" => vec![".js", ".mjs", ".cjs"],
        "json" => vec![".json"],
        "julia" => vec![".jl"],
        "ocaml" => vec![".ml", ".mli"],
        "php" => vec![".php"],
        "python" | "py" => vec![".py", ".pyw", ".pyi"],
        "ruby" => vec![".rb", ".rake", ".gemspec"],
        "rust" => vec![".rs"],
        "scala" => vec![".scala", ".sc"],
        "typescript" => vec![".ts"],
        "tsx" => vec![".tsx"],
        "verilog" => vec![".v", ".vh"],
        "arduino" => vec![".ino", ".pde"],
        "bicep" => vec![".bicep"],
        "bitbake" => vec![".bb", ".bbappend", ".bbclass"],
        "cairo" => vec![".cairo"],
        "csv" => vec![".csv"],
        "cuda" => vec![".cu", ".cuh"],
        "doxygen" => vec![],
        "hcl" | "terraform" => vec![".hcl", ".tf", ".tfvars"],
        "lua" => vec![".lua"],
        "make" => vec!["Makefile", ".mk"],
        "markdown" => vec![".md", ".markdown"],
        "objc" => vec![".m", ".mm"],
        "toml" => vec![".toml"],
        "vim" => vec![".vim", ".vimrc"],
        "vue" => vec![".vue"],
        "xml" => vec![".xml", ".xsd", ".xsl"],
        "yaml" => vec![".yaml", ".yml"],
        "zig" => vec![".zig"],
        "ada" => vec![".ada", ".adb", ".ads"],
        "angular" => vec![],
        "apex" => vec![".cls", ".trigger"],
        "awk" => vec![".awk"],
        "clojure" => vec![".clj", ".cljs", ".cljc", ".edn"],
        "cmake" => vec!["CMakeLists.txt", ".cmake"],
        "cobol" => vec![".cob", ".cbl", ".cpy"],
        "comment" => vec![],
        "commonlisp" => vec![".lisp", ".lsp", ".cl"],
        "crystal" => vec![".cr"],
        "cue" => vec![".cue"],
        "d" => vec![".d"],
        "dart" => vec![".dart"],
        "dhall" => vec![".dhall"],
        "dockerfile" => vec!["Dockerfile", ".dockerfile"],
        "elixir" => vec![".ex", ".exs"],
        "elm" => vec![".elm"],
        "erlang" => vec![".erl", ".hrl"],
        "fsharp" => vec![".fs", ".fsx", ".fsi"],
        "fortran" => vec![".f", ".for", ".f90", ".f95", ".f03", ".f08"],
        "graphql" => vec![".graphql", ".gql"],
        "groovy" => vec![".groovy", ".gradle"],
        "kotlin" => vec![".kt", ".kts"],
        "latex" => vec![".tex", ".sty", ".cls"],
        "nix" => vec![".nix"],
        "perl" => vec![".pl", ".pm", ".t"],
        "powershell" => vec![".ps1", ".psm1", ".psd1"],
        "proto" => vec![".proto"],
        "r" => vec![".r", ".R"],
        "sql" => vec![".sql"],
        "swift" => vec![".swift"],
        _ => vec![],
    }
}

/// Get the language name from a file extension.
pub fn language_for_extension(ext: &str) -> Option<&'static str> {
    let ext_lower = ext.to_lowercase();
    let ext_check = if ext_lower.starts_with('.') { ext_lower.as_str() } else { return None };

    match ext_check {
        ".agda" | ".lagda" => Some("agda"),
        ".sh" | ".bash" | ".zsh" => Some("bash"),
        ".c" => Some("c"),
        ".cpp" | ".cc" | ".cxx" | ".hpp" | ".hh" | ".hxx" => Some("cpp"),
        ".h" => Some("c"), // Defaults to C, could be C++
        ".cs" => Some("c-sharp"),
        ".css" => Some("css"),
        ".go" => Some("go"),
        ".hs" | ".lhs" => Some("haskell"),
        ".html" | ".htm" => Some("html"),
        ".java" => Some("java"),
        ".js" | ".mjs" | ".cjs" => Some("javascript"),
        ".json" => Some("json"),
        ".jl" => Some("julia"),
        ".ml" | ".mli" => Some("ocaml"),
        ".php" => Some("php"),
        ".py" | ".pyw" | ".pyi" => Some("python"),
        ".rb" | ".rake" | ".gemspec" => Some("ruby"),
        ".rs" => Some("rust"),
        ".scala" | ".sc" => Some("scala"),
        ".ts" => Some("typescript"),
        ".tsx" => Some("tsx"),
        ".v" | ".vh" => Some("verilog"),
        ".ino" | ".pde" => Some("arduino"),
        ".bicep" => Some("bicep"),
        ".bb" | ".bbappend" | ".bbclass" => Some("bitbake"),
        ".cairo" => Some("cairo"),
        ".csv" => Some("csv"),
        ".cu" | ".cuh" => Some("cuda"),
        ".hcl" | ".tf" | ".tfvars" => Some("hcl"),
        ".lua" => Some("lua"),
        ".mk" => Some("make"),
        ".md" | ".markdown" => Some("markdown"),
        ".m" | ".mm" => Some("objc"),
        ".toml" => Some("toml"),
        ".vim" | ".vimrc" => Some("vim"),
        ".vue" => Some("vue"),
        ".xml" | ".xsd" | ".xsl" => Some("xml"),
        ".yaml" | ".yml" => Some("yaml"),
        ".zig" => Some("zig"),
        ".ada" | ".adb" | ".ads" => Some("ada"),
        ".cls" | ".trigger" => Some("apex"),
        ".awk" => Some("awk"),
        ".clj" | ".cljs" | ".cljc" | ".edn" => Some("clojure"),
        ".cmake" => Some("cmake"),
        ".cob" | ".cbl" | ".cpy" => Some("cobol"),
        ".lisp" | ".lsp" | ".cl" => Some("commonlisp"),
        ".cr" => Some("crystal"),
        ".cue" => Some("cue"),
        ".d" => Some("d"),
        ".dart" => Some("dart"),
        ".dhall" => Some("dhall"),
        ".dockerfile" => Some("dockerfile"),
        ".ex" | ".exs" => Some("elixir"),
        ".elm" => Some("elm"),
        ".erl" | ".hrl" => Some("erlang"),
        ".fs" | ".fsx" | ".fsi" => Some("fsharp"),
        ".f" | ".for" | ".f90" | ".f95" | ".f03" | ".f08" => Some("fortran"),
        ".graphql" | ".gql" => Some("graphql"),
        ".groovy" | ".gradle" => Some("groovy"),
        ".kt" | ".kts" => Some("kotlin"),
        ".tex" | ".sty" => Some("latex"),
        ".nix" => Some("nix"),
        ".pl" | ".pm" | ".t" => Some("perl"),
        ".ps1" | ".psm1" | ".psd1" => Some("powershell"),
        ".proto" => Some("proto"),
        ".r" => Some("r"),
        ".sql" => Some("sql"),
        ".swift" => Some("swift"),
        _ => None,
    }
}

/// Get a built-in language by file extension.
/// Alias for language_for_extension + get_builtin_language.
pub fn get_builtin_language_for_extension(ext: &str) -> Option<Language> {
    language_for_extension(ext).and_then(get_builtin_language)
}

/// Get the language name for a file extension.
/// Alias for language_for_extension.
pub fn get_builtin_language_name_for_extension(ext: &str) -> Option<&'static str> {
    language_for_extension(ext)
}

/// Check if a language is available as a built-in.
pub fn is_builtin_language(name: &str) -> bool {
    get_builtin_language(name).is_some()
}

/// Get all available file extensions for built-in grammars.
pub fn available_builtin_extensions() -> Vec<&'static str> {
    vec![
        // Tier 1
        ".agda", ".lagda", ".sh", ".bash", ".zsh", ".c", ".h",
        ".cpp", ".cc", ".cxx", ".hpp", ".hh", ".hxx", ".cs",
        ".css", ".go", ".hs", ".lhs", ".html", ".htm", ".java",
        ".js", ".mjs", ".cjs", ".json", ".jl", ".ml", ".mli",
        ".php", ".py", ".pyw", ".pyi", ".rb", ".rake", ".gemspec",
        ".rs", ".scala", ".sc", ".ts", ".tsx", ".v", ".vh",
        // Tier 2
        ".ino", ".pde", ".bicep", ".bb", ".bbappend", ".bbclass",
        ".cairo", ".csv", ".cu", ".cuh", ".hcl", ".tf", ".tfvars",
        ".lua", ".mk", ".md", ".markdown", ".m", ".mm", ".toml",
        ".vim", ".vue", ".xml", ".xsd", ".xsl", ".yaml", ".yml",
        ".zig",
        // Tier 3
        ".ada", ".adb", ".ads", ".cls", ".trigger", ".awk",
        ".clj", ".cljs", ".cljc", ".edn", ".cmake", ".cob", ".cbl",
        ".cpy", ".lisp", ".lsp", ".cl", ".cr", ".cue", ".d",
        ".dart", ".dhall", ".dockerfile", ".ex", ".exs", ".elm",
        ".erl", ".hrl", ".fs", ".fsx", ".fsi", ".f", ".for",
        ".f90", ".f95", ".f03", ".f08", ".graphql", ".gql",
        ".groovy", ".gradle", ".kt", ".kts", ".tex", ".sty",
        ".nix", ".pl", ".pm", ".t", ".ps1", ".psm1", ".psd1",
        ".proto", ".r", ".sql", ".swift",
    ]
}
