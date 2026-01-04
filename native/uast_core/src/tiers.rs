//! Language tier definitions for UAST-Grep.
//!
//! Tier 1+2 (Built-in): 37 commonly-used languages compiled into the binary
//! Tier 3 (WASM): 34 niche/large languages loaded on-demand from .wasm files

/// Languages compiled directly into the binary (fast, no loading overhead)
pub const BUILTIN_LANGUAGES: &[&str] = &[
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

/// Languages loaded from WASM files (niche/large, loaded on-demand)
pub const WASM_LANGUAGES: &[&str] = &[
    "verilog", "latex", "sql", "fortran", "fsharp", "kotlin", "cobol",
    "scala", "objc", "julia", "d", "crystal", "cuda", "haskell", "swift",
    "perl", "arduino", "agda", "ocaml", "apex", "dart", "groovy",
    "commonlisp", "zig", "awk", "vim", "r", "bitbake", "ada", "cairo",
    "dhall", "cue", "doxygen", "comment",
];

/// Check if a language is built-in (Tier 1+2)
pub fn is_builtin(name: &str) -> bool {
    let canonical = normalize_name(name);
    BUILTIN_LANGUAGES.iter().any(|&l| l == canonical)
}

/// Check if a language is available as WASM (Tier 3)
pub fn is_wasm(name: &str) -> bool {
    let canonical = normalize_name(name);
    WASM_LANGUAGES.iter().any(|&l| l == canonical)
}

/// Public wrapper for normalize_name (for use by wasm_loader).
pub fn normalize_name_pub(name: &str) -> &'static str {
    normalize_name(name)
}

/// Normalize language name to canonical form (handle aliases).
/// Returns a static string for the canonical name.
fn normalize_name(name: &str) -> &'static str {
    // First check aliases that map to different canonical names
    match name.to_lowercase().as_str() {
        // C# variants
        "csharp" | "cs" | "c-sharp" | "c_sharp" => "c_sharp",
        // C++ variants
        "c++" | "cxx" | "cpp" => "cpp",
        // Shell variants
        "sh" | "shell" | "bash" => "bash",
        // Go variants
        "golang" | "go" => "go",
        // JavaScript variants
        "js" | "javascript" => "javascript",
        // TypeScript variants
        "ts" | "typescript" => "typescript",
        // Python variants
        "py" | "python" => "python",
        // Ruby variants
        "rb" | "ruby" => "ruby",
        // Rust variants
        "rs" | "rust" => "rust",
        // YAML variants
        "yml" | "yaml" => "yaml",
        // Markdown variants
        "md" | "markdown" => "markdown",
        // HCL/Terraform variants
        "terraform" | "tf" | "hcl" => "hcl",
        // Dockerfile variants
        "docker" | "dockerfile" => "dockerfile",
        // Make variants
        "makefile" | "make" => "make",
        // Proto variants
        "protobuf" | "proto" => "proto",
        // GraphQL variants
        "gql" | "graphql" => "graphql",
        // Elixir variants
        "ex" | "exs" | "elixir" => "elixir",
        // Erlang variants
        "erl" | "erlang" => "erlang",
        // Clojure variants
        "clj" | "clojure" => "clojure",
        // LaTeX variants
        "tex" | "latex" => "latex",
        // Fortran variants
        "f90" | "f95" | "fortran" => "fortran",
        // F# variants
        "fs" | "f#" | "fsharp" => "fsharp",
        // Kotlin variants
        "kt" | "kotlin" => "kotlin",
        // COBOL variants
        "cob" | "cobol" => "cobol",
        // OCaml variants
        "ml" | "ocaml" => "ocaml",
        // Julia variants
        "jl" | "julia" => "julia",
        // D variants
        "dlang" | "d" => "d",
        // Crystal variants
        "cr" | "crystal" => "crystal",
        // CUDA variants
        "cu" | "cuda" => "cuda",
        // Haskell variants
        "hs" | "haskell" => "haskell",
        // Perl variants
        "pl" | "perl" => "perl",
        // Arduino variants
        "ino" | "arduino" => "arduino",
        // Apex variants
        "sfapex" | "salesforce" | "apex" => "apex",
        // Objective-C variants
        "objective-c" | "objectivec" | "objc" => "objc",
        // Vim variants
        "viml" | "vimscript" | "vim" => "vim",
        // Common Lisp variants
        "lisp" | "cl" | "commonlisp" => "commonlisp",
        // Lua
        "lua" => "lua",
        // Elm
        "elm" => "elm",
        // Direct matches for all other languages
        "powershell" => "powershell",
        "json" => "json",
        "xml" => "xml",
        "toml" => "toml",
        "csv" => "csv",
        "bicep" => "bicep",
        "nix" => "nix",
        "html" => "html",
        "css" => "css",
        "tsx" => "tsx",
        "vue" => "vue",
        "angular" => "angular",
        "java" => "java",
        "c" => "c",
        "php" => "php",
        "cmake" => "cmake",
        "verilog" => "verilog",
        "sql" => "sql",
        "scala" => "scala",
        "swift" => "swift",
        "agda" => "agda",
        "dart" => "dart",
        "groovy" => "groovy",
        "zig" => "zig",
        "awk" => "awk",
        "r" => "r",
        "bitbake" => "bitbake",
        "ada" => "ada",
        "cairo" => "cairo",
        "dhall" => "dhall",
        "cue" => "cue",
        "doxygen" => "doxygen",
        "comment" => "comment",
        // Unknown - return empty (won't match anything)
        _ => "",
    }
}
