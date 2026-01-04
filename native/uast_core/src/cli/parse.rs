//! Parse command - parse a file and print AST
//!
//! This command parses a source file and outputs the AST in various formats.

use clap::Args;
use console::style;
use std::fs;
use std::path::Path;

use super::output::{OutputFormat, TreeFormatter};
use crate::dynamic_loader::load_grammar_by_name;
use crate::uast::{
    convert_tree_to_uast, get_builtin_language, get_builtin_language_name_for_extension,
    parse_to_uast_json,
};

/// Arguments for the parse command
#[derive(Args, Debug)]
pub struct ParseArgs {
    /// File to parse
    pub file: String,

    /// Language to use (auto-detected from extension if not specified)
    #[arg(short, long)]
    pub language: Option<String>,

    /// Output format: text, json, or tree
    #[arg(short, long, default_value = "text")]
    pub format: String,

    /// Show source spans in tree output
    #[arg(long)]
    pub spans: bool,

    /// Show node text in tree output
    #[arg(long)]
    pub text: bool,
}

/// Execute the parse command
pub fn execute(args: ParseArgs, quiet: bool, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    let path = Path::new(&args.file);

    // Read the source file
    let source = fs::read_to_string(path)
        .map_err(|e| format!("Error reading file '{}': {}", args.file, e))?;

    // Determine the language
    let language = match args.language {
        Some(ref lang) => lang.clone(),
        None => detect_language_from_path(path)?,
    };

    if verbose && !quiet {
        eprintln!(
            "{} Parsing '{}' as {}",
            style("[info]").blue(),
            args.file,
            style(&language).cyan()
        );
    }

    // Parse the output format
    let format: OutputFormat = args.format.parse()
        .map_err(|e: String| e)?;

    match format {
        OutputFormat::Json => {
            // Use the UAST JSON mapper
            let json = parse_to_uast_json(&language, &source, Some(&args.file))?;
            println!("{}", json);
        }
        OutputFormat::Tree => {
            // Parse to UAST and format as tree
            let uast = parse_to_typed_uast(&language, &source, &args.file)?;
            let formatter = TreeFormatter::new()
                .with_spans(args.spans || verbose)
                .with_text(args.text || verbose);
            let output = formatter.format_tree(&uast);
            print!("{}", output);
        }
        OutputFormat::Text => {
            // Default text format - show tree structure with basic info
            let uast = parse_to_typed_uast(&language, &source, &args.file)?;
            let formatter = TreeFormatter::new()
                .with_spans(args.spans)
                .with_text(args.text);
            let output = formatter.format_tree(&uast);
            print!("{}", output);
        }
        OutputFormat::Sarif => {
            // SARIF format doesn't make sense for parse command
            return Err("SARIF format is not supported for parse command. Use json or tree.".into());
        }
    }

    Ok(())
}

/// Detect the language from the file path extension
fn detect_language_from_path(path: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let extension = path
        .extension()
        .and_then(|e| e.to_str())
        .ok_or_else(|| format!("Cannot detect language: file '{}' has no extension", path.display()))?;

    // Try to find language for this extension
    if let Some(lang) = get_builtin_language_name_for_extension(extension) {
        return Ok(lang.to_string());
    }

    // Map common extensions manually
    let language = match extension.to_lowercase().as_str() {
        "rs" => "rust",
        "py" | "pyw" | "pyi" => "python",
        "js" | "mjs" | "cjs" => "javascript",
        "ts" => "typescript",
        "tsx" => "tsx",
        "jsx" => "javascript",
        "java" => "java",
        "go" => "go",
        "c" | "h" => "c",
        "cpp" | "hpp" | "cc" | "cxx" | "hh" | "hxx" => "cpp",
        "cs" => "c-sharp",
        "rb" | "rake" | "gemspec" => "ruby",
        "php" => "php",
        "swift" => "swift",
        "kt" | "kts" => "kotlin",
        "scala" | "sc" => "scala",
        "sh" | "bash" | "zsh" => "bash",
        "lua" => "lua",
        "pl" | "pm" => "perl",
        "r" => "r",
        "jl" => "julia",
        "ps1" | "psm1" | "psd1" => "powershell",
        "hs" | "lhs" => "haskell",
        "ml" | "mli" => "ocaml",
        "ex" | "exs" => "elixir",
        "erl" | "hrl" => "erlang",
        "clj" | "cljs" | "cljc" | "edn" => "clojure",
        "fs" | "fsi" | "fsx" => "fsharp",
        "elm" => "elm",
        "html" | "htm" => "html",
        "css" => "css",
        "json" => "json",
        "yaml" | "yml" => "yaml",
        "xml" => "xml",
        "toml" => "toml",
        "md" | "markdown" => "markdown",
        "dockerfile" => "dockerfile",
        "mk" => "make",
        "cmake" => "cmake",
        "hcl" | "tf" | "tfvars" => "hcl",
        "nix" => "nix",
        "sql" => "sql",
        "graphql" | "gql" => "graphql",
        "proto" => "proto",
        "zig" => "zig",
        "dart" => "dart",
        "vue" => "vue",
        "v" | "vh" => "verilog",
        "sv" | "svh" => "systemverilog",
        "ada" | "adb" | "ads" => "ada",
        "f" | "f90" | "f95" | "f03" | "f08" | "for" => "fortran",
        "cob" | "cbl" | "cpy" => "cobol",
        "d" => "d",
        "m" | "mm" => "objc",
        "tex" | "ltx" => "latex",
        "agda" => "agda",
        "cairo" => "cairo",
        "cr" => "crystal",
        "cu" | "cuh" => "cuda",
        "cue" => "cue",
        "dhall" => "dhall",
        "groovy" | "gvy" | "gy" | "gsh" => "groovy",
        "lisp" | "lsp" | "cl" => "commonlisp",
        "awk" => "awk",
        "bicep" => "bicep",
        _ => return Err(format!(
            "Cannot detect language for extension '.{}'. Use --language to specify.",
            extension
        ).into()),
    };

    Ok(language.to_string())
}

/// Parse source code to a typed UAST node
fn parse_to_typed_uast(
    language: &str,
    source: &str,
    _file_path: &str,
) -> Result<crate::uast::schema::UastNode, Box<dyn std::error::Error>> {
    // Get the tree-sitter language
    let ts_language = get_builtin_language(language)
        .or_else(|| load_grammar_by_name(language).ok())
        .ok_or_else(|| format!("Unsupported language: {}", language))?;

    // Create parser and parse
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_language)
        .map_err(|e| format!("Error setting language: {}", e))?;

    let tree = parser.parse(source, None)
        .ok_or("Parse error: failed to parse source")?;

    // Convert to typed UAST
    let uast = convert_tree_to_uast(&tree, source, language);

    Ok(uast)
}
