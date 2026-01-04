//! Run command - search for pattern matches
//!
//! This command searches for pattern matches across files using UAST or native patterns.
//! Supports parallel scanning and .gitignore-aware file walking.

use clap::Args;
use console::style;
use ignore::WalkBuilder;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;

use super::output::MatchFormatter;
use crate::dynamic_loader::load_grammar_by_name;
use crate::uast::{
    get_builtin_language, get_native_types_for_uast, is_uast_pattern,
};

/// Arguments for the run command
#[derive(Args, Debug)]
pub struct RunArgs {
    /// Pattern to search for (UAST PascalCase or native snake_case)
    #[arg(short, long)]
    pub pattern: String,

    /// Language to use for parsing
    #[arg(short, long)]
    pub language: String,

    /// Paths to search (files or directories)
    #[arg(default_value = ".")]
    pub paths: Vec<String>,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,

    /// Show only count of matches per file
    #[arg(short, long)]
    pub count: bool,

    /// Force pattern to be interpreted as UAST type
    #[arg(short, long)]
    pub uast: bool,

    /// Number of parallel jobs (default: number of CPUs)
    #[arg(short = 'j', long)]
    pub jobs: Option<usize>,

    /// Don't respect .gitignore files
    #[arg(long)]
    pub no_ignore: bool,

    /// Include hidden files
    #[arg(long)]
    pub hidden: bool,

    /// Maximum depth to recurse into directories
    #[arg(long)]
    pub max_depth: Option<usize>,
}

/// Result from scanning a single file
#[derive(Debug)]
struct FileResult {
    path: PathBuf,
    matches: Vec<MatchInfo>,
}

#[derive(Debug, Clone, serde::Serialize)]
struct MatchInfo {
    line: u32,
    column: u32,
    text: String,
    node_kind: String,
}

#[derive(serde::Serialize)]
struct JsonOutput {
    files_scanned: u64,
    total_matches: u64,
    files_with_matches: u64,
    results: Vec<JsonFileResult>,
}

#[derive(serde::Serialize)]
struct JsonFileResult {
    path: String,
    matches: Vec<MatchInfo>,
}

/// Execute the run command
pub fn execute(args: RunArgs, quiet: bool, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    // Get the tree-sitter language
    let ts_language = get_builtin_language(&args.language)
        .or_else(|| load_grammar_by_name(&args.language).ok())
        .ok_or_else(|| {
            format!(
                "Unknown or unsupported language '{}'. Use 'languages' command to see available languages.",
                args.language
            )
        })?;

    // Determine if pattern is UAST type and resolve to native types
    let use_uast = args.uast || is_uast_pattern(&args.pattern);
    let native_patterns: Vec<String> = if use_uast {
        let native_types = get_native_types_for_uast(&args.pattern, &args.language);
        if native_types.is_empty() {
            return Err(format!(
                "No native types found for UAST type '{}' in language '{}'. Use 'parse' command to see available node types.",
                args.pattern, args.language
            ).into());
        }
        if verbose && !quiet {
            eprintln!(
                "{} UAST '{}' -> native types: {:?}",
                style("[info]").blue(),
                args.pattern,
                native_types
            );
        }
        native_types.iter().map(|s| s.to_string()).collect()
    } else {
        vec![args.pattern.clone()]
    };

    // Get file extensions for this language
    let extensions = get_extensions_for_language(&args.language);
    if extensions.is_empty() && verbose && !quiet {
        eprintln!(
            "{} No known extensions for '{}', will scan all files in paths",
            style("[warn]").yellow(),
            args.language
        );
    }

    // Collect files to scan
    let files = collect_files(&args.paths, &extensions, &args)?;
    let file_count = files.len();

    if files.is_empty() {
        if !quiet {
            eprintln!(
                "{} No {} files found in specified paths",
                style("[warn]").yellow(),
                args.language
            );
        }
        return Ok(());
    }

    if verbose && !quiet {
        eprintln!(
            "{} Found {} files to scan",
            style("[info]").blue(),
            file_count
        );
    }

    // Set up parallel processing
    if let Some(jobs) = args.jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build_global()
            .ok(); // Ignore error if pool already built
    }

    // Progress bar (if not quiet and not JSON output)
    let progress = if !quiet && !args.json {
        let pb = ProgressBar::new(file_count as u64);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} files ({per_sec})")
                .unwrap()
                .progress_chars("=>-"),
        );
        Some(pb)
    } else {
        None
    };

    // Counters for statistics
    let total_matches = Arc::new(AtomicU64::new(0));
    let files_with_matches = Arc::new(AtomicU64::new(0));

    // Process files in parallel
    let results: Vec<FileResult> = files
        .par_iter()
        .filter_map(|path| {
            let result = scan_file(path, &ts_language, &native_patterns);

            if let Some(ref pb) = progress {
                pb.inc(1);
            }

            match result {
                Ok(file_result) => {
                    if !file_result.matches.is_empty() {
                        let match_count = file_result.matches.len() as u64;
                        total_matches.fetch_add(match_count, Ordering::Relaxed);
                        files_with_matches.fetch_add(1, Ordering::Relaxed);
                    }
                    Some(file_result)
                }
                Err(e) => {
                    if verbose && !quiet {
                        eprintln!(
                            "{} Error scanning '{}': {}",
                            style("[warn]").yellow(),
                            path.display(),
                            e
                        );
                    }
                    None
                }
            }
        })
        .collect();

    if let Some(ref pb) = progress {
        pb.finish_and_clear();
    }

    let elapsed = start.elapsed();
    let total = total_matches.load(Ordering::Relaxed);
    let with_matches = files_with_matches.load(Ordering::Relaxed);

    // Output results
    if args.json {
        output_json(&results, file_count as u64, total, with_matches)?;
    } else if args.count {
        output_counts(&results);
    } else {
        output_text(&results);
    }

    // Print summary if not quiet and not JSON
    if !quiet && !args.json {
        eprintln!();
        eprintln!("{}", style("--- Statistics ---").dim());
        eprintln!("Files scanned:      {}", file_count);
        eprintln!("Files with matches: {}", with_matches);
        eprintln!("Total matches:      {}", total);
        eprintln!("Time elapsed:       {:.2?}", elapsed);
    }

    Ok(())
}

/// Collect files from paths using ignore-aware walker
fn collect_files(
    paths: &[String],
    extensions: &[&str],
    args: &RunArgs,
) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let mut files = Vec::new();

    for path_str in paths {
        let path = Path::new(path_str);

        if path.is_file() {
            // Check extension if we have filters
            if extensions.is_empty() || has_matching_extension(path, extensions) {
                files.push(path.to_path_buf());
            }
        } else if path.is_dir() {
            // Use ignore crate for directory walking
            let mut builder = WalkBuilder::new(path);
            builder
                .hidden(!args.hidden)
                .git_ignore(!args.no_ignore)
                .git_global(!args.no_ignore)
                .git_exclude(!args.no_ignore);

            if let Some(depth) = args.max_depth {
                builder.max_depth(Some(depth));
            }

            // Add file type filters
            if !extensions.is_empty() {
                for ext in extensions {
                    // Remove leading dot if present
                    let ext_clean = ext.trim_start_matches('.');
                    builder.add_custom_ignore_filename("");
                    // Note: ignore crate doesn't have built-in extension filtering,
                    // so we filter manually below
                }
            }

            for entry in builder.build() {
                if let Ok(entry) = entry {
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        if extensions.is_empty() || has_matching_extension(entry_path, extensions) {
                            files.push(entry_path.to_path_buf());
                        }
                    }
                }
            }
        } else {
            eprintln!(
                "{} Path '{}' does not exist",
                style("[warn]").yellow(),
                path_str
            );
        }
    }

    Ok(files)
}

/// Check if a path has a matching extension
fn has_matching_extension(path: &Path, extensions: &[&str]) -> bool {
    if let Some(ext) = path.extension() {
        let ext_str = format!(".{}", ext.to_string_lossy());
        extensions.iter().any(|e| e.eq_ignore_ascii_case(&ext_str))
    } else {
        // Check for extension-less files like Dockerfile, Makefile
        if let Some(name) = path.file_name() {
            let name_str = name.to_string_lossy();
            extensions.iter().any(|e| e.eq_ignore_ascii_case(&name_str))
        } else {
            false
        }
    }
}

/// Scan a single file for matches
fn scan_file(
    path: &Path,
    ts_language: &tree_sitter::Language,
    patterns: &[String],
) -> Result<FileResult, Box<dyn std::error::Error>> {
    let source = fs::read_to_string(path)?;

    let mut parser = tree_sitter::Parser::new();
    parser.set_language(ts_language)?;

    let tree = parser.parse(&source, None)
        .ok_or("Failed to parse file")?;

    let matches = search_tree(tree.root_node(), &source, patterns);

    Ok(FileResult {
        path: path.to_path_buf(),
        matches,
    })
}

/// Search the tree for nodes matching any of the given patterns
fn search_tree(
    node: tree_sitter::Node,
    source: &str,
    patterns: &[String],
) -> Vec<MatchInfo> {
    let mut matches = Vec::new();
    search_tree_recursive(node, source, patterns, &mut matches);
    matches
}

fn search_tree_recursive(
    node: tree_sitter::Node,
    source: &str,
    patterns: &[String],
    matches: &mut Vec<MatchInfo>,
) {
    if patterns.iter().any(|p| node.kind() == p) {
        let line = node.start_position().row + 1;
        let col = node.start_position().column + 1;

        // Get the text for this node
        let start = node.start_byte();
        let end = node.end_byte();
        let text = &source[start..end.min(source.len())];

        // Get first line as preview (truncate if too long)
        let preview = text.lines().next().unwrap_or("");
        let preview = if preview.len() > 100 {
            format!("{}...", &preview[..100])
        } else {
            preview.to_string()
        };

        matches.push(MatchInfo {
            line: line as u32,
            column: col as u32,
            text: preview,
            node_kind: node.kind().to_string(),
        });
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        search_tree_recursive(child, source, patterns, matches);
    }
}

/// Output results as JSON
fn output_json(
    results: &[FileResult],
    files_scanned: u64,
    total_matches: u64,
    files_with_matches: u64,
) -> Result<(), Box<dyn std::error::Error>> {
    let json_results: Vec<JsonFileResult> = results
        .iter()
        .filter(|r| !r.matches.is_empty())
        .map(|r| JsonFileResult {
            path: r.path.display().to_string(),
            matches: r.matches.clone(),
        })
        .collect();

    let output = JsonOutput {
        files_scanned,
        total_matches,
        files_with_matches,
        results: json_results,
    };

    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

/// Output match counts per file
fn output_counts(results: &[FileResult]) {
    for result in results {
        if !result.matches.is_empty() {
            println!("{}:{}", result.path.display(), result.matches.len());
        }
    }
}

/// Output results as text
fn output_text(results: &[FileResult]) {
    let formatter = MatchFormatter::new();
    for result in results {
        for m in &result.matches {
            print!(
                "{}",
                formatter.format_match(
                    &result.path.display().to_string(),
                    m.line,
                    m.column,
                    &m.text
                )
            );
        }
    }
}

/// Get file extensions for a language
fn get_extensions_for_language(language: &str) -> Vec<&'static str> {
    match language.to_lowercase().as_str() {
        "javascript" | "js" => vec![".js", ".mjs", ".cjs"],
        "typescript" | "ts" => vec![".ts"],
        "tsx" => vec![".tsx"],
        "python" | "py" => vec![".py", ".pyw", ".pyi"],
        "java" => vec![".java"],
        "go" | "golang" => vec![".go"],
        "rust" | "rs" => vec![".rs"],
        "c" => vec![".c", ".h"],
        "cpp" | "c++" | "cxx" => vec![".cpp", ".hpp", ".cc", ".cxx", ".hh", ".hxx"],
        "c-sharp" | "csharp" | "cs" => vec![".cs"],
        "ruby" | "rb" => vec![".rb", ".rake", ".gemspec"],
        "php" => vec![".php"],
        "swift" => vec![".swift"],
        "kotlin" | "kt" => vec![".kt", ".kts"],
        "scala" => vec![".scala", ".sc"],
        "dart" => vec![".dart"],
        "bash" | "sh" | "shell" => vec![".sh", ".bash", ".zsh"],
        "lua" => vec![".lua"],
        "perl" | "pl" => vec![".pl", ".pm"],
        "r" => vec![".r", ".R"],
        "julia" | "jl" => vec![".jl"],
        "powershell" | "ps1" => vec![".ps1", ".psm1", ".psd1"],
        "groovy" => vec![".groovy", ".gvy", ".gy", ".gsh"],
        "haskell" | "hs" => vec![".hs", ".lhs"],
        "ocaml" | "ml" => vec![".ml", ".mli"],
        "elixir" | "ex" => vec![".ex", ".exs"],
        "erlang" | "erl" => vec![".erl", ".hrl"],
        "clojure" | "clj" => vec![".clj", ".cljs", ".cljc", ".edn"],
        "fsharp" | "fs" => vec![".fs", ".fsi", ".fsx"],
        "elm" => vec![".elm"],
        "html" => vec![".html", ".htm"],
        "css" => vec![".css"],
        "json" => vec![".json"],
        "yaml" | "yml" => vec![".yaml", ".yml"],
        "xml" => vec![".xml"],
        "toml" => vec![".toml"],
        "markdown" | "md" => vec![".md", ".markdown"],
        "dockerfile" | "docker" => vec!["Dockerfile"],
        "make" | "makefile" => vec!["Makefile", ".mk"],
        "cmake" => vec![".cmake", "CMakeLists.txt"],
        "hcl" | "terraform" => vec![".hcl", ".tf", ".tfvars"],
        "nix" => vec![".nix"],
        "sql" => vec![".sql"],
        "graphql" | "gql" => vec![".graphql", ".gql"],
        "proto" | "protobuf" => vec![".proto"],
        "zig" => vec![".zig"],
        "vue" => vec![".vue"],
        _ => vec![],
    }
}
