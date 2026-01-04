//! TsQuery command - execute native tree-sitter queries
//!
//! This command runs tree-sitter S-expression queries directly against source files,
//! providing maximum performance for users who know tree-sitter query syntax.

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
use crate::uast::get_builtin_language;

/// Arguments for the ts-query command
#[derive(Args, Debug)]
pub struct TsQueryArgs {
    /// Tree-sitter query pattern (S-expression syntax)
    pub query: String,

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

    /// Show captures only (not full match)
    #[arg(long)]
    pub captures_only: bool,

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

/// A single query match with captures
#[derive(Debug, Clone, serde::Serialize)]
struct QueryMatch {
    pattern_index: u32,
    line: u32,
    column: u32,
    text: String,
    captures: Vec<CaptureInfo>,
}

#[derive(Debug, Clone, serde::Serialize)]
struct CaptureInfo {
    name: String,
    line: u32,
    column: u32,
    text: String,
}

/// Results for a single file
#[derive(Debug)]
struct FileQueryResult {
    path: PathBuf,
    matches: Vec<QueryMatch>,
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
    matches: Vec<QueryMatch>,
}

/// Execute the ts-query command
pub fn execute(args: TsQueryArgs, quiet: bool, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
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

    // Compile the query to validate it
    let query = tree_sitter::Query::new(&ts_language, &args.query)
        .map_err(|e| format!("Invalid query: {}", e))?;

    if verbose && !quiet {
        eprintln!(
            "{} Query has {} patterns, {} captures",
            style("[info]").blue(),
            query.pattern_count(),
            query.capture_names().len()
        );
        if !query.capture_names().is_empty() {
            eprintln!(
                "{}   Captures: {}",
                style("[info]").blue(),
                query.capture_names().join(", ")
            );
        }
    }

    // Get file extensions for this language
    let extensions = get_extensions_for_language(&args.language);

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
            .ok();
    }

    // Progress bar
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

    // Counters
    let total_matches = Arc::new(AtomicU64::new(0));
    let files_with_matches = Arc::new(AtomicU64::new(0));

    // Clone query pattern for parallel use
    let query_pattern = args.query.clone();

    // Process files in parallel
    let results: Vec<FileQueryResult> = files
        .par_iter()
        .filter_map(|path| {
            let result = query_file(path, &ts_language, &query_pattern, args.captures_only);

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
                            "{} Error querying '{}': {}",
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
        output_text(&results, args.captures_only);
    }

    // Print summary
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

/// Collect files to query
fn collect_files(
    paths: &[String],
    extensions: &[&str],
    args: &TsQueryArgs,
) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let mut files = Vec::new();

    for path_str in paths {
        let path = Path::new(path_str);

        if path.is_file() {
            if extensions.is_empty() || has_matching_extension(path, extensions) {
                files.push(path.to_path_buf());
            }
        } else if path.is_dir() {
            let mut builder = WalkBuilder::new(path);
            builder
                .hidden(!args.hidden)
                .git_ignore(!args.no_ignore)
                .git_global(!args.no_ignore)
                .git_exclude(!args.no_ignore);

            if let Some(depth) = args.max_depth {
                builder.max_depth(Some(depth));
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
        }
    }

    Ok(files)
}

fn has_matching_extension(path: &Path, extensions: &[&str]) -> bool {
    if let Some(ext) = path.extension() {
        let ext_str = format!(".{}", ext.to_string_lossy());
        extensions.iter().any(|e| e.eq_ignore_ascii_case(&ext_str))
    } else {
        false
    }
}

/// Query a single file
fn query_file(
    path: &Path,
    ts_language: &tree_sitter::Language,
    query_pattern: &str,
    captures_only: bool,
) -> Result<FileQueryResult, Box<dyn std::error::Error>> {
    let source = fs::read_to_string(path)?;

    let mut parser = tree_sitter::Parser::new();
    parser.set_language(ts_language)?;

    let tree = parser.parse(&source, None)
        .ok_or("Failed to parse file")?;

    // Compile query (per-file to avoid sharing issues)
    let query = tree_sitter::Query::new(ts_language, query_pattern)?;

    let mut query_cursor = tree_sitter::QueryCursor::new();
    let mut matches_vec = Vec::new();

    use streaming_iterator::StreamingIterator;

    let mut matches = query_cursor.matches(&query, tree.root_node(), source.as_bytes());

    while let Some(m) = matches.next() {
        let captures: Vec<CaptureInfo> = m
            .captures
            .iter()
            .map(|c| {
                let node = c.node;
                let start = node.start_byte();
                let end = node.end_byte();
                let text = &source[start..end.min(source.len())];

                // Truncate long text
                let text = if text.len() > 100 {
                    format!("{}...", &text[..100])
                } else {
                    text.to_string()
                };

                CaptureInfo {
                    name: query.capture_names()[c.index as usize].to_string(),
                    line: node.start_position().row as u32 + 1,
                    column: node.start_position().column as u32 + 1,
                    text,
                }
            })
            .collect();

        // Get the first capture's location as the match location
        // (or use pattern root if no captures)
        let (line, column, text) = if !captures.is_empty() && captures_only {
            (
                captures[0].line,
                captures[0].column,
                captures[0].text.clone(),
            )
        } else if !m.captures.is_empty() {
            let first_node = m.captures[0].node;
            let start = first_node.start_byte();
            let end = first_node.end_byte();
            let text = &source[start..end.min(source.len())];
            let text = text.lines().next().unwrap_or("");
            let text = if text.len() > 100 {
                format!("{}...", &text[..100])
            } else {
                text.to_string()
            };
            (
                first_node.start_position().row as u32 + 1,
                first_node.start_position().column as u32 + 1,
                text,
            )
        } else {
            continue; // Skip matches with no captures
        };

        matches_vec.push(QueryMatch {
            pattern_index: m.pattern_index as u32,
            line,
            column,
            text,
            captures,
        });
    }

    Ok(FileQueryResult {
        path: path.to_path_buf(),
        matches: matches_vec,
    })
}

/// Output as JSON
fn output_json(
    results: &[FileQueryResult],
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

/// Output counts per file
fn output_counts(results: &[FileQueryResult]) {
    for result in results {
        if !result.matches.is_empty() {
            println!("{}:{}", result.path.display(), result.matches.len());
        }
    }
}

/// Output as text
fn output_text(results: &[FileQueryResult], captures_only: bool) {
    let formatter = MatchFormatter::new();

    for result in results {
        for m in &result.matches {
            if captures_only && !m.captures.is_empty() {
                // Print each capture on its own line
                for cap in &m.captures {
                    print!(
                        "{}",
                        formatter.format_match(
                            &result.path.display().to_string(),
                            cap.line,
                            cap.column,
                            &format!("@{}: {}", cap.name, cap.text)
                        )
                    );
                }
            } else {
                // Print match with inline captures
                let captures_str = if !m.captures.is_empty() {
                    let caps: Vec<String> = m
                        .captures
                        .iter()
                        .map(|c| format!("@{}=\"{}\"", c.name, truncate_text(&c.text, 30)))
                        .collect();
                    format!(" [{}]", caps.join(", "))
                } else {
                    String::new()
                };

                print!(
                    "{}",
                    formatter.format_match(
                        &result.path.display().to_string(),
                        m.line,
                        m.column,
                        &format!("{}{}", m.text, captures_str)
                    )
                );
            }
        }
    }
}

fn truncate_text(text: &str, max_len: usize) -> String {
    if text.len() > max_len {
        format!("{}...", &text[..max_len])
    } else {
        text.to_string()
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
        "cpp" | "c++" => vec![".cpp", ".hpp", ".cc", ".cxx"],
        "c-sharp" | "csharp" | "cs" => vec![".cs"],
        "ruby" | "rb" => vec![".rb"],
        "php" => vec![".php"],
        "swift" => vec![".swift"],
        "kotlin" | "kt" => vec![".kt", ".kts"],
        "scala" => vec![".scala"],
        "bash" | "sh" => vec![".sh", ".bash"],
        "lua" => vec![".lua"],
        "powershell" | "ps1" => vec![".ps1", ".psm1"],
        "haskell" | "hs" => vec![".hs"],
        "ocaml" | "ml" => vec![".ml", ".mli"],
        "json" => vec![".json"],
        "yaml" | "yml" => vec![".yaml", ".yml"],
        "toml" => vec![".toml"],
        "html" => vec![".html", ".htm"],
        "css" => vec![".css"],
        "sql" => vec![".sql"],
        "zig" => vec![".zig"],
        _ => vec![],
    }
}
