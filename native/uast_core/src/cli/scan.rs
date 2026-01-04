//! Scan command - scan files with YAML rules
//!
//! This command scans source files against YAML rule definitions and
//! outputs results in various formats including SARIF.

use clap::Args;
use console::style;
use ignore::WalkBuilder;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use super::output::{OutputFormat, ScanResultFormatter, print_summary};
use crate::dynamic_loader::load_grammar_by_name;
use crate::rules::{
    apply_fix, parse_rules_from_directory, parse_rules_from_file, Fix, RuleYaml, ScanResult,
    Scanner, Severity,
};
use crate::sarif::SarifWriter;
use crate::uast::{convert_tree_to_uast, get_builtin_language};

/// Arguments for the scan command
#[derive(Args, Debug)]
pub struct ScanArgs {
    /// Path to rules file or directory
    #[arg(short, long)]
    pub rules: Option<String>,

    /// Output format: text, json, or sarif
    #[arg(short, long, default_value = "text")]
    pub format: String,

    /// Apply fixes (modify files)
    #[arg(long)]
    pub fix: bool,

    /// Paths to scan (files or directories)
    #[arg(default_value = ".")]
    pub paths: Vec<String>,

    /// Filter by severity (error, warning, info, hint)
    #[arg(long)]
    pub severity: Option<String>,

    /// Filter by rule ID pattern
    #[arg(long)]
    pub rule_id: Option<String>,

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

    /// Fail if any issues are found (for CI)
    #[arg(long)]
    pub fail_on_issues: bool,
}

/// Execute the scan command
pub fn execute(args: ScanArgs, quiet: bool, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    // Load rules
    let rules = load_rules(&args, quiet, verbose)?;
    if rules.is_empty() {
        if !quiet {
            eprintln!("{} No rules loaded", style("[warn]").yellow());
        }
        return Ok(());
    }

    if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
        eprintln!(
            "{} Loaded {} rules",
            style("[info]").blue(),
            rules.len()
        );
    }

    // Create scanner and add rules
    let mut scanner = Scanner::new();
    for rule in &rules {
        if let Err(e) = scanner.add_rule(rule.clone()) {
            if !quiet {
                eprintln!(
                    "{} Error compiling rule '{}': {}",
                    style("[warn]").yellow(),
                    rule.id,
                    e
                );
            }
        }
    }

    // Get languages from rules to determine file extensions
    let languages: Vec<String> = rules.iter().map(|r| r.language.clone()).collect();
    let extensions = collect_extensions_for_languages(&languages);

    // Collect files to scan
    let files = collect_files(&args.paths, &extensions, &args)?;
    let file_count = files.len();

    if files.is_empty() {
        if !quiet {
            eprintln!("{} No matching files found", style("[warn]").yellow());
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
    let progress = if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
        let pb = ProgressBar::new(file_count as u64);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} files")
                .unwrap()
                .progress_chars("=>-"),
        );
        Some(pb)
    } else {
        None
    };

    // Process files in parallel
    let results: Vec<ScanResult> = files
        .par_iter()
        .flat_map(|(path, language)| {
            let result = scan_file(path, language, &scanner);

            if let Some(ref pb) = progress {
                pb.inc(1);
            }

            match result {
                Ok(file_results) => file_results,
                Err(e) => {
                    if verbose && !quiet {
                        eprintln!(
                            "{} Error scanning '{}': {}",
                            style("[warn]").yellow(),
                            path.display(),
                            e
                        );
                    }
                    Vec::new()
                }
            }
        })
        .collect();

    if let Some(ref pb) = progress {
        pb.finish_and_clear();
    }

    // Filter results by severity and rule ID
    let filtered_results = filter_results(results, &args);

    // Apply fixes if requested
    if args.fix {
        apply_fixes_to_files(&filtered_results, quiet, verbose)?;
    }

    let elapsed = start.elapsed();

    // Output results
    let format: OutputFormat = args.format.parse().unwrap_or(OutputFormat::Text);
    match format {
        OutputFormat::Text => {
            let formatter = ScanResultFormatter::new().with_fixes(!args.fix);
            print!("{}", formatter.format_text_all(&filtered_results));
            if !quiet {
                print_summary(&filtered_results, file_count, elapsed);
            }
        }
        OutputFormat::Json => {
            let formatter = ScanResultFormatter::new();
            println!("{}", formatter.format_json(&filtered_results)?);
        }
        OutputFormat::Sarif => {
            let formatter = ScanResultFormatter::new();
            println!("{}", formatter.format_sarif(&filtered_results)?);
        }
        OutputFormat::Tree => {
            // Tree format doesn't make sense for scan results
            return Err("Tree format is not supported for scan command. Use text, json, or sarif.".into());
        }
    }

    // Exit with error if --fail-on-issues and issues were found
    if args.fail_on_issues && !filtered_results.is_empty() {
        let error_count = filtered_results.iter().filter(|r| r.severity == Severity::Error).count();
        if error_count > 0 {
            return Err(format!("Found {} error(s)", error_count).into());
        }
    }

    Ok(())
}

/// Load rules from file or directory
fn load_rules(
    args: &ScanArgs,
    quiet: bool,
    verbose: bool,
) -> Result<Vec<RuleYaml>, Box<dyn std::error::Error>> {
    match &args.rules {
        Some(path) => {
            let rules_path = Path::new(path);
            if rules_path.is_dir() {
                if verbose && !quiet {
                    eprintln!(
                        "{} Loading rules from directory: {}",
                        style("[info]").blue(),
                        path
                    );
                }
                parse_rules_from_directory(rules_path)
                    .map_err(|e| format!("Error loading rules from '{}': {}", path, e).into())
            } else if rules_path.is_file() {
                if verbose && !quiet {
                    eprintln!(
                        "{} Loading rules from file: {}",
                        style("[info]").blue(),
                        path
                    );
                }
                parse_rules_from_file(rules_path)
                    .map_err(|e| format!("Error loading rules from '{}': {}", path, e).into())
            } else {
                Err(format!("Rules path '{}' does not exist", path).into())
            }
        }
        None => {
            // Try default locations
            let default_paths = ["rules", ".rules", "rules.yaml", ".uast-grep.yaml"];
            for default_path in default_paths {
                let path = Path::new(default_path);
                if path.exists() {
                    if verbose && !quiet {
                        eprintln!(
                            "{} Using default rules: {}",
                            style("[info]").blue(),
                            default_path
                        );
                    }
                    if path.is_dir() {
                        return parse_rules_from_directory(path)
                            .map_err(|e| format!("Error loading rules: {}", e).into());
                    } else {
                        return parse_rules_from_file(path)
                            .map_err(|e| format!("Error loading rules: {}", e).into());
                    }
                }
            }
            if !quiet {
                eprintln!(
                    "{} No rules file specified and no default rules found",
                    style("[warn]").yellow()
                );
                eprintln!("  Use --rules to specify a rules file or directory");
            }
            Ok(Vec::new())
        }
    }
}

/// Collect file extensions for the given languages
fn collect_extensions_for_languages(languages: &[String]) -> Vec<&'static str> {
    let mut extensions = Vec::new();
    for lang in languages {
        extensions.extend(get_extensions_for_language(lang));
    }
    extensions.sort();
    extensions.dedup();
    extensions
}

/// Get file extensions for a language
fn get_extensions_for_language(language: &str) -> Vec<&'static str> {
    match language.to_lowercase().as_str() {
        "*" | "any" | "all" => vec![], // Universal rules match all files
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
        "dart" => vec![".dart"],
        "bash" | "sh" | "shell" => vec![".sh", ".bash"],
        "lua" => vec![".lua"],
        "perl" | "pl" => vec![".pl", ".pm"],
        "r" => vec![".r", ".R"],
        "julia" | "jl" => vec![".jl"],
        "powershell" | "ps1" => vec![".ps1", ".psm1", ".psd1"],
        "haskell" | "hs" => vec![".hs"],
        "ocaml" | "ml" => vec![".ml", ".mli"],
        "elixir" | "ex" => vec![".ex", ".exs"],
        "erlang" | "erl" => vec![".erl"],
        "clojure" | "clj" => vec![".clj", ".cljs"],
        "fsharp" | "fs" => vec![".fs", ".fsx"],
        "elm" => vec![".elm"],
        "html" => vec![".html", ".htm"],
        "css" => vec![".css"],
        "json" => vec![".json"],
        "yaml" | "yml" => vec![".yaml", ".yml"],
        "xml" => vec![".xml"],
        "toml" => vec![".toml"],
        "markdown" | "md" => vec![".md"],
        "sql" => vec![".sql"],
        "graphql" | "gql" => vec![".graphql", ".gql"],
        "proto" => vec![".proto"],
        "zig" => vec![".zig"],
        "vue" => vec![".vue"],
        _ => vec![],
    }
}

/// Collect files to scan with their detected languages
fn collect_files(
    paths: &[String],
    extensions: &[&str],
    args: &ScanArgs,
) -> Result<Vec<(PathBuf, String)>, Box<dyn std::error::Error>> {
    let mut files: Vec<(PathBuf, String)> = Vec::new();

    for path_str in paths {
        let path = Path::new(path_str);

        if path.is_file() {
            if let Some(lang) = detect_language_from_path(path) {
                if extensions.is_empty() || has_matching_extension(path, extensions) {
                    files.push((path.to_path_buf(), lang));
                }
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
                        if let Some(lang) = detect_language_from_path(entry_path) {
                            if extensions.is_empty() || has_matching_extension(entry_path, extensions) {
                                files.push((entry_path.to_path_buf(), lang));
                            }
                        }
                    }
                }
            }
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
        false
    }
}

/// Detect language from file path
fn detect_language_from_path(path: &Path) -> Option<String> {
    let extension = path.extension()?.to_str()?;
    let lang = match extension.to_lowercase().as_str() {
        "rs" => "rust",
        "py" | "pyw" | "pyi" => "python",
        "js" | "mjs" | "cjs" => "javascript",
        "ts" => "typescript",
        "tsx" => "tsx",
        "java" => "java",
        "go" => "go",
        "c" | "h" => "c",
        "cpp" | "hpp" | "cc" | "cxx" => "cpp",
        "cs" => "c-sharp",
        "rb" => "ruby",
        "php" => "php",
        "swift" => "swift",
        "kt" | "kts" => "kotlin",
        "scala" | "sc" => "scala",
        "dart" => "dart",
        "sh" | "bash" => "bash",
        "lua" => "lua",
        "pl" | "pm" => "perl",
        "r" => "r",
        "jl" => "julia",
        "ps1" | "psm1" | "psd1" => "powershell",
        "hs" => "haskell",
        "ml" | "mli" => "ocaml",
        "ex" | "exs" => "elixir",
        "erl" => "erlang",
        "clj" | "cljs" => "clojure",
        "fs" | "fsx" => "fsharp",
        "elm" => "elm",
        "html" | "htm" => "html",
        "css" => "css",
        "json" => "json",
        "yaml" | "yml" => "yaml",
        "xml" => "xml",
        "toml" => "toml",
        "md" | "markdown" => "markdown",
        "sql" => "sql",
        "graphql" | "gql" => "graphql",
        "proto" => "proto",
        "zig" => "zig",
        "vue" => "vue",
        _ => return None,
    };
    Some(lang.to_string())
}

/// Scan a single file with the scanner
fn scan_file(
    path: &Path,
    language: &str,
    scanner: &Scanner,
) -> Result<Vec<ScanResult>, Box<dyn std::error::Error>> {
    let source = fs::read_to_string(path)?;

    // Get tree-sitter language
    let ts_language = get_builtin_language(language)
        .or_else(|| load_grammar_by_name(language).ok())
        .ok_or_else(|| format!("Unsupported language: {}", language))?;

    // Parse the file
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&ts_language)?;

    let tree = parser.parse(&source, None)
        .ok_or("Failed to parse file")?;

    // Convert to UAST
    let uast = convert_tree_to_uast(&tree, &source, language);

    // Scan with rules
    let mut results = scanner.scan_source(&uast, language, Some(path));

    // Set file path on all results
    let path_str = path.display().to_string();
    for result in &mut results {
        result.file_path = Some(path_str.clone());
    }

    Ok(results)
}

/// Filter results by severity and rule ID
fn filter_results(results: Vec<ScanResult>, args: &ScanArgs) -> Vec<ScanResult> {
    results
        .into_iter()
        .filter(|r| {
            // Filter by severity
            if let Some(ref severity_filter) = args.severity {
                let severity_str = match r.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                    Severity::Info => "info",
                    Severity::Hint => "hint",
                };
                if !severity_str.eq_ignore_ascii_case(severity_filter) {
                    return false;
                }
            }

            // Filter by rule ID pattern
            if let Some(ref rule_pattern) = args.rule_id {
                if !r.rule_id.contains(rule_pattern) {
                    return false;
                }
            }

            true
        })
        .collect()
}

/// Apply fixes to files
fn apply_fixes_to_files(
    results: &[ScanResult],
    quiet: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Group results by file
    let mut by_file: HashMap<String, Vec<&ScanResult>> = HashMap::new();
    for result in results {
        if result.fix.is_some() {
            if let Some(ref path) = result.file_path {
                by_file.entry(path.clone()).or_default().push(result);
            }
        }
    }

    let mut fixed_count = 0;
    let mut file_count = 0;

    for (path, file_results) in by_file {
        // Sort by location (reverse order so we can apply fixes from end to start)
        let mut sorted_results = file_results;
        sorted_results.sort_by(|a, b| {
            b.location.start_offset.cmp(&a.location.start_offset)
        });

        // Read the file
        let source = fs::read_to_string(&path)?;

        // Apply fixes from end to start
        let mut modified_source = source;
        let results_len = sorted_results.len();
        for result in sorted_results {
            if let Some(ref fix) = result.fix {
                if let Ok(new_source) = apply_fix(&modified_source, fix) {
                    modified_source = new_source;
                    fixed_count += 1;
                }
            }
        }

        // Write back
        fs::write(&path, modified_source)?;
        file_count += 1;

        if verbose && !quiet {
            eprintln!(
                "{} Fixed {} issue(s) in {}",
                style("[fix]").green(),
                results_len,
                path
            );
        }
    }

    if !quiet && fixed_count > 0 {
        eprintln!(
            "{} Applied {} fix(es) in {} file(s)",
            style("[fix]").green().bold(),
            fixed_count,
            file_count
        );
    }

    Ok(())
}
