//! Scan command - scan files with YAML rules
//!
//! This command scans source files against YAML rule definitions and
//! outputs results in various formats including SARIF.

use clap::Args;
use console::style;
use crossbeam_channel::Receiver;
use ignore::WalkBuilder;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::time::Instant;

use super::output::{OutputFormat, ScanResultFormatter};
use super::streaming::{create_channel, ScanMessage, StreamingStats};
use crate::dynamic_loader::load_grammar_by_name;
use crate::rules::{
    apply_fix, list_builtin_rulesets, load_builtin_rules, parse_rules_from_directory,
    parse_rules_from_file, BuiltinRuleset, RuleYaml, ScanResult, Scanner, Severity,
};
use crate::sarif::SarifWriter;
use crate::uast::get_builtin_language;

#[cfg(feature = "arena")]
use crate::uast::arena::UastArena;

#[cfg(feature = "arena")]
use crate::uast::convert::convert_tree_to_arena;

#[cfg(not(feature = "arena"))]
use crate::uast::convert_tree_to_uast;

/// Arguments for the scan command
#[derive(Args, Debug)]
pub struct ScanArgs {
    /// Built-in ruleset: security, performance, quality, or all
    ///
    /// Use built-in rules embedded in the binary:
    ///   -r security     ~1,430 security rules (179 CWEs)
    ///   -r performance  ~1,130 performance rules
    ///   -r quality      ~820 code quality rules
    ///   -r all          All ~3,380 rules combined
    ///
    /// Short forms: sec, perf, qual, *
    #[arg(short = 'r', long = "rules", value_name = "RULESET")]
    pub builtin_rules: Option<String>,

    /// External rules file or directory (can be combined with -r)
    ///
    /// Load additional rules from filesystem. When used with -r,
    /// external rules are merged with built-in rules.
    #[arg(short = 'e', long = "external", value_name = "PATH")]
    pub external_rules: Option<String>,

    /// Output format: text, json[=stream|array|pretty], or sarif
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

    /// Exclude files/directories matching glob pattern (can be repeated)
    ///
    /// Examples:
    ///   --exclude "**/test/**"      Exclude test directories
    ///   --exclude "*.generated.cs"  Exclude generated files
    ///   --exclude "**/node_modules" Exclude node_modules
    #[arg(short = 'x', long = "exclude", value_name = "GLOB")]
    pub exclude: Vec<String>,

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

    /// Buffer size for streaming results (controls memory usage)
    #[arg(long, default_value = "1000")]
    pub buffer_size: usize,

    /// SARIF batch size - results per SARIF file (default: 50000)
    #[arg(long, default_value = "50000")]
    pub sarif_batch_size: usize,
}

/// Execute the scan command with streaming output
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

    // Total count shown when using both -r and -e
    if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
        if args.builtin_rules.is_some() && args.external_rules.is_some() {
            eprintln!(
                "{} Total: {} rules (built-in + external)",
                style("[info]").blue(),
                rules.len()
            );
        }
    }

    // Create scanner and add rules (skip disabled rules)
    let mut scanner = Scanner::new();
    for rule in &rules {
        // Skip disabled rules
        if !rule.enabled {
            continue;
        }
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

    // Parse output format
    let format: OutputFormat = args.format.parse().unwrap_or(OutputFormat::Text);
    if matches!(format, OutputFormat::Tree) {
        return Err("Tree format is not supported for scan command. Use text, json, or sarif.".into());
    }

    // Progress bar (hide for JSON/SARIF machine-readable formats)
    let progress = if !quiet && !matches!(format, OutputFormat::JsonStream | OutputFormat::JsonArray | OutputFormat::JsonPretty | OutputFormat::Sarif) {
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

    // Create bounded channel for streaming results
    let (tx, rx) = create_channel(args.buffer_size);
    let stats = Arc::new(StreamingStats::new());

    // Spawn consumer thread based on format
    let consumer_stats = stats.clone();
    let show_fixes = !args.fix;
    let sarif_batch_size = args.sarif_batch_size;
    let rules_for_sarif = rules.clone();
    let apply_fixes = args.fix;

    let consumer_handle: JoinHandle<Result<Vec<ScanResult>, String>> = match format {
        OutputFormat::Text => spawn_text_consumer(rx, consumer_stats, show_fixes, quiet),
        OutputFormat::JsonStream => spawn_json_stream_consumer(rx, consumer_stats),
        OutputFormat::JsonArray => spawn_json_array_consumer(rx, consumer_stats, false),
        OutputFormat::JsonPretty => spawn_json_array_consumer(rx, consumer_stats, true),
        OutputFormat::Sarif => spawn_sarif_consumer(rx, consumer_stats, sarif_batch_size, rules_for_sarif),
        OutputFormat::Tree => unreachable!(), // Already checked above
    };

    // Wrap scanner in Arc for parallel access
    let scanner = Arc::new(scanner);

    // Process files in parallel - SEND results to channel instead of collecting
    files.par_iter().for_each(|(path, language)| {
        match scan_file(path, language, &scanner) {
            Ok(file_results) => {
                for result in file_results {
                    // Apply early filtering
                    if passes_filter(&result, &args) {
                        // Send to consumer (ignore send errors if channel closed)
                        let _ = tx.send(ScanMessage::Result(result));
                    }
                }
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
            }
        }

        // Update progress
        stats.increment_files();
        if let Some(ref pb) = progress {
            pb.inc(1);
        }
    });

    // Close the channel to signal completion
    drop(tx);

    // Wait for consumer to finish
    let fix_results = consumer_handle
        .join()
        .map_err(|_| "Consumer thread panicked")??;

    if let Some(ref pb) = progress {
        pb.finish_and_clear();
    }

    // Apply fixes if requested (collected during streaming)
    if apply_fixes && !fix_results.is_empty() {
        apply_fixes_to_files(&fix_results, quiet, verbose)?;
    }

    let elapsed = start.elapsed();

    // Print summary for text format
    if !quiet && matches!(format, OutputFormat::Text) {
        print_streaming_summary(&stats, file_count, elapsed);
    }

    // Exit with error if --fail-on-issues and errors were found
    if args.fail_on_issues && stats.get_error_count() > 0 {
        return Err(format!("Found {} error(s)", stats.get_error_count()).into());
    }

    Ok(())
}

/// Load rules from built-in rulesets and/or external files
///
/// Rule loading strategy:
/// 1. If -r is specified, load built-in ruleset (security, performance, quality, all)
/// 2. If -e is specified, load external rules from path
/// 3. If both -r and -e, merge them (external rules added after built-in)
/// 4. If neither specified, try default locations or show help
fn load_rules(
    args: &ScanArgs,
    quiet: bool,
    verbose: bool,
) -> Result<Vec<RuleYaml>, Box<dyn std::error::Error>> {
    let mut all_rules: Vec<RuleYaml> = Vec::new();

    // Load built-in rules if -r is specified
    if let Some(ruleset_name) = &args.builtin_rules {
        if let Some(ruleset) = BuiltinRuleset::from_str(ruleset_name) {
            if verbose && !quiet {
                eprintln!(
                    "{} Loading built-in {} rules",
                    style("[info]").blue(),
                    ruleset.name()
                );
            }

            let builtin = load_builtin_rules(ruleset)
                .map_err(|e| format!("Error loading built-in rules: {}", e))?;

            if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
                eprintln!(
                    "{} Loaded {} built-in {} rules",
                    style("[info]").blue(),
                    builtin.len(),
                    ruleset.name()
                );
            }

            all_rules.extend(builtin);
        } else {
            // Not a valid ruleset name - show available options
            return Err(format!(
                "Unknown ruleset '{}'. Available rulesets:\n{}",
                ruleset_name,
                list_builtin_rulesets()
                    .iter()
                    .map(|(name, desc)| format!("  {} - {}", name, desc))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
            .into());
        }
    }

    // Load external rules if -e is specified
    if let Some(path) = &args.external_rules {
        let rules_path = Path::new(path);
        if rules_path.is_dir() {
            if verbose && !quiet {
                eprintln!(
                    "{} Loading external rules from directory: {}",
                    style("[info]").blue(),
                    path
                );
            }
            let external = parse_rules_from_directory(rules_path)
                .map_err(|e| format!("Error loading external rules from '{}': {}", path, e))?;

            if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
                eprintln!(
                    "{} Loaded {} external rules from {}",
                    style("[info]").blue(),
                    external.len(),
                    path
                );
            }

            all_rules.extend(external);
        } else if rules_path.is_file() {
            if verbose && !quiet {
                eprintln!(
                    "{} Loading external rules from file: {}",
                    style("[info]").blue(),
                    path
                );
            }
            let external = parse_rules_from_file(rules_path)
                .map_err(|e| format!("Error loading external rules from '{}': {}", path, e))?;

            if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
                eprintln!(
                    "{} Loaded {} external rules from {}",
                    style("[info]").blue(),
                    external.len(),
                    path
                );
            }

            all_rules.extend(external);
        } else {
            return Err(format!("External rules path '{}' does not exist", path).into());
        }
    }

    // If no rules specified, default to all built-in rules
    if args.builtin_rules.is_none() && args.external_rules.is_none() {
        // First check for local override rules
        let local_paths = [".rules", "rules.yaml", ".uast-grep.yaml"];
        for local_path in local_paths {
            let path = Path::new(local_path);
            if path.exists() {
                if verbose && !quiet {
                    eprintln!(
                        "{} Found local rules: {}",
                        style("[info]").blue(),
                        local_path
                    );
                }
                // Load local rules AND all built-in rules
                if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
                    eprintln!(
                        "{} Loading all built-in rules (default)",
                        style("[info]").blue()
                    );
                }
                let builtin = load_builtin_rules(BuiltinRuleset::All)
                    .map_err(|e| format!("Error loading built-in rules: {}", e))?;
                all_rules.extend(builtin);

                // Add local rules
                if path.is_dir() {
                    let local = parse_rules_from_directory(path)
                        .map_err(|e| format!("Error loading local rules: {}", e))?;
                    if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
                        eprintln!(
                            "{} Loaded {} local rules from {}",
                            style("[info]").blue(),
                            local.len(),
                            local_path
                        );
                    }
                    all_rules.extend(local);
                } else {
                    let local = parse_rules_from_file(path)
                        .map_err(|e| format!("Error loading local rules: {}", e))?;
                    if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
                        eprintln!(
                            "{} Loaded {} local rules from {}",
                            style("[info]").blue(),
                            local.len(),
                            local_path
                        );
                    }
                    all_rules.extend(local);
                }
                return Ok(all_rules);
            }
        }

        // No local rules found - use all built-in rules by default
        if !quiet && !matches!(args.format.as_str(), "json" | "sarif") {
            eprintln!(
                "{} Loading all built-in rules",
                style("[info]").blue()
            );
        }
        let builtin = load_builtin_rules(BuiltinRuleset::All)
            .map_err(|e| format!("Error loading built-in rules: {}", e))?;
        all_rules.extend(builtin);
    }

    Ok(all_rules)
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
    use ignore::overrides::OverrideBuilder;

    let mut files: Vec<(PathBuf, String)> = Vec::new();

    // Build exclude overrides if any patterns provided
    let overrides = if !args.exclude.is_empty() {
        let mut builder = OverrideBuilder::new(".");
        for pattern in &args.exclude {
            // Negate the pattern to make it an exclusion
            let negated = format!("!{}", pattern);
            builder.add(&negated)?;
        }
        Some(builder.build()?)
    } else {
        None
    };

    // Helper to check if path should be excluded
    let is_excluded = |path: &Path| -> bool {
        if let Some(ref ov) = overrides {
            // Override returns Match::Ignore for negated patterns that match
            matches!(ov.matched(path, false), ignore::Match::Ignore(_))
        } else {
            false
        }
    };

    for path_str in paths {
        let path = Path::new(path_str);

        if path.is_file() {
            if is_excluded(path) {
                continue;
            }
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

            // Add exclude overrides to walk builder
            if let Some(ref ov) = overrides {
                builder.overrides(ov.clone());
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

    // Convert to UAST - use arena if feature enabled
    #[cfg(feature = "arena")]
    let uast = {
        let arena = UastArena::new();
        let arena_node = convert_tree_to_arena(&arena, &tree, &source, language);
        // Convert to owned for scanner compatibility
        arena_node.to_owned(&source)
        // Arena is dropped here, freeing all memory at once
    };

    #[cfg(not(feature = "arena"))]
    let uast = convert_tree_to_uast(&tree, &source, language);

    // Scan with rules
    let mut results = scanner.scan_source(&uast, language, Some(path), &source);

    // Set file path on all results
    let path_str = path.display().to_string();
    for result in &mut results {
        result.file_path = Some(path_str.clone());
    }

    Ok(results)
}

/// Check if a result passes the filter criteria (early filtering during scan)
fn passes_filter(result: &ScanResult, args: &ScanArgs) -> bool {
    // Filter by severity
    if let Some(ref severity_filter) = args.severity {
        let severity_str = match result.severity {
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
        if !result.rule_id.contains(rule_pattern) {
            return false;
        }
    }

    true
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

// ============================================================================
// Streaming Consumer Threads
// ============================================================================

/// Spawn text consumer - prints results immediately as they arrive
fn spawn_text_consumer(
    rx: Receiver<ScanMessage>,
    stats: Arc<StreamingStats>,
    show_fixes: bool,
    quiet: bool,
) -> JoinHandle<Result<Vec<ScanResult>, String>> {
    thread::spawn(move || {
        let formatter = ScanResultFormatter::new().with_fixes(show_fixes);
        let stdout = std::io::stdout();
        let mut handle = stdout.lock();
        let mut fix_results = Vec::new();

        for msg in rx {
            let ScanMessage::Result(result) = msg;
            stats.increment_severity(result.severity);

            // Print immediately
            if !quiet {
                let formatted = formatter.format_text(&result);
                let _ = handle.write_all(formatted.as_bytes());
            }

            // Collect results with fixes for later application
            if result.fix.is_some() {
                fix_results.push(result);
            }
        }

        let _ = handle.flush();
        Ok(fix_results)
    })
}

/// Spawn JSON stream consumer - outputs one JSON object per line (JSONL)
/// This is memory efficient and the default for --format json
fn spawn_json_stream_consumer(
    rx: Receiver<ScanMessage>,
    stats: Arc<StreamingStats>,
) -> JoinHandle<Result<Vec<ScanResult>, String>> {
    thread::spawn(move || {
        let stdout = std::io::stdout();
        let mut handle = stdout.lock();
        let mut fix_results = Vec::new();

        for msg in rx {
            let ScanMessage::Result(result) = msg;
            stats.increment_severity(result.severity);

            // Print one JSON object per line (JSONL format)
            if let Ok(json) = serde_json::to_string(&result) {
                let _ = writeln!(handle, "{}", json);
            }

            // Collect results with fixes
            if result.fix.is_some() {
                fix_results.push(result);
            }
        }

        let _ = handle.flush();
        Ok(fix_results)
    })
}

/// Spawn JSON array consumer - collects all results, outputs valid JSON array
/// WARNING: Can OOM on large scans. Use json=stream for large codebases.
fn spawn_json_array_consumer(
    rx: Receiver<ScanMessage>,
    stats: Arc<StreamingStats>,
    pretty: bool,
) -> JoinHandle<Result<Vec<ScanResult>, String>> {
    thread::spawn(move || {
        let mut all_results = Vec::new();
        let mut fix_results = Vec::new();

        for msg in rx {
            let ScanMessage::Result(result) = msg;
            stats.increment_severity(result.severity);

            // Collect results with fixes
            if result.fix.is_some() {
                fix_results.push(result.clone());
            }

            all_results.push(result);
        }

        // Output complete JSON array
        let json = if pretty {
            serde_json::to_string_pretty(&all_results)
        } else {
            serde_json::to_string(&all_results)
        };

        if let Ok(json) = json {
            println!("{}", json);
        }

        Ok(fix_results)
    })
}

/// Spawn SARIF consumer - buffers up to batch_size, then outputs
fn spawn_sarif_consumer(
    rx: Receiver<ScanMessage>,
    stats: Arc<StreamingStats>,
    batch_size: usize,
    rules: Vec<RuleYaml>,
) -> JoinHandle<Result<Vec<ScanResult>, String>> {
    thread::spawn(move || {
        let mut batch = Vec::with_capacity(batch_size.min(10000)); // Cap initial allocation
        let mut batch_num = 0;
        let mut fix_results = Vec::new();

        for msg in rx {
            let ScanMessage::Result(result) = msg;
            stats.increment_severity(result.severity);

            // Collect results with fixes
            if result.fix.is_some() {
                fix_results.push(result.clone());
            }

            batch.push(result);

            // Output batch when full
            if batch.len() >= batch_size {
                if let Err(e) = output_sarif_batch(&batch, &rules, batch_num) {
                    eprintln!("{} Error writing SARIF batch: {}", style("[warn]").yellow(), e);
                }
                batch.clear();
                batch_num += 1;
            }
        }

        // Output remaining results
        if !batch.is_empty() {
            if let Err(e) = output_sarif_batch(&batch, &rules, batch_num) {
                eprintln!("{} Error writing SARIF: {}", style("[warn]").yellow(), e);
            }
        }

        Ok(fix_results)
    })
}

/// Output a SARIF batch to stdout or file
fn output_sarif_batch(
    results: &[ScanResult],
    rules: &[RuleYaml],
    batch_num: usize,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let writer = SarifWriter::new("UAST-Grep", env!("CARGO_PKG_VERSION"));
    let log = writer.from_scan_results(results, rules);
    let json = writer.to_json_pretty(&log)?;

    if batch_num == 0 {
        // First batch goes to stdout
        println!("{}", json);
    } else {
        // Additional batches go to files
        let filename = format!("sarif-{:03}.json", batch_num);
        fs::write(&filename, &json)?;
        eprintln!("{} Wrote {} ({} results)", style("[info]").blue(), filename, results.len());
    }

    Ok(())
}

/// Print summary using streaming statistics
fn print_streaming_summary(stats: &StreamingStats, files_scanned: usize, elapsed: std::time::Duration) {
    use std::sync::atomic::Ordering;

    let total = stats.get_total_results();
    let errors = stats.get_error_count();
    let warnings = stats.warning_count.load(Ordering::Relaxed);
    let infos = stats.info_count.load(Ordering::Relaxed);
    let hints = stats.hint_count.load(Ordering::Relaxed);

    eprintln!();
    eprintln!("{}", style("Summary").bold().underlined());
    eprintln!("Files scanned: {}", files_scanned);
    eprintln!("Time elapsed:  {:.2?}", elapsed);
    eprintln!();

    if total == 0 {
        eprintln!("{}", style("No issues found").green());
    } else {
        eprintln!("Issues found:  {}", total);
        if errors > 0 {
            eprintln!("  {} errors", style(errors).red().bold());
        }
        if warnings > 0 {
            eprintln!("  {} warnings", style(warnings).yellow().bold());
        }
        if infos > 0 {
            eprintln!("  {} info", style(infos).blue().bold());
        }
        if hints > 0 {
            eprintln!("  {} hints", style(hints).cyan());
        }
    }
}
