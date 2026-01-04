//! Verify command - check grammar availability and validity
//!
//! This command verifies that grammar files (built-in, WASM, and native DLLs)
//! are valid and loadable.

use clap::Args;
use console::style;
use serde::Serialize;

use crate::verify::{
    verify_all_grammars, verify_builtin_grammars, verify_grammar,
    verify_native_grammars, verify_wasm_grammars, GrammarSource,
    GrammarVerification, VerificationStatus, VerificationSummary,
};

/// Arguments for the verify command
#[derive(Args, Debug)]
pub struct VerifyArgs {
    /// Verify only specific grammar(s) by name
    #[arg(value_name = "GRAMMAR")]
    pub grammars: Vec<String>,

    /// Verify only built-in grammars
    #[arg(long)]
    pub builtin_only: bool,

    /// Verify only WASM grammars
    #[arg(long)]
    pub wasm_only: bool,

    /// Verify only native DLL grammars
    #[arg(long)]
    pub native_only: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,

    /// Show detailed verification information
    #[arg(long, short = 'd')]
    pub detailed: bool,

    /// Only show grammars with errors
    #[arg(long)]
    pub errors_only: bool,
}

/// JSON output for a single verification result
#[derive(Serialize)]
struct VerificationResultJson {
    name: String,
    source: String,
    status: String,
    ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    size_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    details: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

impl From<&GrammarVerification> for VerificationResultJson {
    fn from(v: &GrammarVerification) -> Self {
        let error = match &v.status {
            VerificationStatus::Invalid(msg) => Some(msg.clone()),
            VerificationStatus::LoadError(msg) => Some(msg.clone()),
            _ => None,
        };

        Self {
            name: v.name.clone(),
            source: v.source.to_string(),
            status: match &v.status {
                VerificationStatus::Ok => "ok".to_string(),
                VerificationStatus::Missing => "missing".to_string(),
                VerificationStatus::Invalid(_) => "invalid".to_string(),
                VerificationStatus::LoadError(_) => "error".to_string(),
            },
            ok: v.status.is_ok(),
            path: v.path.as_ref().map(|p| p.display().to_string()),
            size_bytes: v.size_bytes,
            details: v.details.clone(),
            error,
        }
    }
}

/// JSON output for verification summary
#[derive(Serialize)]
struct VerificationSummaryJson {
    total: usize,
    ok: usize,
    missing: usize,
    errors: usize,
    builtin_count: usize,
    wasm_count: usize,
    native_count: usize,
    all_ok: bool,
    results: Vec<VerificationResultJson>,
}

impl From<&VerificationSummary> for VerificationSummaryJson {
    fn from(s: &VerificationSummary) -> Self {
        Self {
            total: s.total(),
            ok: s.ok_count,
            missing: s.missing_count,
            errors: s.error_count,
            builtin_count: s.builtin_count,
            wasm_count: s.wasm_count,
            native_count: s.native_count,
            all_ok: s.all_ok(),
            results: s.results.iter().map(VerificationResultJson::from).collect(),
        }
    }
}

/// Execute the verify command
pub fn execute(args: VerifyArgs, quiet: bool, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    // Get verification results
    let summary = if !args.grammars.is_empty() {
        // Verify specific grammars
        let results: Vec<_> = args.grammars.iter().map(|name| verify_grammar(name)).collect();
        VerificationSummary::new(results)
    } else if args.builtin_only {
        verify_builtin_grammars()
    } else if args.wasm_only {
        verify_wasm_grammars()
    } else if args.native_only {
        verify_native_grammars()
    } else {
        verify_all_grammars()
    };

    // Filter results if needed
    let summary = if args.errors_only {
        let filtered: Vec<_> = summary
            .results
            .iter()
            .filter(|r| !r.status.is_ok())
            .cloned()
            .collect();
        VerificationSummary::new(filtered)
    } else {
        summary
    };

    if args.json {
        output_json(&summary)?;
    } else {
        output_text(&summary, quiet, verbose || args.detailed);
    }

    // Return error exit code if any verifications failed
    if !summary.all_ok() && summary.error_count > 0 {
        std::process::exit(1);
    }

    Ok(())
}

fn output_json(summary: &VerificationSummary) -> Result<(), Box<dyn std::error::Error>> {
    let json_summary = VerificationSummaryJson::from(summary);
    let json = serde_json::to_string_pretty(&json_summary)?;
    println!("{}", json);
    Ok(())
}

fn output_text(summary: &VerificationSummary, quiet: bool, verbose: bool) {
    if !quiet {
        println!("{}", style("Grammar Verification:").bold());
        println!();
    }

    for result in &summary.results {
        let (status_symbol, status_style) = match &result.status {
            VerificationStatus::Ok => (
                "\u{2713}", // checkmark
                style("OK").green(),
            ),
            VerificationStatus::Missing => (
                "\u{2717}", // X mark
                style("MISSING").yellow(),
            ),
            VerificationStatus::Invalid(_) | VerificationStatus::LoadError(_) => (
                "\u{2717}", // X mark
                style("ERROR").red(),
            ),
        };

        let source_str = format!("[{}]", result.source);
        let source_styled = match result.source {
            GrammarSource::Builtin => style(source_str).cyan(),
            GrammarSource::Wasm => style(source_str).magenta(),
            GrammarSource::Native => style(source_str).green(),
            GrammarSource::Unknown => style(source_str).dim(),
        };

        // Build detail string
        let detail_str = if verbose {
            match (&result.status, &result.details, &result.path) {
                (VerificationStatus::Ok, Some(details), _) => {
                    let size = result.size_display();
                    if size.is_empty() {
                        format!(" ({})", details)
                    } else {
                        format!(" ({}, {})", size, details)
                    }
                }
                (VerificationStatus::Ok, None, Some(path)) => {
                    let size = result.size_display();
                    if size.is_empty() {
                        format!(" ({})", path.display())
                    } else {
                        format!(" ({}, {})", size, path.display())
                    }
                }
                (VerificationStatus::Missing, Some(details), _) => {
                    format!(" - {}", style(details).dim())
                }
                (VerificationStatus::Invalid(msg), _, _) => {
                    format!(" - {}", style(msg).red())
                }
                (VerificationStatus::LoadError(msg), _, _) => {
                    format!(" - {}", style(msg).red())
                }
                _ => {
                    let size = result.size_display();
                    if size.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", size)
                    }
                }
            }
        } else {
            match &result.status {
                VerificationStatus::Ok => {
                    let size = result.size_display();
                    if size.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", size)
                    }
                }
                VerificationStatus::Missing if result.details.is_some() => {
                    format!(" - {}", style(result.details.as_ref().unwrap()).dim())
                }
                VerificationStatus::Invalid(msg) => format!(" - {}", style(msg).red()),
                VerificationStatus::LoadError(msg) => format!(" - {}", style(msg).red()),
                _ => String::new(),
            }
        };

        println!(
            "  {:12} {:10} {} {}{}",
            style(&result.name).bold(),
            source_styled,
            status_symbol,
            status_style,
            detail_str
        );
    }

    if !quiet {
        println!();
        println!(
            "{}: {} grammars ({} builtin, {} wasm, {} native)",
            style("Total").bold(),
            style(summary.total()).green().bold(),
            summary.builtin_count,
            summary.wasm_count,
            summary.native_count
        );

        let status_line = format!(
            "{} OK, {} missing, {} errors",
            summary.ok_count, summary.missing_count, summary.error_count
        );

        if summary.all_ok() {
            println!("{}: {}", style("Status").bold(), style(status_line).green());
        } else if summary.error_count > 0 {
            println!("{}: {}", style("Status").bold(), style(status_line).red());
        } else {
            println!("{}: {}", style("Status").bold(), style(status_line).yellow());
        }
    }
}
