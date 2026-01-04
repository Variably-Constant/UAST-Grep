//! CLI module for UAST-Grep
//!
//! This module provides the command-line interface for the UAST-Grep tool.
//! It supports multiple commands:
//!
//! - `parse` - Parse a file and print AST
//! - `run` - Search for pattern matches
//! - `scan` - Scan with YAML rules
//! - `languages` - List supported languages
//! - `ts-query` - Execute native tree-sitter query
//! - `verify` - Verify grammar availability and validity

pub mod languages;
pub mod output;
pub mod parse;
pub mod run;
pub mod scan;
pub mod ts_query;
pub mod verify;

use clap::{Parser, Subcommand};

/// UAST-Grep - Cross-language AST search tool
///
/// A fast structural code search tool using tree-sitter and UAST.
/// Supports 74+ languages via unified AST patterns.
#[derive(Parser)]
#[command(name = "uast-grep")]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    /// Suppress progress output
    #[arg(short, long, global = true)]
    pub quiet: bool,

    /// Enable verbose output
    #[arg(short, long, global = true)]
    pub verbose: bool,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Parse a file and print AST
    Parse(parse::ParseArgs),

    /// Search for pattern matches
    Run(run::RunArgs),

    /// Scan files with YAML rules
    Scan(scan::ScanArgs),

    /// List supported languages
    Languages(languages::LanguagesArgs),

    /// Execute native tree-sitter query
    #[command(name = "ts-query")]
    TsQuery(ts_query::TsQueryArgs),

    /// Verify grammar availability and validity
    Verify(verify::VerifyArgs),
}

/// Execute the CLI with the given arguments
pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Parse(args) => parse::execute(args, cli.quiet, cli.verbose),
        Commands::Run(args) => run::execute(args, cli.quiet, cli.verbose),
        Commands::Scan(args) => scan::execute(args, cli.quiet, cli.verbose),
        Commands::Languages(args) => languages::execute(args, cli.quiet),
        Commands::TsQuery(args) => ts_query::execute(args, cli.quiet, cli.verbose),
        Commands::Verify(args) => verify::execute(args, cli.quiet, cli.verbose),
    }
}
