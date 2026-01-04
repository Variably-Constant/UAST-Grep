//! Languages command - list supported languages
//!
//! This command displays all available languages for parsing,
//! including both built-in grammars and dynamically loaded ones.

use clap::Args;
use console::style;
use serde::Serialize;

use crate::dynamic_loader::available_dynamic_grammars;
use crate::uast::{available_builtin_extensions, available_builtin_languages};

/// Arguments for the languages command
#[derive(Args, Debug)]
pub struct LanguagesArgs {
    /// Output as JSON
    #[arg(long)]
    pub json: bool,

    /// Show only built-in languages
    #[arg(long)]
    pub builtin_only: bool,

    /// Show file extensions for each language
    #[arg(long, short = 'e')]
    pub extensions: bool,
}

/// Language information for JSON output
#[derive(Serialize)]
struct LanguageInfo {
    name: String,
    source: String, // "builtin" or "dynamic"
    #[serde(skip_serializing_if = "Vec::is_empty")]
    extensions: Vec<String>,
}

/// Language summary for JSON output
#[derive(Serialize)]
struct LanguagesSummary {
    total: usize,
    builtin_count: usize,
    dynamic_count: usize,
    languages: Vec<LanguageInfo>,
}

/// Execute the languages command
pub fn execute(args: LanguagesArgs, quiet: bool) -> Result<(), Box<dyn std::error::Error>> {
    let builtin = available_builtin_languages();
    let dynamic = if args.builtin_only {
        Vec::new()
    } else {
        available_dynamic_grammars()
    };
    let extensions = available_builtin_extensions();

    if args.json {
        output_json(&builtin, &dynamic)?;
    } else {
        output_text(&builtin, &dynamic, &extensions, args.extensions, quiet);
    }

    Ok(())
}

fn output_json(
    builtin: &[&'static str],
    dynamic: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    let mut languages: Vec<LanguageInfo> = Vec::new();

    // Collect built-in languages
    for lang in builtin {
        let lang_extensions = get_extensions_for_language(lang);
        languages.push(LanguageInfo {
            name: lang.to_string(),
            source: "builtin".to_string(),
            extensions: lang_extensions,
        });
    }

    // Collect dynamic languages
    for lang in dynamic {
        languages.push(LanguageInfo {
            name: lang.clone(),
            source: "dynamic".to_string(),
            extensions: Vec::new(), // Dynamic languages don't have pre-registered extensions
        });
    }

    // Sort by name
    languages.sort_by(|a, b| a.name.cmp(&b.name));

    let summary = LanguagesSummary {
        total: languages.len(),
        builtin_count: builtin.len(),
        dynamic_count: dynamic.len(),
        languages,
    };

    let json = serde_json::to_string_pretty(&summary)?;
    println!("{}", json);

    Ok(())
}

fn output_text(
    builtin: &[&'static str],
    dynamic: &[String],
    extensions: &[&'static str],
    show_extensions: bool,
    quiet: bool,
) {
    if !quiet {
        println!("{}", style("Built-in languages (compiled in):").bold());
    }

    // Sort builtin languages
    let mut sorted_builtin: Vec<_> = builtin.iter().collect();
    sorted_builtin.sort();

    for lang in sorted_builtin {
        if show_extensions {
            let exts = get_extensions_for_language(lang);
            if !exts.is_empty() {
                println!(
                    "  {} {}",
                    style(lang).cyan(),
                    style(format!("({})", exts.join(", "))).dim()
                );
            } else {
                println!("  {}", style(lang).cyan());
            }
        } else {
            println!("  {}", style(lang).cyan());
        }
    }

    if !quiet {
        println!();
    }

    // Show dynamic languages if available
    if !dynamic.is_empty() {
        if !quiet {
            println!("{}", style("Dynamic languages (from DLLs):").bold());
        }

        let mut sorted_dynamic: Vec<String> = dynamic.to_vec();
        sorted_dynamic.sort();

        for lang in sorted_dynamic {
            println!("  {}", style(&lang).green());
        }

        if !quiet {
            println!();
        }
    } else if !quiet {
        println!("{}", style("No dynamic grammar DLLs found.").dim());
        println!("{}", style("Set UAST_GRAMMAR_PATH or place DLLs in runtimes/win-x64/native/").dim());
        println!();
    }

    // Summary
    if !quiet {
        let total = builtin.len() + dynamic.len();
        println!(
            "{}: {} languages available ({} built-in, {} dynamic)",
            style("Total").bold(),
            style(total).green().bold(),
            builtin.len(),
            dynamic.len()
        );
    }

    // Show known extensions
    if show_extensions && !quiet {
        println!();
        println!("{}", style("Recognized file extensions:").bold());
        let mut sorted_exts: Vec<_> = extensions.iter().collect();
        sorted_exts.sort();
        println!("  {}", sorted_exts.iter().map(|e| format!(".{}", e)).collect::<Vec<_>>().join(", "));
    }
}

/// Get file extensions associated with a language
fn get_extensions_for_language(language: &str) -> Vec<String> {
    // Map common language names to their extensions
    match language.to_lowercase().as_str() {
        "rust" => vec![".rs".to_string()],
        "python" => vec![".py".to_string(), ".pyw".to_string()],
        "javascript" => vec![".js".to_string(), ".mjs".to_string(), ".cjs".to_string()],
        "typescript" => vec![".ts".to_string()],
        "tsx" => vec![".tsx".to_string()],
        "java" => vec![".java".to_string()],
        "go" => vec![".go".to_string()],
        "c" => vec![".c".to_string(), ".h".to_string()],
        "cpp" => vec![".cpp".to_string(), ".hpp".to_string(), ".cc".to_string(), ".hh".to_string()],
        "c-sharp" => vec![".cs".to_string()],
        "ruby" => vec![".rb".to_string()],
        "php" => vec![".php".to_string()],
        "swift" => vec![".swift".to_string()],
        "kotlin" => vec![".kt".to_string(), ".kts".to_string()],
        "scala" => vec![".scala".to_string()],
        "bash" => vec![".sh".to_string(), ".bash".to_string()],
        "lua" => vec![".lua".to_string()],
        "perl" => vec![".pl".to_string(), ".pm".to_string()],
        "r" => vec![".r".to_string(), ".R".to_string()],
        "julia" => vec![".jl".to_string()],
        "powershell" => vec![".ps1".to_string(), ".psm1".to_string()],
        "haskell" => vec![".hs".to_string()],
        "ocaml" => vec![".ml".to_string(), ".mli".to_string()],
        "elixir" => vec![".ex".to_string(), ".exs".to_string()],
        "erlang" => vec![".erl".to_string()],
        "clojure" => vec![".clj".to_string(), ".cljs".to_string()],
        "fsharp" => vec![".fs".to_string(), ".fsx".to_string()],
        "elm" => vec![".elm".to_string()],
        "html" => vec![".html".to_string(), ".htm".to_string()],
        "css" => vec![".css".to_string()],
        "json" => vec![".json".to_string()],
        "yaml" => vec![".yaml".to_string(), ".yml".to_string()],
        "xml" => vec![".xml".to_string()],
        "toml" => vec![".toml".to_string()],
        "markdown" => vec![".md".to_string()],
        "dockerfile" => vec!["Dockerfile".to_string()],
        "make" => vec!["Makefile".to_string(), ".mk".to_string()],
        "cmake" => vec![".cmake".to_string()],
        "hcl" => vec![".hcl".to_string(), ".tf".to_string()],
        "nix" => vec![".nix".to_string()],
        "sql" => vec![".sql".to_string()],
        "graphql" => vec![".graphql".to_string(), ".gql".to_string()],
        "proto" => vec![".proto".to_string()],
        "zig" => vec![".zig".to_string()],
        "dart" => vec![".dart".to_string()],
        "vue" => vec![".vue".to_string()],
        _ => Vec::new(),
    }
}
