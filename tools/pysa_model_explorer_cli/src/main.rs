/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use anyhow::Context;
use clap::Parser;
use clap::Subcommand;
use clap::ValueEnum;

#[derive(Debug, Clone, ValueEnum)]
enum OutputFormat {
    Json,
    Text,
}

#[derive(Debug, Clone, ValueEnum, PartialEq)]
enum ShowSection {
    Sources,
    Sinks,
    Tito,
}

#[derive(Debug, Parser)]
#[command(
    name = "pysa-model-explorer",
    about = "Explore Pysa taint analysis result directories"
)]
struct Cli {
    /// Path to the Pysa result directory
    result_dir: PathBuf,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Get the taint model for a callable
    GetModel {
        /// Fully qualified callable name
        callable: String,

        /// Output format
        #[arg(long, value_enum, default_value_t = OutputFormat::Json)]
        format: OutputFormat,

        /// Which sections to show (can be repeated; defaults to all)
        #[arg(long, value_enum)]
        show: Vec<ShowSection>,

        /// Filter by taint kind
        #[arg(long)]
        kind: Option<String>,

        /// Filter by caller port
        #[arg(long)]
        caller_port: Option<String>,

        /// Show taint features
        #[arg(long, default_value_t = false)]
        show_features: bool,

        /// Show TITO positions
        #[arg(long, default_value_t = false)]
        show_tito_positions: bool,

        /// Show class intervals
        #[arg(long, default_value_t = false)]
        show_class_intervals: bool,

        /// Show leaf names
        #[arg(long, default_value_t = false)]
        show_leaf_names: bool,
    },

    /// Get the call graph for a callable
    GetCallGraph {
        /// Fully qualified callable name
        callable: String,

        /// Output format
        #[arg(long, value_enum, default_value_t = OutputFormat::Json)]
        format: OutputFormat,
    },

    /// Get issues for a callable
    GetIssues {
        /// Fully qualified callable name
        callable: String,

        /// Output format
        #[arg(long, value_enum, default_value_t = OutputFormat::Json)]
        format: OutputFormat,

        /// Filter by issue code
        #[arg(long)]
        code: Option<i64>,

        /// Filter by master_handle (substring match)
        #[arg(long)]
        handle: Option<String>,

        /// Show taint features
        #[arg(long, default_value_t = false)]
        show_features: bool,

        /// Show TITO positions
        #[arg(long, default_value_t = false)]
        show_tito_positions: bool,

        /// Show class intervals
        #[arg(long, default_value_t = false)]
        show_class_intervals: bool,

        /// Show leaf names
        #[arg(long, default_value_t = false)]
        show_leaf_names: bool,
    },

    /// Count issues for a callable
    CountIssues {
        /// Fully qualified callable name
        callable: String,

        /// Filter by issue code
        #[arg(long)]
        code: Option<i64>,

        /// Filter by master_handle (substring match)
        #[arg(long)]
        handle: Option<String>,
    },

    /// Search for callables matching a regex pattern
    Search {
        /// Regex pattern to match against callable names
        pattern: String,
    },

    /// Build (or load cached) the index for a result directory
    BuildIndex,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Command::GetModel {
            callable,
            format,
            show,
            kind,
            caller_port,
            show_features,
            show_tito_positions,
            show_class_intervals,
            show_leaf_names,
        } => {
            let db = pysa_model_explorer::index::open_or_build_index(&cli.result_dir)?;
            let position = db
                .get_model_position(callable)?
                .context(format!("no model for callable `{}`", callable))?;
            let data = db.read_entry(&position)?;
            let mut model: pysa_model_explorer::types::Model = serde_json::from_value(data)?;

            let (show_sources, show_sinks, show_tito) = if show.is_empty() {
                (true, true, true)
            } else {
                (
                    show.contains(&ShowSection::Sources),
                    show.contains(&ShowSection::Sinks),
                    show.contains(&ShowSection::Tito),
                )
            };
            let options = pysa_model_explorer::model::ModelOptions {
                show_sources,
                show_sinks,
                show_tito,
                kind: kind.clone(),
                caller_port: caller_port.clone(),
                show_features: *show_features,
                show_tito_positions: *show_tito_positions,
                show_class_intervals: *show_class_intervals,
                show_leaf_names: *show_leaf_names,
            };
            pysa_model_explorer::model::filter_and_strip_model(&mut model, &options);

            match format {
                OutputFormat::Json => pysa_model_explorer::format::print_model_json(&model)?,
                OutputFormat::Text => pysa_model_explorer::format::print_model_text(&model)?,
            }
        }
        Command::GetCallGraph { callable, format } => {
            let db = pysa_model_explorer::index::open_or_build_index(&cli.result_dir)?;
            let position = db.get_call_graph_position(callable)?.context(format!(
                "no higher order call graph for callable `{}`",
                callable
            ))?;
            let data = db.read_entry(&position)?;
            let call_graph: pysa_model_explorer::types::CallGraph = serde_json::from_value(data)?;

            match format {
                OutputFormat::Json => {
                    pysa_model_explorer::format::print_call_graph_json(&call_graph)?
                }
                OutputFormat::Text => {
                    pysa_model_explorer::format::print_call_graph_text(&call_graph)?
                }
            }
        }
        Command::GetIssues {
            callable,
            format,
            code,
            handle,
            show_features,
            show_tito_positions,
            show_class_intervals,
            show_leaf_names,
        } => {
            let db = pysa_model_explorer::index::open_or_build_index(&cli.result_dir)?;
            let positions = db.get_issue_positions(callable)?;
            anyhow::ensure!(!positions.is_empty(), "no data for callable `{}`", callable);

            let mut issues = Vec::new();
            for position in &positions {
                let data = db.read_entry(position)?;
                let issue: pysa_model_explorer::types::Issue = serde_json::from_value(data)?;
                issues.push(issue);
            }

            let options = pysa_model_explorer::issue::IssueOptions {
                code: *code,
                handle: handle.clone(),
                show_features: *show_features,
                show_tito_positions: *show_tito_positions,
                show_class_intervals: *show_class_intervals,
                show_leaf_names: *show_leaf_names,
            };
            pysa_model_explorer::issue::filter_issues(&mut issues, &options);

            match format {
                OutputFormat::Json => pysa_model_explorer::format::print_issues_json(&issues)?,
                OutputFormat::Text => pysa_model_explorer::format::print_issues_text(&issues)?,
            }
        }
        Command::CountIssues {
            callable,
            code,
            handle,
        } => {
            let db = pysa_model_explorer::index::open_or_build_index(&cli.result_dir)?;
            let positions = db.get_issue_positions(callable)?;
            anyhow::ensure!(!positions.is_empty(), "no data for callable `{}`", callable);

            let mut issues = Vec::new();
            for position in &positions {
                let data = db.read_entry(position)?;
                let issue: pysa_model_explorer::types::Issue = serde_json::from_value(data)?;
                issues.push(issue);
            }

            let options = pysa_model_explorer::issue::IssueOptions {
                code: *code,
                handle: handle.clone(),
                ..Default::default()
            };
            pysa_model_explorer::issue::filter_issues(&mut issues, &options);

            println!("{}", issues.len());
        }
        Command::Search { pattern } => {
            let db = pysa_model_explorer::index::open_or_build_index(&cli.result_dir)?;
            let regex = regex::Regex::new(pattern)
                .context(format!("invalid regex pattern: {}", pattern))?;

            let matches = db.search_callables(&regex)?;

            for name in matches {
                println!("{}", name);
            }
        }
        Command::BuildIndex => {
            pysa_model_explorer::index::open_or_build_index(&cli.result_dir)?;
            println!("Index built successfully.");
        }
    }

    Ok(())
}
