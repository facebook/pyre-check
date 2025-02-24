/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::process::ExitCode;

use clap::Parser;
use clap::Subcommand;

use crate::util::args::get_args_expanded;
use crate::util::trace::init_tracing;

#[derive(Debug, Parser)]
struct Args {
    /// Enable verbose logging.
    #[clap(long = "verbose", short = 'v', global = true)]
    verbose: bool,

    /// Set this to true to run profiling of fast jobs.
    /// Will run the command repeatedly.
    #[clap(long = "profiling", global = true, hide = true)]
    profiling: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Clone, Subcommand)]
#[command(name = "pyre2")]
#[command(about = "Next generation of Pyre type checker", long_about = None)]
enum Command {
    /// Full type checking on a file or a project
    Check(crate::commands::check::Args),

    /// Entry point for Buck integration
    BuckCheck(crate::commands::buck_check::Args),

    /// Start an LSP server
    Lsp(crate::commands::lsp::Args),
}

/// Run based on the command line arguments.
pub fn run() -> anyhow::Result<ExitCode> {
    let args = Args::parse_from(get_args_expanded()?);
    if args.profiling {
        loop {
            let _ = run_command(args.command.clone(), false);
        }
    } else {
        init_tracing(args.verbose, false);
        run_command(args.command, true)
    }
}

fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<ExitCode> {
    match command {
        Command::Check(args) => args.run(allow_forget),
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(),
    }
}
