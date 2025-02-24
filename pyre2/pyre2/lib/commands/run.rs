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

/// Set this to true to run profiling of fast jobs.
/// Will run this repeatedly.
const PROFILING: bool = false;

#[derive(Debug, Parser)]
struct Args {
    /// Enable verbose logging.
    #[clap(long = "verbose", short = 'v', global = true)]
    verbose: bool,

    #[command(subcommand)]
    command: Command,
}

impl Args {
    fn init_tracing(&self) {
        if !PROFILING {
            init_tracing(self.verbose, false);
        }
    }
}

#[derive(Debug, Subcommand)]
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
    if PROFILING {
        loop {
            let _ = run_once(false);
        }
    }
    run_once(true)
}

fn run_once(allow_forget: bool) -> anyhow::Result<ExitCode> {
    let args = Args::parse_from(get_args_expanded()?);
    args.init_tracing();
    match args.command {
        Command::Check(args) => args.run(allow_forget),
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(),
    }
}
