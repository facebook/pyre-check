/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::process::ExitCode;

use clap::Subcommand;

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    /// Full type checking on a file or a project
    Check(crate::commands::check::Args),

    /// Entry point for Buck integration
    BuckCheck(crate::commands::buck_check::Args),

    /// Start an LSP server
    Lsp(crate::commands::lsp::Args),
}

pub fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<ExitCode> {
    match command {
        Command::Check(args) => args.run(allow_forget),
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(),
    }
}
