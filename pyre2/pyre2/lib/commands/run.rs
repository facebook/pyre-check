/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

/// Exit status of a command, if the run is completed.
pub enum CommandExitStatus {
    /// The command completed without an issue.
    Success,
    /// The command completed, but problems (e.g. type errors) were found.
    UserError,
}

/// `Ok` means we successfully ran the command and returned with the given status.
/// `Err` means we unexpectedly crashed while running the command.
pub fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<CommandExitStatus> {
    match command {
        Command::Check(args) => args.run(allow_forget),
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(),
    }
}
