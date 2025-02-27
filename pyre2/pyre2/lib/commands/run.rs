/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Subcommand;

use crate::clap_env;
pub use crate::commands::buck_check::Args as BuckCheckArgs;
pub use crate::commands::check::Args as CheckArgs;
pub use crate::commands::lsp::Args as LspArgs;

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    /// Full type checking on a file or a project
    Check {
        /// Files to check (glob supported)
        files: Vec<String>,
        /// Watch for file changes and re-check them.
        #[clap(long, env = clap_env("WATCH"))]
        watch: bool,

        #[clap(flatten)]
        args: CheckArgs,
    },

    /// Entry point for Buck integration
    BuckCheck(BuckCheckArgs),

    /// Start an LSP server
    Lsp(LspArgs),
}

/// Exit status of a command, if the run is completed.
pub enum CommandExitStatus {
    /// The command completed without an issue.
    Success,
    /// The command completed, but problems (e.g. type errors) were found.
    UserError,
}
