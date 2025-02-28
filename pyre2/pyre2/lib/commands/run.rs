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
        /// Files to check (glob supported).
        /// If no file is specified, switch to project-checking mode where the files to
        /// check are determined from the closest configuration file.
        files: Vec<String>,
        /// Watch for file changes and re-check them.
        #[clap(long, env = clap_env("WATCH"))]
        watch: bool,

        /// Explicitly set the Pyre configuration to use when type checking or starting a language server.
        /// It is an error to pass this flag in "single-file checking mode".
        /// When not set, Pyre will perform an upward-filesystem-walk approach to find the nearest
        /// pyre.toml or 'pyproject.toml with `tool.pyre` section'. If no config is found, Pyre exits with error.
        /// If both a pyre.toml and valid pyproject.toml are found, pyre.toml takes precedence.
        #[clap(long = "config-file", env = clap_env("CONFIG_FILE"))]
        config_file: Option<std::path::PathBuf>,

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
