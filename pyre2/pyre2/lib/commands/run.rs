/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Parser;

use crate::clap_env;
pub use crate::commands::buck_check::Args as BuckCheckArgs;
pub use crate::commands::check::Args as CheckArgs;
pub use crate::commands::lsp::Args as LspArgs;
use crate::util::thread_pool::ThreadCount;

#[derive(Debug, Parser, Clone)]
pub struct CommonGlobalArgs {
    /// Number of threads to use for parallelization.
    /// Setting the value to 1 implies sequential execution without any parallelism.
    /// Setting the value to 0 means to pick the number of threads automatically using default heuristics.
    #[clap(long, short = 'j', default_value = "0", global = true, env = clap_env("THREADS"))]
    pub threads: ThreadCount,
}

/// Exit status of a command, if the run is completed.
pub enum CommandExitStatus {
    /// The command completed without an issue.
    Success,
    /// The command completed, but problems (e.g. type errors) were found.
    UserError,
    /// An error occurred in the environment or the underlying infrastructure,
    /// which prevents the command from completing.
    InfraError,
}
