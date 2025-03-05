/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub use crate::commands::buck_check::Args as BuckCheckArgs;
pub use crate::commands::check::Args as CheckArgs;
pub use crate::commands::lsp::Args as LspArgs;

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
