/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use crate::state::handle::Handle;
use crate::state::steps::Load;

/// Trait to capture which handles are executed by `State`.
/// Calls to `start_work` and `finish_work` will be paired.
/// It may be the case that a single `Handle` is computed multiple times within a single call to `run`.
///
/// The `Subscriber` will be invoked on a working thread, so should complete quickly.
pub trait Subscriber: Sync {
    /// We are starting work on a `Handle`, with the intention to computing at
    /// least some of its steps.
    fn start_work(&self, handle: Handle);

    /// We have finished work on a `Handle`, having computed its solutions.
    /// While we have computed the solutions, we return the `Load` as that contains
    /// the `ErrorCollector` and `ModuleInfo` which are useful context for the completion.
    fn finish_work(&self, handle: Handle, result: Arc<Load>);
}
