/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

/// Represents a list of files that need to be type checked.
pub trait FileList {
    /// Returns a list of files to type check.
    fn files(&self) -> anyhow::Result<Vec<PathBuf>>;
}
