/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

/// Represents a list of files that need to be type checked.
pub trait FileList: Clone {
    /// Returns a list of files to type check.
    fn files(&self) -> anyhow::Result<Vec<PathBuf>>;

    /// Returns true if the given file path is covered by this list.
    /// The input path does not necessarily need to exist. For non-existent files, the return value would
    /// be true if the FileList would have included the file had it existed. As a result, `covers(path)` is
    /// not guaranteed to return the same value as `files().contains(path)`.
    /// This function is primarily used by incremental check to determine if a removed file or a newly created
    /// file on disk would affect the file listing result or not.
    fn covers(&self, path: &Path) -> bool;
}
