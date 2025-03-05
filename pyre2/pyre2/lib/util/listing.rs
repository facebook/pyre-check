/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

/// Represents a list of files that need to be type checked.
pub trait FileList {
    /// Returns a list of containing directories for all the files in the list.
    /// This is intended to be used by file watchers to determine which directories on
    /// disk need to be watched for changes. In theory, it's not wrong to return either
    /// the filesystem root directory or a long list that contains the parent directory
    /// for every single file returned by `files()`. But in practice we want to make the
    /// returned directories specified enough to minimize watching on irrelevant files, and
    /// at the same time to make the directories general enough to keep the list size small.
    fn roots(&self) -> Vec<PathBuf>;

    /// Returns a list of files to type check.
    fn files(&self) -> anyhow::Result<Vec<PathBuf>>;
}
