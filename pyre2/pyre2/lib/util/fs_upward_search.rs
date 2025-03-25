/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

pub struct SearchResult {
    pub parent_directory: PathBuf,
    pub file_name: PathBuf,
}

impl SearchResult {
    pub fn to_path_buf(&self) -> PathBuf {
        self.parent_directory.join(&self.file_name)
    }
}

/// Search upward from the given directory `start_dir` for a file that matches any of the given `names`.
/// Return the first match found, or None if no match is found.
/// Elements in `names` are ordered: names that come earlier in the list are prioritized over names that come later.
/// It's the caller's responsibility to ensure `start_dir` is meaningful. This function simply assumes it is, without
/// checking e.g. whether `start_dir` is a directory or if it's an absolute path.
pub fn first_match(start_dir: &Path, names: &[impl AsRef<Path>]) -> Option<SearchResult> {
    let mut current_dir = start_dir;

    loop {
        for name in names {
            let candidate = current_dir.join(name);
            if candidate.exists() {
                return Some(SearchResult {
                    parent_directory: current_dir.to_path_buf(),
                    file_name: name.as_ref().to_owned(),
                });
            }
        }

        match current_dir.parent() {
            Some(parent) => current_dir = parent,
            None => break,
        }
    }
    None
}
