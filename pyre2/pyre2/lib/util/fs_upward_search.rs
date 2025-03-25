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

fn first_match_inner(
    start_dir: &Path,
    names: &[impl AsRef<Path>],
    stop_after: Option<&Path>,
) -> Option<SearchResult> {
    let mut current_dir = start_dir;

    loop {
        for name in names {
            let candidate = current_dir.join(name);
            if candidate.is_file() {
                return Some(SearchResult {
                    parent_directory: current_dir.to_path_buf(),
                    file_name: name.as_ref().to_owned(),
                });
            }
        }

        if let Some(stop_after) = stop_after
            && current_dir == stop_after
        {
            return None;
        }

        match current_dir.parent() {
            Some(parent) => current_dir = parent,
            None => break,
        }
    }
    None
}

/// Search upward from the given directory `start_dir` for a file that matches any of the given `names`.
/// Return the first match found, or None if no match is found.
/// Elements in `names` are ordered: names that come earlier in the list are prioritized over names that come later.
/// It's the caller's responsibility to ensure `start_dir` is meaningful. This function simply assumes it is, without
/// checking e.g. whether `start_dir` is a directory or if it's an absolute path.
pub fn first_match(start_dir: &Path, names: &[impl AsRef<Path>]) -> Option<SearchResult> {
    first_match_inner(start_dir, names, None)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use tempfile::TempDir;

    use super::*;

    fn assert_first_match(files: &[&str], from: &str, targets: &[&str], expected: Option<&str>) {
        let root = TempDir::new().unwrap();
        for file in files {
            let file_path = root.path().join(file);
            if let Some(parent) = file_path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&file_path, b"").unwrap();
        }
        let start_path = root.path().join(from);
        let expected = expected.map(PathBuf::from);
        let actual = first_match_inner(&start_path, targets, Some(root.path())).map(|result| {
            result
                .to_path_buf()
                .strip_prefix(root.path())
                .unwrap()
                .to_path_buf()
        });
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_first_match() {
        assert_first_match(&[], "", &["a"], None);

        assert_first_match(&["a"], "", &["a"], Some("a"));
        assert_first_match(&["a"], "", &["b"], None);
        assert_first_match(&["a", "b"], "", &["a", "b"], Some("a"));
        assert_first_match(&["b"], "", &["a", "b"], Some("b"));

        assert_first_match(&["a", "b/c"], "b", &["a"], Some("a"));
        assert_first_match(&["a", "b/c"], "b", &["b"], None);
        assert_first_match(&["a", "b/c"], "b", &["c"], Some("b/c"));
        assert_first_match(&["a", "b/c"], "b/c", &["c"], Some("b/c"));
        assert_first_match(&["a", "b/c"], "b", &["a", "c"], Some("b/c"));
        assert_first_match(&["a", "b/c"], "b", &["d"], None);

        assert_first_match(&["a/d", "a/b/d", "a/b/c/d"], "", &["d"], None);
        assert_first_match(&["a/d", "a/b/d", "a/b/c/d"], "a", &["d"], Some("a/d"));
        assert_first_match(&["a/d", "a/b/d", "a/b/c/d"], "a/b", &["d"], Some("a/b/d"));
        assert_first_match(
            &["a/d", "a/b/d", "a/b/c/d"],
            "a/b/c",
            &["d"],
            Some("a/b/c/d"),
        );

        assert_first_match(&["a/d", "a/b/e", "a/b/c/f"], "", &["d"], None);
        assert_first_match(&["a/d", "a/b/e", "a/b/c/f"], "a", &["d"], Some("a/d"));
        assert_first_match(&["a/d", "a/b/e", "a/b/c/f"], "a/b", &["d"], Some("a/d"));
        assert_first_match(&["a/d", "a/b/e", "a/b/c/f"], "a/b/c", &["d"], Some("a/d"));
        assert_first_match(&["a/d", "a/b/e", "a/b/c/f"], "a/b/e", &["d"], Some("a/d"));
    }
}
