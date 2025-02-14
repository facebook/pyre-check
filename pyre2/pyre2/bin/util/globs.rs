/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use starlark_map::small_set::SmallSet;

use crate::util::fs_anyhow;
use crate::util::prelude::SliceExt;

pub struct Globs(Vec<String>);

impl Globs {
    pub fn new(patterns: Vec<String>) -> Self {
        Self(patterns)
    }

    /// Given a glob pattern, return the directories that can contain files that match the pattern.
    pub fn roots(&self) -> Vec<PathBuf> {
        self.0.map(|pattern| {
            let mut path = PathBuf::new();
            for component in pattern.split(path::is_separator) {
                if component.contains('*') {
                    break;
                }
                path.push(component);
            }
            if path.extension().is_some() {
                path.pop();
            }
            path
        })
    }

    pub fn resolve(&self) -> anyhow::Result<Vec<PathBuf>> {
        let mut result = SmallSet::new();
        for pattern in &self.0 {
            let res = Self::resolve_pattern(pattern)
                .with_context(|| format!("When resolving pattern `{pattern}`"))?;
            if res.is_empty() {
                return Err(anyhow::anyhow!("No files matched pattern `{}`", pattern));
            }
            result.extend(res);
        }
        Ok(result.into_iter().collect())
    }

    pub fn resolve_dir(path: &Path, results: &mut Vec<PathBuf>) -> anyhow::Result<()> {
        for entry in fs_anyhow::read_dir(path)? {
            let entry = entry
                .with_context(|| format!("When iterating over directory `{}`", path.display()))?;
            let path = entry.path();
            if path.is_dir() {
                Self::resolve_dir(&path, results)?;
            } else if let Some(ext) = path.extension()
                && (ext == "py" || ext == "pyi")
            {
                results.push(path);
            }
        }
        Ok(())
    }

    fn resolve_pattern(pattern: &str) -> anyhow::Result<Vec<PathBuf>> {
        let mut result = Vec::new();
        let paths = glob::glob(pattern)?;
        for path in paths {
            let path = path?;
            if path.is_dir() {
                Self::resolve_dir(&path, &mut result)?;
            } else {
                result.push(path);
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn test_roots() {
        fn f(pattern: &str, root: &str) {
            let globs = Globs::new(vec![pattern.to_owned()]);
            assert_eq!(globs.roots(), vec![PathBuf::from(root)]);
        }

        f("project/**/files", "project");
        f("**/files", "");
        f("pattern", "pattern");
        f("pattern.txt", "");
        f("a/b", "a/b");
        f("a/b/c.txt", "a/b");
        f("a/b*/c", "a");
        f("a/b/*.txt", "a/b");
    }
}
