/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use starlark_map::small_set::SmallSet;

use crate::util::fs_anyhow;

pub struct Globs(Vec<String>);

impl Globs {
    pub fn new(patterns: Vec<String>) -> Self {
        Self(patterns)
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
