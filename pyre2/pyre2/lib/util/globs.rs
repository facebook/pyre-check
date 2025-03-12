/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::ffi::OsString;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use serde::Deserialize;
use starlark_map::small_set::SmallSet;

use crate::util::fs_anyhow;
use crate::util::listing::FileList;
use crate::util::prelude::SliceExt;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Globs(Vec<String>);

impl Globs {
    pub fn new(patterns: Vec<String>) -> Self {
        Self(patterns)
    }

    fn contains_asterisk(part: &OsStr) -> bool {
        let asterisk = OsString::from("*");
        let asterisk = asterisk.as_encoded_bytes();
        let bytes = part.as_encoded_bytes();

        if bytes == asterisk {
            return true;
        } else if asterisk.len() > bytes.len() {
            return false;
        }

        for i in 0..=bytes.len() - asterisk.len() {
            if *asterisk == bytes[i..i + asterisk.len()] {
                return true;
            }
        }
        false
    }

    fn get_root_for_pattern(pattern: &str) -> PathBuf {
        let mut path = PathBuf::new();

        // we need to add any path prefix and root items (there should be at most one of each,
        // and prefix only exists on windows) to the root we're building
        let parsed_path = PathBuf::from(pattern);
        parsed_path
            .components()
            .take_while(|comp| {
                match comp {
                    // this should be alright to do, since a prefix will always come before a root,
                    // which will always come before the rest of the path
                    Component::Prefix(_)
                    | Component::RootDir
                    | Component::CurDir
                    | Component::ParentDir => true,
                    Component::Normal(part) => !Self::contains_asterisk(part),
                }
            })
            .for_each(|comp| path.push(comp));
        if path.extension().is_some() {
            path.pop();
        }
        path
    }

    fn resolve_dir(path: &Path, results: &mut Vec<PathBuf>) -> anyhow::Result<()> {
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

impl FileList for Globs {
    /// Given a glob pattern, return the directories that can contain files that match the pattern.
    fn roots(&self) -> Vec<PathBuf> {
        self.0.map(|s| Self::get_root_for_pattern(s))
    }

    fn files(&self) -> anyhow::Result<Vec<PathBuf>> {
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
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    #[cfg_attr(target_family = "windows", ignore)]
    fn test_roots() {
        fn f(pattern: &str, root: &str) {
            let globs = Globs::new(vec![pattern.to_owned()]);
            assert_eq!(
                globs.roots(),
                vec![PathBuf::from(root)],
                "Glob parsing failed for pattern {}",
                pattern
            );
        }

        f("project/**/files", "project");
        f("**/files", "");
        f("pattern", "pattern");
        f("pattern.txt", "");
        f("a/b", "a/b");
        f("a/b/c.txt", "a/b");
        f("a/b*/c", "a");
        f("a/b/*.txt", "a/b");
        f("/**", "/");
        f("/absolute/path/**/files", "/absolute/path");
    }

    #[test]
    #[cfg_attr(not(target_family = "windows"), ignore)]
    fn test_windows_roots() {
        fn f(pattern: &str, root: &str) {
            let globs = Globs::new(vec![pattern.to_owned()]);
            assert_eq!(globs.roots(), vec![PathBuf::from(root)]);
        }

        f(r"C:\\windows\project\**\files", r"C:\\windows\project");
        f(
            r"c:\windows\project\**\files",
            r"c:\windows\project\**files",
        );
        f(r"\windows\project\**\files", r"\windows\project");
        f(r"c:project\**\files", "c:project");
        f(r"project\**\files", "project");
        f(r"**\files", "");
        f("pattern", "pattern");
        f("pattern.txt", "");
        f(r"a\b", r"a\b");
        f(r"a\b\c.txt", r"a\b");
        f(r"a\b*\c", "a");
        f(r"a\b\*.txt", r"a\b");
    }

    #[test]
    fn test_contains_asterisk() {
        assert!(!Globs::contains_asterisk(&OsString::from("")));
        assert!(Globs::contains_asterisk(&OsString::from("*")));
        assert!(Globs::contains_asterisk(&OsString::from("*a")));
        assert!(Globs::contains_asterisk(&OsString::from("a*")));
        assert!(!Globs::contains_asterisk(&OsString::from("abcd")));
        assert!(Globs::contains_asterisk(&OsString::from("**")));
        assert!(Globs::contains_asterisk(&OsString::from("asdf*fdsa")));
    }
}
