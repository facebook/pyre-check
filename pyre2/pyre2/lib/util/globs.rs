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
        //! Create a new `Globs` from the given patterns. If you want them to be relative
        //! to a root, please use `Globs::new_with_root()` instead.
        Self(patterns)
    }

    pub fn new_with_root(root: &Path, patterns: Vec<String>) -> Self {
        //! Create a new `Globs`, rewriting all patterns to be relative to `root`.
        if root == Path::new("") || root == Path::new(".") {
            return Self(patterns);
        }
        Self(
            patterns
                .into_iter()
                .map(|pattern| Self::pattern_relative_to_root(root, pattern))
                .collect(),
        )
    }

    pub fn from_root(self, root: &Path) -> Self {
        // TODO(connernilsen): store root as part of globs to make it easier to rewrite later on
        Self::new_with_root(root, self.0)
    }

    /// Given a glob pattern, return the directories that can contain files that match the pattern.
    pub fn roots(&self) -> Vec<PathBuf> {
        self.0.map(|s| Self::get_root_for_pattern(s))
    }

    fn pattern_relative_to_root(root: &Path, pattern: String) -> String {
        let parsed_pattern = Path::new(&pattern);
        if parsed_pattern.has_root() {
            return pattern;
        }

        let mut relative_path = root.to_path_buf();
        parsed_pattern.components().for_each(|comp: Component| {
            match comp {
                prefix @ Component::Prefix(_) => {
                    // we'll only hit this if we're on windows -- this will fully replace `relative_path`
                    relative_path.push(prefix);
                    match root.components().next() {
                        // if we're on a different drive, give up because we can only hope
                        // this relative path is correct
                        Some(root_prefix @ Component::Prefix(_)) if prefix != root_prefix => (),
                        // otherwise, push the root, and prepare to push the rest of the path
                        _ => relative_path.push(root),
                    }
                }
                Component::RootDir => unreachable!(
                    "There can't be a root in here, since we already checked for a root"
                ),
                rest => relative_path.push(rest),
            }
        });

        // this must be in String form, the Glob library doesn't take path bufs
        relative_path.to_string_lossy().to_string()
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
        let pattern = Path::new(pattern);
        let mut path = PathBuf::new();

        // we need to add any path prefix and root items (there should be at most one of each,
        // and prefix only exists on windows) to the root we're building
        pattern
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

    fn is_python_extension(ext: Option<&OsStr>) -> bool {
        ext.is_some_and(|e| e == "py" || e == "pyi")
    }

    fn resolve_dir(path: &Path, results: &mut Vec<PathBuf>) -> anyhow::Result<()> {
        for entry in fs_anyhow::read_dir(path)? {
            let entry = entry
                .with_context(|| format!("When iterating over directory `{}`", path.display()))?;
            let path = entry.path();
            if path.is_dir() {
                Self::resolve_dir(&path, results)?;
            } else if Self::is_python_extension(path.extension()) {
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
            } else if Self::is_python_extension(path.extension()) {
                result.push(path);
            }
        }
        Ok(result)
    }
}

impl FileList for Globs {
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

        if cfg!(windows) {
            // These all use the \ separator, which only works on Windows.
            f(r"C:\\windows\project\**\files", r"C:\\windows\project");
            f(r"c:\windows\project\**\files", r"c:\windows\project");
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

    #[test]
    fn test_globs_relative_to_root() {
        let mut inputs: Vec<&str> = vec![
            "project/**/files",
            "**/files",
            "pattern",
            "pattern.txt",
            "a/b",
            "a/b/c.txt",
            "a/b*/c",
            "a/b/*.txt",
            "/**",
            "/absolute/path/**/files",
        ];
        if cfg!(windows) {
            inputs.extend([r"c:\absolute\path\**", r"c:relative\path\**"]);
        }
        let inputs: Vec<String> = inputs.into_iter().map(String::from).collect();

        let f = |root: &str, expected: Vec<&str>, windows_extras: Vec<&str>| {
            let mut expected: Vec<String> = expected.into_iter().map(String::from).collect();
            let mut inputs = inputs.clone();
            let mut root = root.to_owned();

            // because windows will construct all paths with `\` instead of `/`, let's make things simple by
            // switching evertyhing over to that ourselves
            if cfg!(windows) {
                expected.extend(
                    windows_extras
                        .into_iter()
                        .map(String::from)
                        .collect::<Vec<String>>(),
                );
                expected.iter_mut().for_each(|s| *s = s.replace("/", r"\"));
                inputs.iter_mut().for_each(|s| *s = s.replace("/", r"\"));
                root = root.replace("/", r"\");
            }
            let globs = Globs::new_with_root(Path::new(&root), inputs);
            assert_eq!(globs.0, expected, "with root {:?}", root);
        };

        f(
            "",
            vec![
                "project/**/files",
                "**/files",
                "pattern",
                "pattern.txt",
                "a/b",
                "a/b/c.txt",
                "a/b*/c",
                "a/b/*.txt",
                "/**",
                "/absolute/path/**/files",
            ],
            vec![r"c:\absolute\path\**", r"c:relative\path\**"],
        );
        f(
            ".",
            vec![
                "project/**/files",
                "**/files",
                "pattern",
                "pattern.txt",
                "a/b",
                "a/b/c.txt",
                "a/b*/c",
                "a/b/*.txt",
                "/**",
                "/absolute/path/**/files",
            ],
            vec![r"c:\absolute\path\**", r"c:relative\path\**"],
        );
        f(
            "..",
            vec![
                "../project/**/files",
                "../**/files",
                "../pattern",
                "../pattern.txt",
                "../a/b",
                "../a/b/c.txt",
                "../a/b*/c",
                "../a/b/*.txt",
                "/**",
                "/absolute/path/**/files",
            ],
            vec![r"c:\absolute\path\**", r"c:..\relative\path\**"],
        );
        f(
            "no/trailing/slash",
            vec![
                "no/trailing/slash/project/**/files",
                "no/trailing/slash/**/files",
                "no/trailing/slash/pattern",
                "no/trailing/slash/pattern.txt",
                "no/trailing/slash/a/b",
                "no/trailing/slash/a/b/c.txt",
                "no/trailing/slash/a/b*/c",
                "no/trailing/slash/a/b/*.txt",
                "/**",
                "/absolute/path/**/files",
            ],
            vec![
                r"c:\absolute\path\**",
                r"c:no\trailing\slash\relative\path\**",
            ],
        );
        f(
            "relative/path/to/",
            vec![
                "relative/path/to/project/**/files",
                "relative/path/to/**/files",
                "relative/path/to/pattern",
                "relative/path/to/pattern.txt",
                "relative/path/to/a/b",
                "relative/path/to/a/b/c.txt",
                "relative/path/to/a/b*/c",
                "relative/path/to/a/b/*.txt",
                "/**",
                "/absolute/path/**/files",
            ],
            vec![
                r"c:\absolute\path\**",
                r"c:relative\path\to\relative\path\**",
            ],
        );
        f(
            "/my/path/to",
            vec![
                "/my/path/to/project/**/files",
                "/my/path/to/**/files",
                "/my/path/to/pattern",
                "/my/path/to/pattern.txt",
                "/my/path/to/a/b",
                "/my/path/to/a/b/c.txt",
                "/my/path/to/a/b*/c",
                "/my/path/to/a/b/*.txt",
                "/**",
                "/absolute/path/**/files",
            ],
            vec![r"c:\absolute\path\**", r"c:\my\path\to\relative\path\**"],
        );
        if cfg!(windows) {
            f(
                r"c:\my\path\to",
                vec![
                    "c:/my/path/to/project/**/files",
                    "c:/my/path/to/**/files",
                    "c:/my/path/to/pattern",
                    "c:/my/path/to/pattern.txt",
                    "c:/my/path/to/a/b",
                    "c:/my/path/to/a/b/c.txt",
                    "c:/my/path/to/a/b*/c",
                    "c:/my/path/to/a/b/*.txt",
                    "/**",
                    "/absolute/path/**/files",
                ],
                vec![r"c:\absolute\path\**", r"c:\my\path\to\relative\path\**"],
            );
            f(
                r"c:my\path\to",
                vec![
                    "c:my/path/to/project/**/files",
                    "c:my/path/to/**/files",
                    "c:my/path/to/pattern",
                    "c:my/path/to/pattern.txt",
                    "c:my/path/to/a/b",
                    "c:my/path/to/a/b/c.txt",
                    "c:my/path/to/a/b*/c",
                    "c:my/path/to/a/b/*.txt",
                    "/**",
                    "/absolute/path/**/files",
                ],
                vec![r"c:\absolute\path\**", r"c:my\path\to\relative\path\**"],
            );
            f(
                r"d:\my\path\to",
                vec![
                    "d:/my/path/to/project/**/files",
                    "d:/my/path/to/**/files",
                    "d:/my/path/to/pattern",
                    "d:/my/path/to/pattern.txt",
                    "d:/my/path/to/a/b",
                    "d:/my/path/to/a/b/c.txt",
                    "d:/my/path/to/a/b*/c",
                    "d:/my/path/to/a/b/*.txt",
                    "/**",
                    "/absolute/path/**/files",
                ],
                vec![r"c:\absolute\path\**", r"c:relative\path\**"],
            );
            f(
                r"d:my\path\to",
                vec![
                    "d:my/path/to/project/**/files",
                    "d:my/path/to/**/files",
                    "d:my/path/to/pattern",
                    "d:my/path/to/pattern.txt",
                    "d:my/path/to/a/b",
                    "d:my/path/to/a/b/c.txt",
                    "d:my/path/to/a/b*/c",
                    "d:my/path/to/a/b/*.txt",
                    "/**",
                    "/absolute/path/**/files",
                ],
                vec![r"c:\absolute\path\**", r"c:relative\path\**"],
            );
        }
    }

    #[test]
    fn test_is_python_extension() {
        assert!(!Globs::is_python_extension(None));
        assert!(!Globs::is_python_extension(Some(OsStr::new(
            "hello world!"
        ))));
        assert!(Globs::is_python_extension(Some(OsStr::new("py"))));
        assert!(Globs::is_python_extension(Some(OsStr::new("pyi"))));
    }
}
