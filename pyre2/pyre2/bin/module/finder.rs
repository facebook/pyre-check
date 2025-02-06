/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use tracing::debug;

use crate::module::module_name::ModuleName;

pub fn find_module(module: ModuleName, include: &[PathBuf]) -> Option<PathBuf> {
    let parts = module.components();
    let possibilities = vec![
        parts.join("/") + ".pyi",
        parts.join("/") + ".py",
        parts.join("/") + "/__init__.pyi",
        parts.join("/") + "/__init__.py",
    ];

    for include in include {
        for suffix in &possibilities {
            let path = include.join(suffix);
            if path.exists() {
                debug!("Found {module} at {}", path.display());
                return Some(path);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    // Utility structure to facilitate setting up filesystem structure under test directories.
    enum TestPathKind {
        File,
        Directory(Vec<TestPath>),
    }
    struct TestPath {
        name: String,
        kind: TestPathKind,
    }

    impl TestPath {
        fn file(name: &str) -> Self {
            Self {
                name: name.to_owned(),
                kind: TestPathKind::File,
            }
        }
        fn dir(name: &str, children: Vec<TestPath>) -> Self {
            Self {
                name: name.to_owned(),
                kind: TestPathKind::Directory(children),
            }
        }
    }

    fn setup_test_directory(root: &Path, paths: Vec<TestPath>) {
        for path in paths {
            match path.kind {
                TestPathKind::File => {
                    std::fs::File::create(root.join(path.name)).unwrap();
                }
                TestPathKind::Directory(children) => {
                    let dir = root.join(path.name);
                    std::fs::create_dir(&dir).unwrap();
                    setup_test_directory(&dir, children);
                }
            }
        }
    }

    #[test]
    fn test_find_module_simple() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![TestPath::dir(
                "foo",
                vec![TestPath::file("bar.py"), TestPath::file("baz.pyi")],
            )],
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.bar"), &[root.to_path_buf()]),
            Some(root.join("foo/bar.py"))
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.baz"), &[root.to_path_buf()]),
            Some(root.join("foo/baz.pyi"))
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.qux"), &[root.to_path_buf()]),
            None,
        );
    }

    #[test]
    fn test_find_module_init() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![TestPath::dir(
                "foo",
                vec![
                    TestPath::dir("bar", vec![TestPath::file("__init__.py")]),
                    TestPath::dir("baz", vec![TestPath::file("__init__.pyi")]),
                ],
            )],
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.bar"), &[root.to_path_buf()]),
            Some(root.join("foo/bar/__init__.py"))
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.baz"), &[root.to_path_buf()]),
            Some(root.join("foo/baz/__init__.pyi"))
        );
    }

    #[test]
    fn test_find_pyi_takes_precedence() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![TestPath::dir(
                "foo",
                vec![TestPath::file("bar.pyi"), TestPath::file("bar.py")],
            )],
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.bar"), &[root.to_path_buf()]),
            Some(root.join("foo/bar.pyi"))
        );
    }
}
