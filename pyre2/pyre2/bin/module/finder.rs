/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use ruff_python_ast::name::Name;
use vec1::Vec1;

use crate::module::module_name::ModuleName;

enum FindResult {
    /// Found a single-file module. The path must not point to an __init__ file.
    SingleFileModule(PathBuf),
    /// Found a regular package. First path must point to an __init__ file.
    /// Second path indicates where to continue search next. It should always point to the parent of the __init__ file.
    RegularPackage(PathBuf, PathBuf),
    /// Found a namespace package.
    /// The path component indicates where to continue search next. It may contain more than one directories as the namespace package
    /// may span across multiple search roots.
    NamespacePackage(Vec1<PathBuf>),
}

fn find_one_part(name: &Name, roots: &[PathBuf]) -> Option<FindResult> {
    let mut namespace_roots = Vec::new();
    for root in roots {
        let candidate_dir = root.join(name.as_str());
        // First check if `name` corresponds to a regular package.
        for candidate_init_suffix in ["__init__.pyi", "__init__.py"] {
            let init_path = candidate_dir.join(candidate_init_suffix);
            if init_path.exists() {
                return Some(FindResult::RegularPackage(init_path, candidate_dir));
            }
        }
        // Second check if `name` corresponds to a single-file module.
        for candidate_file_suffix in ["pyi", "py"] {
            let candidate_path = root.join(format!("{name}.{candidate_file_suffix}"));
            if candidate_path.exists() {
                return Some(FindResult::SingleFileModule(candidate_path));
            }
        }
        // Finally check if `name` corresponds to a namespace package.
        if candidate_dir.is_dir() {
            namespace_roots.push(candidate_dir);
        }
    }
    match Vec1::try_from_vec(namespace_roots) {
        Err(_) => None,
        Ok(namespace_roots) => Some(FindResult::NamespacePackage(namespace_roots)),
    }
}

pub fn find_module(module: ModuleName, include: &[PathBuf]) -> Option<PathBuf> {
    let parts = module.components();
    if parts.is_empty() {
        return None;
    }
    let mut current_result = find_one_part(&parts[0], include);
    for part in parts.iter().skip(1) {
        match current_result {
            None => {
                // Nothing has been found in the previous round. No point keep looking.
                break;
            }
            Some(FindResult::SingleFileModule(_)) => {
                // We've already reached leaf nodes. Cannot keep searching
                current_result = None;
                break;
            }
            Some(FindResult::RegularPackage(_, next_root)) => {
                current_result = find_one_part(part, &[next_root]);
            }
            Some(FindResult::NamespacePackage(next_roots)) => {
                current_result = find_one_part(part, &next_roots);
            }
        }
    }
    current_result.map(|x| match x {
        FindResult::SingleFileModule(path) | FindResult::RegularPackage(path, _) => path,
        FindResult::NamespacePackage(roots) => {
            // TODO(grievejia): Preserving all info in the list instead of dropping all but the first one.
            roots.first().clone()
        }
    })
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
                vec![
                    TestPath::file("__init__.py"),
                    TestPath::file("bar.py"),
                    TestPath::file("baz.pyi"),
                ],
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
                    TestPath::file("__init__.py"),
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
                vec![
                    TestPath::file("__init__.py"),
                    TestPath::file("bar.pyi"),
                    TestPath::file("bar.py"),
                ],
            )],
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.bar"), &[root.to_path_buf()]),
            Some(root.join("foo/bar.pyi"))
        );
    }

    #[test]
    fn test_find_init_takes_precedence() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![TestPath::dir(
                "foo",
                vec![
                    TestPath::file("__init__.py"),
                    TestPath::file("bar.py"),
                    TestPath::dir("bar", vec![TestPath::file("__init__.py")]),
                ],
            )],
        );
        assert_eq!(
            find_module(ModuleName::from_str("foo.bar"), &[root.to_path_buf()]),
            Some(root.join("foo/bar/__init__.py"))
        );
    }

    #[test]
    fn test_basic_namespace_package() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![
                TestPath::dir("a", vec![]),
                TestPath::dir("b", vec![TestPath::dir("c", vec![])]),
                TestPath::dir("c", vec![TestPath::dir("d", vec![TestPath::file("e.py")])]),
            ],
        );
        let search_roots = [root.to_path_buf()];
        assert_eq!(
            find_module(ModuleName::from_str("a"), &search_roots),
            Some(root.join("a"))
        );
        assert_eq!(
            find_module(ModuleName::from_str("b"), &search_roots),
            Some(root.join("b"))
        );
        assert_eq!(
            find_module(ModuleName::from_str("c.d"), &search_roots),
            Some(root.join("c/d"))
        );
        assert_eq!(
            find_module(ModuleName::from_str("c.d.e"), &search_roots),
            Some(root.join("c/d/e.py"))
        );
    }

    #[test]
    fn test_find_regular_package_early_return() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![
                TestPath::dir(
                    "search_root0",
                    vec![TestPath::dir(
                        "a",
                        vec![TestPath::file("__init__.py"), TestPath::file("b.py")],
                    )],
                ),
                TestPath::dir(
                    "search_root1",
                    vec![TestPath::dir(
                        "a",
                        vec![TestPath::file("__init__.py"), TestPath::file("c.py")],
                    )],
                ),
            ],
        );
        assert_eq!(
            find_module(
                ModuleName::from_str("a.c"),
                &[root.join("search_root0"), root.join("search_root1")]
            ),
            // We won't find `a.c` because when searching for package `a`, we've already
            // committed to `search_root0/a/` as the path to search next for `c`. And there's
            // no `c.py` in `search_root0/a/`.
            None
        );
    }

    #[test]
    fn test_find_namespace_package_no_early_return() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = tempdir.path();
        setup_test_directory(
            root,
            vec![
                TestPath::dir(
                    "search_root0",
                    vec![TestPath::dir("a", vec![TestPath::file("b.py")])],
                ),
                TestPath::dir(
                    "search_root1",
                    vec![TestPath::dir("a", vec![TestPath::file("c.py")])],
                ),
            ],
        );
        assert_eq!(
            find_module(
                ModuleName::from_str("a.c"),
                &[root.join("search_root0"), root.join("search_root1")]
            ),
            // We will find `a.c` because `a` is a namespace package whose search roots
            // include both `search_root0/a/` and `search_root1/a/`.
            Some(root.join("search_root1/a/c.py"))
        );
    }
}
