/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;

use anyhow::anyhow;
use anyhow::Context as _;
use starlark_map::small_map::SmallMap;
use tracing::debug;
use vec1::Vec1;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::state::loader::LoadResult;
use crate::state::loader::Loader;
use crate::util::fs_anyhow;

#[derive(Debug, PartialEq)]
struct ManifestItem {
    module_name: ModuleName,
    relative_path: PathBuf,
}

#[derive(Debug, PartialEq)]
pub struct BuckSourceDatabase {
    sources: SmallMap<ModuleName, Vec1<PathBuf>>,
    dependencies: SmallMap<ModuleName, Vec1<PathBuf>>,
}

/// Return type from `BuckSourceDatabase::lookup`
#[derive(Debug, PartialEq)]
enum LookupResult {
    /// Source file of this module is owned by the current target.
    /// Type errors in the file should be reported to user.
    OwningSource(PathBuf),
    /// Source file of this module is owned by the dependency of the current target.
    /// The file should be analyzed but no type errors should be reported.
    ExternalSource(PathBuf),
    /// Did not find any source file associated with the given module name.
    NoSource,
}

fn read_manifest_file_data(data: &[u8]) -> anyhow::Result<Vec<ManifestItem>> {
    let raw_items: Vec<Vec<String>> = serde_json::from_slice(data)?;
    let mut results = Vec::new();
    for raw_item in raw_items {
        match ModuleName::from_relative_path(Path::new(raw_item[0].as_str())) {
            Ok(module_name) => {
                let relative_path = PathBuf::from(raw_item[1].clone());
                results.push(ManifestItem {
                    module_name,
                    relative_path,
                });
            }
            Err(error) => {
                // This often happens for buckified 3rd-party targets
                debug!("Cannot convert path to module name: {error:#}");
            }
        }
    }
    Ok(results)
}

fn read_manifest_file(path: &Path) -> anyhow::Result<Vec<ManifestItem>> {
    let data = fs_anyhow::read(path)?;
    read_manifest_file_data(&data)
        .with_context(|| format!("failed to parse manifest JSON `{}`", path.display()))
}

fn read_manifest_files(manifest_paths: &[PathBuf]) -> anyhow::Result<Vec<ManifestItem>> {
    let mut result = Vec::new();
    for manifest_path in manifest_paths {
        let manifest_items = read_manifest_file(manifest_path.as_path())?;
        result.extend(manifest_items);
    }
    Ok(result)
}

fn same_module_path_compare(left: &Path, right: &Path) -> Ordering {
    // .pyi file always comes before .py file
    match (
        left.extension().and_then(OsStr::to_str),
        right.extension().and_then(OsStr::to_str),
    ) {
        (Some("pyi"), Some("py")) => Ordering::Less,
        (Some("py"), Some("pyi")) => Ordering::Greater,
        _ => Ordering::Equal, // Preserve original ordering by default
    }
}

fn create_manifest_item_index(
    items: impl Iterator<Item = ManifestItem>,
) -> SmallMap<ModuleName, Vec1<PathBuf>> {
    let mut accumulated: SmallMap<ModuleName, Vec<PathBuf>> =
        SmallMap::with_capacity(items.size_hint().0);
    for item in items {
        accumulated
            .entry(item.module_name)
            .or_default()
            .push(item.relative_path);
    }
    accumulated
        .into_iter()
        .map(|(name, mut paths)| {
            paths.sort_by(|left, right| same_module_path_compare(left, right));
            (name, Vec1::try_from_vec(paths).unwrap())
        })
        .collect()
}

impl BuckSourceDatabase {
    pub fn from_manifest_files(
        source_manifests: &[PathBuf],
        dependency_manifests: &[PathBuf],
        typeshed_manifests: &[PathBuf],
    ) -> anyhow::Result<Self> {
        let sources = read_manifest_files(source_manifests)?;
        let dependencies = read_manifest_files(dependency_manifests)?;
        let typeshed = read_manifest_files(typeshed_manifests)?;
        Ok(Self::from_manifest_items(sources, dependencies, typeshed))
    }

    fn from_manifest_items(
        source_items: Vec<ManifestItem>,
        dependency_items: Vec<ManifestItem>,
        typeshed_items: Vec<ManifestItem>,
    ) -> Self {
        BuckSourceDatabase {
            sources: create_manifest_item_index(source_items.into_iter()),
            dependencies: create_manifest_item_index(
                dependency_items.into_iter().chain(typeshed_items),
            ),
        }
    }

    pub fn modules_to_check(&self) -> Vec<ModuleName> {
        self.sources.keys().copied().collect()
    }

    fn lookup(&self, name: ModuleName) -> LookupResult {
        match self.sources.get(&name) {
            Some(paths) => LookupResult::OwningSource(paths.first().clone()),
            None => match self.dependencies.get(&name) {
                Some(paths) => LookupResult::ExternalSource(paths.first().clone()),
                None => LookupResult::NoSource,
            },
        }
    }
}

impl Loader for BuckSourceDatabase {
    fn load(&self, name: ModuleName) -> (LoadResult, ErrorStyle) {
        match self.lookup(name) {
            LookupResult::OwningSource(path) => (LoadResult::from_path(path), ErrorStyle::Delayed),
            LookupResult::ExternalSource(path) => (LoadResult::from_path(path), ErrorStyle::Never),
            LookupResult::NoSource => (
                LoadResult::FailedToFind(anyhow!("Not a dependency or typeshed")),
                ErrorStyle::Never,
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_read_manifest() {
        assert_eq!(
            read_manifest_file_data(r#"[["foo/bar.py", "root/foo/bar.py", "derp"]]"#.as_bytes())
                .unwrap(),
            vec![ManifestItem {
                module_name: ModuleName::from_str("foo.bar"),
                relative_path: PathBuf::from_str("root/foo/bar.py").unwrap()
            }]
        );
        assert_eq!(
            read_manifest_file_data(
                r#"[["foo/bar.derp", "root/foo/bar.derp", "derp"]]"#.as_bytes()
            )
            .unwrap(),
            vec![]
        )
    }

    #[test]
    fn test_load_simple() {
        let foo_path = PathBuf::from_str("/root/foo.py").unwrap();
        let bar_path = PathBuf::from_str("/root/bar.py").unwrap();
        let baz_path = PathBuf::from_str("/root/baz.py").unwrap();
        let source_db = BuckSourceDatabase::from_manifest_items(
            vec![ManifestItem {
                module_name: ModuleName::from_str("foo"),
                relative_path: foo_path.clone(),
            }],
            vec![ManifestItem {
                module_name: ModuleName::from_str("bar"),
                relative_path: bar_path.clone(),
            }],
            vec![ManifestItem {
                module_name: ModuleName::from_str("baz"),
                relative_path: baz_path.clone(),
            }],
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("foo")),
            LookupResult::OwningSource(foo_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("bar")),
            LookupResult::ExternalSource(bar_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("baz")),
            LookupResult::ExternalSource(baz_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("qux")),
            LookupResult::NoSource
        );
    }

    #[test]
    fn test_load_source_over_dependencies() {
        let src_foo_path = PathBuf::from_str("/src/foo.py").unwrap();
        let dep_foo_path = PathBuf::from_str("/dep/foo.py").unwrap();

        let src_bar_path = PathBuf::from_str("/src/bar.py").unwrap();
        let dep_bar_path = PathBuf::from_str("/dep/bar.pyi").unwrap();

        let source_db = BuckSourceDatabase::from_manifest_items(
            vec![
                ManifestItem {
                    module_name: ModuleName::from_str("foo"),
                    relative_path: src_foo_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("bar"),
                    relative_path: src_bar_path.clone(),
                },
            ],
            vec![
                ManifestItem {
                    module_name: ModuleName::from_str("foo"),
                    relative_path: dep_foo_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("bar"),
                    relative_path: dep_bar_path.clone(),
                },
            ],
            vec![],
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("foo")),
            LookupResult::OwningSource(src_foo_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("bar")),
            LookupResult::OwningSource(src_bar_path)
        );
    }

    #[test]
    fn test_load_pyi_over_py() {
        let foo_py_path = PathBuf::from_str("/root/foo.py").unwrap();
        let foo_pyi_path = PathBuf::from_str("/root/foo.pyi").unwrap();
        let bar_py_path = PathBuf::from_str("/root/bar.py").unwrap();
        let bar_pyi_path = PathBuf::from_str("/root/bar.pyi").unwrap();

        let source_db = BuckSourceDatabase::from_manifest_items(
            vec![
                ManifestItem {
                    module_name: ModuleName::from_str("foo"),
                    relative_path: foo_py_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("foo"),
                    relative_path: foo_pyi_path.clone(),
                },
            ],
            vec![
                ManifestItem {
                    module_name: ModuleName::from_str("bar"),
                    relative_path: bar_py_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("bar"),
                    relative_path: bar_pyi_path.clone(),
                },
            ],
            vec![],
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("foo")),
            LookupResult::OwningSource(foo_pyi_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("bar")),
            LookupResult::ExternalSource(bar_pyi_path)
        );
    }

    #[test]
    fn test_load_dependency_typeshed_conflict() {
        let dep_a_path = PathBuf::from_str("/dep/a.py").unwrap();
        let dep_b_path = PathBuf::from_str("/dep/b.pyi").unwrap();
        let dep_c_path = PathBuf::from_str("/dep/c.py").unwrap();
        let dep_d_path = PathBuf::from_str("/dep/d.pyi").unwrap();

        let typeshed_a_path = PathBuf::from_str("/typeshed/a.py").unwrap();
        let typeshed_b_path = PathBuf::from_str("/typeshed/b.py").unwrap();
        let typeshed_c_path = PathBuf::from_str("/typeshed/c.pyi").unwrap();
        let typeshed_d_path = PathBuf::from_str("/typeshed/d.pyi").unwrap();

        let source_db = BuckSourceDatabase::from_manifest_items(
            vec![],
            vec![
                ManifestItem {
                    module_name: ModuleName::from_str("a"),
                    relative_path: dep_a_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("b"),
                    relative_path: dep_b_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("c"),
                    relative_path: dep_c_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("d"),
                    relative_path: dep_d_path.clone(),
                },
            ],
            vec![
                ManifestItem {
                    module_name: ModuleName::from_str("a"),
                    relative_path: typeshed_a_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("b"),
                    relative_path: typeshed_b_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("c"),
                    relative_path: typeshed_c_path.clone(),
                },
                ManifestItem {
                    module_name: ModuleName::from_str("d"),
                    relative_path: typeshed_d_path.clone(),
                },
            ],
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("a")),
            LookupResult::ExternalSource(dep_a_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("b")),
            LookupResult::ExternalSource(dep_b_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("c")),
            LookupResult::ExternalSource(typeshed_c_path)
        );
        assert_eq!(
            source_db.lookup(ModuleName::from_str("d")),
            LookupResult::ExternalSource(dep_d_path)
        );
    }
}
