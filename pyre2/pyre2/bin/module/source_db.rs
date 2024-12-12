/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;

use anyhow::anyhow;
use anyhow::Context as _;
use starlark_map::small_map::SmallMap;
use tracing::warn;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::state::loader::LoadResult;
use crate::util::fs_anyhow;

#[derive(Debug, PartialEq)]
struct ManifestItem {
    module_name: ModuleName,
    relative_path: PathBuf,
}

#[derive(Debug, PartialEq)]
pub struct BuckSourceDatabase {
    sources: SmallMap<ModuleName, PathBuf>,
    dependencies: SmallMap<ModuleName, PathBuf>,
    typeshed: SmallMap<ModuleName, PathBuf>,
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
                warn!("Cannot convert path to module name: {error:#}");
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

// If there are multiple manifest files, we read them and combine them into a single SmallMap (for
// more efficient lookup later). Conflicting module names are rare occurrence in practice, and often
// result in a failure. For now, conflicts are handled in the most naive way of "last occurrence wins".
fn read_manifest_files(
    manifest_paths: &[PathBuf],
) -> anyhow::Result<SmallMap<ModuleName, PathBuf>> {
    let mut result = SmallMap::new();
    for manifest_path in manifest_paths {
        let manifest_items = read_manifest_file(manifest_path.as_path())?;
        for manifest_item in manifest_items {
            result.insert(manifest_item.module_name, manifest_item.relative_path);
        }
    }
    Ok(result)
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
        Ok(BuckSourceDatabase {
            sources,
            dependencies,
            typeshed,
        })
    }

    pub fn modules_to_check(&self) -> Vec<ModuleName> {
        self.sources.keys().copied().collect()
    }

    fn lookup(&self, name: ModuleName) -> LookupResult {
        match self.sources.get(&name) {
            Some(path) => LookupResult::OwningSource(path.clone()),
            None => {
                match (self.dependencies.get(&name), self.typeshed.get(&name)) {
                    (None, None) => LookupResult::NoSource,
                    (Some(dependency), None) => LookupResult::ExternalSource(dependency.clone()),
                    (None, Some(typeshed)) => LookupResult::ExternalSource(typeshed.clone()),
                    (Some(dependency), Some(typeshed)) => {
                        // Always prefer dependency over typeshed, unless dependency is the source of
                        // a 3p library that already has a typeshed stub.
                        if let Some("py") = dependency.extension().and_then(OsStr::to_str) {
                            LookupResult::ExternalSource(typeshed.clone())
                        } else {
                            LookupResult::ExternalSource(dependency.clone())
                        }
                    }
                }
            }
        }
    }

    pub fn load(&self, name: ModuleName) -> (LoadResult, ErrorStyle) {
        match self.lookup(name) {
            LookupResult::OwningSource(path) => {
                (LoadResult::from_path(path), ErrorStyle::Immediate)
            }
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

    use starlark_map::smallmap;

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
        let source_db = BuckSourceDatabase {
            sources: smallmap! {
                ModuleName::from_str("foo") => foo_path.clone()
            },
            dependencies: smallmap! {
                ModuleName::from_str("bar") => bar_path.clone()
            },
            typeshed: smallmap! {
                ModuleName::from_str("baz") => baz_path.clone()
            },
        };
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
    fn test_load_dependency_typeshed_conflict() {
        let dep_a_path = PathBuf::from_str("/dep/a.py").unwrap();
        let dep_b_path = PathBuf::from_str("/dep/b.pyi").unwrap();
        let dep_c_path = PathBuf::from_str("/dep/c.py").unwrap();
        let dep_d_path = PathBuf::from_str("/dep/d.pyi").unwrap();

        let typeshed_a_path = PathBuf::from_str("/typeshed/a.py").unwrap();
        let typeshed_b_path = PathBuf::from_str("/typeshed/b.py").unwrap();
        let typeshed_c_path = PathBuf::from_str("/typeshed/c.pyi").unwrap();
        let typeshed_d_path = PathBuf::from_str("/typeshed/d.pyi").unwrap();

        let source_db = BuckSourceDatabase {
            sources: SmallMap::new(),
            dependencies: smallmap! {
                ModuleName::from_str("a") => dep_a_path.clone(),
                ModuleName::from_str("b") => dep_b_path.clone(),
                ModuleName::from_str("c") => dep_c_path.clone(),
                ModuleName::from_str("d") => dep_d_path.clone(),
            },
            typeshed: smallmap! {
                ModuleName::from_str("a") => typeshed_a_path.clone(),
                ModuleName::from_str("b") => typeshed_b_path.clone(),
                ModuleName::from_str("c") => typeshed_c_path.clone(),
                ModuleName::from_str("d") => typeshed_d_path.clone(),
            },
        };
        assert_eq!(
            source_db.lookup(ModuleName::from_str("a")),
            LookupResult::ExternalSource(typeshed_a_path)
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
