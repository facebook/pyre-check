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

    pub fn load(&self, name: ModuleName) -> (LoadResult, ErrorStyle) {
        let path = self.sources.get(&name).or_else(|| {
            match (self.dependencies.get(&name), self.typeshed.get(&name)) {
                (None, None) => None,
                (dependency, None) => dependency,
                (None, typeshed) => typeshed,
                (Some(dependency), Some(typeshed)) => {
                    // Always prefer dependency over typeshed, unless dependency is the source of
                    // a 3p library that already has a typeshed stub.
                    if let Some("py") = dependency.extension().and_then(OsStr::to_str) {
                        Some(typeshed)
                    } else {
                        Some(dependency)
                    }
                }
            }
        });
        (
            LoadResult::from_path_result(
                path.cloned()
                    .ok_or_else(|| anyhow!("Not a dependency or typeshed")),
            ),
            if self.sources.contains_key(&name) {
                ErrorStyle::Immediate
            } else {
                ErrorStyle::Never
            },
        )
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
}
