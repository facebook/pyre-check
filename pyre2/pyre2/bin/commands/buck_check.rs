/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use anyhow::anyhow;
use anyhow::Context as _;
use clap::Parser;
use serde::Deserialize;
use starlark_map::small_map::SmallMap;
use tracing::info;
use tracing::warn;

use crate::commands::common::CommonArgs;
use crate::config::Config;
use crate::config::PythonVersion;
use crate::error::error::Error;
use crate::error::legacy::LegacyErrors;
use crate::module::module_name::ModuleName;
use crate::state::driver::Driver;
use crate::state::loader::LoadResult;
use crate::util::fs_anyhow;

#[derive(Debug, Parser)]
pub struct Args {
    /// Path to input JSON file
    input_path: PathBuf,

    /// Path to output JSON file
    #[arg(long = "output", short = 'o')]
    output_path: Option<PathBuf>,

    #[clap(flatten)]
    common: CommonArgs,
}

#[derive(Debug, Deserialize, PartialEq)]
struct InputFile {
    dependencies: Vec<PathBuf>,
    py_version: String,
    sources: Vec<PathBuf>,
    typeshed: Option<PathBuf>,
}

#[derive(Debug, PartialEq)]
struct ManifestItem {
    module_name: ModuleName,
    relative_path: PathBuf,
}

#[derive(Debug, PartialEq)]
struct BuckSourceDatabase {
    sources: SmallMap<ModuleName, PathBuf>,
    dependencies: SmallMap<ModuleName, PathBuf>,
    typeshed: SmallMap<ModuleName, PathBuf>,
}

impl BuckSourceDatabase {
    fn from_input_file(input_file: &InputFile) -> anyhow::Result<Self> {
        let sources = read_manifest_files(input_file.sources.as_slice())?;
        let dependencies = read_manifest_files(input_file.dependencies.as_slice())?;
        let typeshed = read_manifest_files(input_file.typeshed.as_slice())?;
        Ok(BuckSourceDatabase {
            sources,
            dependencies,
            typeshed,
        })
    }
}

fn read_input_file(path: &Path) -> anyhow::Result<InputFile> {
    let data = fs_anyhow::read(path)?;
    let input_file: InputFile = serde_json::from_slice(&data)
        .with_context(|| format!("failed to parse input JSON `{}`", path.display()))?;
    Ok(input_file)
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

fn compute_errors(config: Config, sourcedb: BuckSourceDatabase, common: &CommonArgs) -> Vec<Error> {
    let modules_to_check: Vec<_> = sourcedb.sources.keys().copied().collect();
    let lookup_module = |name| {
        let path = sourcedb.sources.get(&name).or_else(|| {
            match (
                sourcedb.dependencies.get(&name),
                sourcedb.typeshed.get(&name),
            ) {
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
            sourcedb.sources.contains_key(&name),
        )
    };
    Driver::new(
        &modules_to_check,
        &config,
        common.timings,
        common.parallel(),
        &lookup_module,
    )
    .errors_in_checked_modules()
}

fn write_output_to_file(path: &Path, legacy_errors: &LegacyErrors) -> anyhow::Result<()> {
    let output_bytes = serde_json::to_vec(legacy_errors)
        .with_context(|| "failed to serialize JSON value to bytes")?;
    fs_anyhow::write(path, &output_bytes)
}

fn write_output_to_stdout(legacy_errors: &LegacyErrors) -> anyhow::Result<()> {
    let content = serde_json::to_string_pretty(legacy_errors)?;
    println!("{}", content);
    Ok(())
}

fn write_output(errors: &[Error], path: Option<&Path>) -> anyhow::Result<()> {
    let legacy_errors = LegacyErrors::from_errors(errors);
    if let Some(path) = path {
        write_output_to_file(path, &legacy_errors)
    } else {
        write_output_to_stdout(&legacy_errors)
    }
}

impl Args {
    pub fn run(self) -> anyhow::Result<()> {
        let input_file = read_input_file(self.input_path.as_path())?;
        let python_version = PythonVersion::from_str(&input_file.py_version)?;
        let config = Config::new(python_version, "linux".to_owned());
        let sourcedb = BuckSourceDatabase::from_input_file(&input_file)?;
        let type_errors = compute_errors(config, sourcedb, &self.common);
        info!("Found {} type errors", type_errors.len());
        write_output(&type_errors, self.output_path.as_deref())?;
        Ok(())
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
