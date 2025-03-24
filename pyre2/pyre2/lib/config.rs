/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use itertools::Itertools;
use serde::Deserialize;
use starlark_map::small_map::SmallMap;
use toml::Table;

use crate::error::kind::ErrorKind;
use crate::globs::Globs;
use crate::metadata::PythonVersion;
use crate::metadata::RuntimeMetadata;
use crate::metadata::DEFAULT_PYTHON_PLATFORM;

static PYPROJECT_FILE_NAME: &str = "pyproject.toml";

pub fn set_if_some<T: Clone>(config_field: &mut T, value: Option<&T>) {
    if let Some(value) = value {
        *config_field = value.clone();
    }
}

#[derive(Debug, Deserialize, Clone)]
#[serde(transparent)]
pub struct ExtraConfigs(Table);

// `Value` types in `Table` might not be `Eq`, but we don't actually care about that w.r.t. `ConfigFile`
impl Eq for ExtraConfigs {}

impl PartialEq for ExtraConfigs {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize, Clone)]
pub struct ConfigFile {
    /// Files that should be counted as sources (e.g. user-space code).
    /// NOTE: this is never replaced with CLI args in this config, but may be overridden by CLI args where used.
    #[serde(default = "ConfigFile::default_project_includes")]
    pub project_includes: Globs,

    /// Files that should be excluded as sources (e.g. user-space code). These take
    /// precedence over `project_includes`.
    /// NOTE: this is never replaced with CLI args in this config, but may be overridden by CLI args where used.
    #[serde(default = "ConfigFile::default_project_excludes")]
    pub project_excludes: Globs,

    /// corresponds to --search-path in Args, the list of directories where imports are
    /// found (including type checked files).
    // TODO(connernilsen): set this to config directory when config is found
    #[serde(default = "ConfigFile::default_search_path")]
    pub search_path: Vec<PathBuf>,

    /// The default Python platform to use, likely `linux`
    // TODO(connernilsen): use python_executable if not set
    #[serde(default = "ConfigFile::default_python_platform")]
    pub python_platform: String,

    /// The default Python version to use, likely `3.13.0`
    // TODO(connernilsen): use python_executable if not set
    #[serde(default)]
    pub python_version: PythonVersion,

    // TODO(connernilsen): use python_executable if not set
    #[serde(default)]
    pub site_package_path: Vec<PathBuf>,

    #[serde(default)]
    pub errors: SmallMap<ErrorKind, bool>,

    /// Any unknown config items
    #[serde(default, flatten)]
    pub extras: ExtraConfigs,
}

impl Default for ConfigFile {
    fn default() -> ConfigFile {
        ConfigFile {
            search_path: Self::default_search_path(),
            python_platform: Self::default_python_platform(),
            python_version: PythonVersion::default(),
            site_package_path: Vec::new(),
            project_includes: Self::default_project_includes(),
            project_excludes: Self::default_project_excludes(),
            errors: SmallMap::default(),
            extras: Self::default_extras(),
        }
    }
}

impl ConfigFile {
    pub fn default_project_includes() -> Globs {
        Globs::new(vec!["".to_owned()])
    }

    pub fn default_project_excludes() -> Globs {
        Globs::new(vec!["**/__pycache__/**".to_owned(), "**/.*".to_owned()])
    }

    pub fn default_python_platform() -> String {
        DEFAULT_PYTHON_PLATFORM.to_owned()
    }

    pub fn default_search_path() -> Vec<PathBuf> {
        vec![PathBuf::from("")]
    }

    pub fn default_extras() -> ExtraConfigs {
        ExtraConfigs(Table::new())
    }

    pub fn get_runtime_metadata(&self) -> RuntimeMetadata {
        RuntimeMetadata::new(self.python_version, self.python_platform.clone())
    }

    fn rewrite_with_path_to_config(&mut self, config_root: &Path) {
        // TODO(connernilsen): store root as part of config to make it easier to rewrite later on
        self.project_includes = self.project_includes.clone().from_root(config_root);
        self.search_path.iter_mut().for_each(|search_root| {
            let mut base = config_root.to_path_buf();
            base.push(search_root.as_path());
            *search_root = base;
        });
        // push config to search path to make sure we can fall back to the config directory as an import path
        // if users forget to add it
        self.search_path.push(config_root.to_path_buf());
        self.site_package_path
            .iter_mut()
            .for_each(|site_package_path| {
                let mut base = config_root.to_path_buf();
                base.push(site_package_path.as_path());
                *site_package_path = base;
            });
        self.project_excludes = self.project_excludes.clone().from_root(config_root);
    }

    pub fn from_file(config_path: &Path, error_on_extras: bool) -> anyhow::Result<ConfigFile> {
        // TODO(connernilsen): fix return type and handle config searching
        let config_str = fs::read_to_string(config_path)?;
        let mut config = if config_path.file_name() == Some(OsStr::new(&PYPROJECT_FILE_NAME)) {
            Self::parse_pyproject_toml(&config_str)
        } else {
            Self::parse_config(&config_str)
        }?;

        if error_on_extras && !config.extras.0.is_empty() {
            let extra_keys = config.extras.0.keys().join(", ");
            bail!("Extra keys found in config: {extra_keys}");
        }

        if let Some(config_root) = config_path.parent() {
            config.rewrite_with_path_to_config(config_root);
        }

        Ok(config)
    }

    fn parse_config(config_str: &str) -> anyhow::Result<ConfigFile> {
        toml::from_str::<ConfigFile>(config_str).map_err(|err| anyhow::Error::msg(err.to_string()))
    }

    fn parse_pyproject_toml(config_str: &str) -> anyhow::Result<ConfigFile> {
        #[derive(Debug, Deserialize)]
        struct PyProject {
            #[serde(default)]
            pub tool: Option<Tool>,
        }

        #[derive(Debug, Deserialize)]
        struct Tool {
            #[serde(default)]
            pub pyrefly: Option<ConfigFile>,
        }

        let maybe_config = toml::from_str::<PyProject>(config_str)
            .map_err(|err| anyhow::Error::msg(err.to_string()))?
            .tool
            .and_then(|c| c.pyrefly);
        // TODO(connernilsen): we don't want to return a default config here, we should keep searching
        Ok(maybe_config.unwrap_or_else(ConfigFile::default))
    }
}

#[cfg(test)]
mod tests {
    use std::path;

    use toml::Value;

    use super::*;

    #[test]
    fn deserialize_pyrefly_config() {
        let config_str = "
            project_includes = [\"tests\", \"./implementation\"]
            project_excludes = [\"tests/untyped/**\"]
            search_path = [\"../..\"]
            python_platform = \"darwin\"
            python_version = \"1.2.3\"
            site_package_path = [\"venv/lib/python1.2.3/site-packages\"]
            [errors]
            assert_type = true
            bad_return = false
        ";
        let config = ConfigFile::parse_config(config_str).unwrap();
        assert_eq!(
            config,
            ConfigFile {
                project_includes: Globs::new(vec![
                    "tests".to_owned(),
                    "./implementation".to_owned()
                ]),
                project_excludes: Globs::new(vec!["tests/untyped/**".to_owned()]),
                search_path: vec![PathBuf::from("../..")],
                python_platform: "darwin".to_owned(),
                python_version: PythonVersion::new(1, 2, 3),
                site_package_path: vec![PathBuf::from("venv/lib/python1.2.3/site-packages")],
                extras: ConfigFile::default_extras(),
                errors: SmallMap::from_iter(
                    [(ErrorKind::AssertType, true), (ErrorKind::BadReturn, false)].into_iter()
                ),
            },
        );
    }

    #[test]
    fn deserialize_pyrefly_config_defaults() {
        let config_str = "";
        let config = ConfigFile::parse_config(config_str).unwrap();
        assert_eq!(config, ConfigFile::default());
    }

    #[test]
    fn deserialize_pyrefly_config_with_unknown() {
        let config_str = "
            laszewo = \"good kids\"
            python_platform = \"windows\"
        ";
        let config = ConfigFile::parse_config(config_str).unwrap();
        assert_eq!(
            config.extras.0,
            Table::from_iter([("laszewo".to_owned(), Value::String("good kids".to_owned()))])
        );
    }

    #[test]
    fn deserialize_pyproject_toml() {
        let config_str = "
            [tool.pyrefly]
            project_includes = [\"./tests\", \"./implementation\"]
            python_platform = \"darwin\"
            python_version = \"1.2.3\"
        ";
        let config = ConfigFile::parse_pyproject_toml(config_str).unwrap();
        assert_eq!(
            config,
            ConfigFile {
                project_includes: Globs::new(vec![
                    "./tests".to_owned(),
                    "./implementation".to_owned()
                ]),
                python_platform: "darwin".to_owned(),
                python_version: PythonVersion::new(1, 2, 3),
                ..ConfigFile::default()
            }
        );
    }

    #[test]
    fn deserialize_pyproject_toml_defaults() {
        let config_str = "";
        let config = ConfigFile::parse_pyproject_toml(config_str).unwrap();
        assert_eq!(config, ConfigFile::default());
    }

    #[test]
    fn deserialize_pyproject_toml_with_unknown() {
        let config_str = "
            top_level = 1
            [table1]
            table1_value = 2
            [tool.pysa]
            pysa_value = 2
            [tool.pyrefly]
            python_version = \"1.2.3\"
        ";
        let config = ConfigFile::parse_pyproject_toml(config_str).unwrap();
        assert_eq!(
            config,
            ConfigFile {
                python_version: PythonVersion::new(1, 2, 3),
                ..ConfigFile::default()
            }
        );
    }

    #[test]
    fn deserialize_pyproject_toml_without_pyrefly() {
        let config_str = "
            top_level = 1
            [table1]
            table1_value = 2
            [tool.pysa]
            pysa_value = 2
        ";
        let config = ConfigFile::parse_pyproject_toml(config_str).unwrap();
        assert_eq!(config, ConfigFile::default(),);
    }

    #[test]
    fn deserialize_pyproject_toml_with_unknown_in_pyrefly() {
        let config_str = "
            top_level = 1
            [table1]
            table1_value = 2
            [tool.pysa]
            pysa_value = 2
            [tool.pyrefly]
            python_version = \"1.2.3\"
            inzo = \"overthinker\"
        ";
        let config = ConfigFile::parse_pyproject_toml(config_str).unwrap();
        assert_eq!(
            config.extras.0,
            Table::from_iter([("inzo".to_owned(), Value::String("overthinker".to_owned()))])
        );
    }

    #[test]
    fn test_rewrite_with_path_to_config() {
        fn with_sep(s: &str) -> String {
            s.replace("/", path::MAIN_SEPARATOR_STR)
        }
        let mut config = ConfigFile {
            project_includes: Globs::new(vec!["path1/**".to_owned(), "path2/path3".to_owned()]),
            project_excludes: Globs::new(vec!["tests/untyped/**".to_owned()]),
            search_path: vec![PathBuf::from("../..")],
            site_package_path: vec![PathBuf::from("venv/lib/python1.2.3/site-packages")],
            python_platform: ConfigFile::default_python_platform(),
            python_version: PythonVersion::default(),
            errors: SmallMap::default(),
            extras: ConfigFile::default_extras(),
        };

        let path_str = with_sep("path/to/my/config");
        let test_path = PathBuf::from(path_str.clone());

        let project_includes_vec = vec![
            path_str.clone() + &with_sep("/path1/**"),
            path_str.clone() + &with_sep("/path2/path3"),
        ];
        let project_excludes_vec = vec![path_str.clone() + &with_sep("/tests/untyped/**")];
        let search_path = vec![test_path.join("../.."), test_path.clone()];
        let site_package_path = vec![test_path.join("venv/lib/python1.2.3/site-packages")];

        config.rewrite_with_path_to_config(&test_path);

        let expected_config = ConfigFile {
            project_includes: Globs::new(project_includes_vec),
            project_excludes: Globs::new(project_excludes_vec),
            search_path,
            site_package_path,
            ..ConfigFile::default()
        };
        assert_eq!(config, expected_config);
    }

    #[test]
    fn test_deserializing_unknown_error_errors() {
        let config_str = "
            [errors]
            subtronics = true
            zeds_dead = false
            GRiZ = true
        ";
        let err = ConfigFile::parse_config(config_str).unwrap_err();
        assert!(err.to_string().contains("unknown variant"));
    }
}
