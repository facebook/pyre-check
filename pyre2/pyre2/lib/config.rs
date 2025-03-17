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

use serde::Deserialize;

use crate::metadata::PythonVersion;
use crate::metadata::RuntimeMetadata;
use crate::metadata::DEFAULT_PYTHON_PLATFORM;
use crate::Globs;

static PYPROJECT_FILE_NAME: &str = "pyproject.toml";

pub fn set_if_some<T: Clone>(config_field: &mut T, value: Option<&T>) {
    if let Some(value) = value {
        *config_field = value.clone();
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Clone)]
pub struct ConfigFile {
    /// Files that should be counted as sources (e.g. user-space code).
    #[serde(default = "ConfigFile::default_project_includes")]
    pub project_includes: Globs,

    /// corresponds to --include (soon to be renamed to --search-path) in Args
    // TODO(connernilsen): set this to config directory when config is found
    #[serde(default = "ConfigFile::default_search_roots")]
    pub search_roots: Vec<PathBuf>,

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
}

impl Default for ConfigFile {
    fn default() -> ConfigFile {
        ConfigFile {
            search_roots: Self::default_search_roots(),
            python_platform: Self::default_python_platform(),
            python_version: PythonVersion::default(),
            site_package_path: Vec::new(),
            project_includes: Self::default_project_includes(),
        }
    }
}

impl ConfigFile {
    pub fn default_project_includes() -> Globs {
        Globs::new(vec!["".to_owned()])
    }

    pub fn default_python_platform() -> String {
        DEFAULT_PYTHON_PLATFORM.to_owned()
    }

    pub fn default_search_roots() -> Vec<PathBuf> {
        vec![PathBuf::from("")]
    }

    pub fn get_runtime_metadata(&self) -> RuntimeMetadata {
        RuntimeMetadata::new(self.python_version, self.python_platform.clone())
    }

    pub fn from_file(p: &Path) -> anyhow::Result<ConfigFile> {
        // TODO(connernilsen): fix return type and handle config searching
        let config_str = fs::read_to_string(p)?;
        if p.file_name() == Some(OsStr::new(&PYPROJECT_FILE_NAME)) {
            Self::parse_pyproject_toml(&config_str)
        } else {
            Self::parse_config(&config_str)
        }
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
            pub pyre: Option<ConfigFile>,
        }

        let maybe_config = toml::from_str::<PyProject>(config_str)
            .map_err(|err| anyhow::Error::msg(err.to_string()))?
            .tool
            .and_then(|c| c.pyre);
        // TODO(connernilsen): we don't want to return a default config here, we should keep searching
        Ok(maybe_config.unwrap_or_else(ConfigFile::default))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialize_pyre_config() {
        let config_str = "
            project_includes = [\"./tests\", \"./implementation\"]
            python_platform = \"darwin\"
            python_version = \"1.2.3\"
        ";
        let config = ConfigFile::parse_config(config_str).unwrap();
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
            },
        );
    }

    #[test]
    fn deserialize_pyre_config_defaults() {
        let config_str = "";
        let config = ConfigFile::parse_config(config_str).unwrap();
        assert_eq!(config, ConfigFile::default());
    }

    #[test]
    fn deserialize_pyre_config_with_unknown() {
        let config_str = "
            unknown = \"value\"
            python_platform = \"windows\"
        ";
        let config = ConfigFile::parse_config(config_str).unwrap();
        assert_eq!(
            config,
            ConfigFile {
                python_platform: "windows".to_owned(),
                ..ConfigFile::default()
            }
        )
    }

    #[test]
    fn deserialize_pyproject_toml() {
        let config_str = "
            [tool.pyre]
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
            [tool.pyre]
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
    fn deserialize_pyproject_toml_without_pyre() {
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
}
