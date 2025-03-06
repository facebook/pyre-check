/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use derive_builder::Builder;
use derive_getters::Getters;
use dupe::Dupe;
use serde::Deserialize;

use crate::metadata::PythonVersion;
use crate::metadata::DEFAULT_PYTHON_PLATFORM;

#[derive(Clone, Dupe, Debug, PartialEq, Eq, Hash)]
pub struct ConfigFile(Arc<ConfigFileInner>);

#[derive(Debug, PartialEq, Eq, Hash, Getters, Deserialize, Builder, Clone)]
#[builder(build_fn(private, name = "build_internal"), name = "ConfigFileBuilder")]
pub struct ConfigFileInner {
    /// corresponds to --include (soon to be renamed to --search-path) in Args
    // TODO(connernilsen): set this to config directory when config is found
    #[serde(default = "ConfigFile::default_search_roots")]
    #[builder(default = "ConfigFile::default_search_roots()")]
    search_roots: Vec<PathBuf>,

    /// The default Python platform to use, likely `linux`
    // TODO(connernilsen): use python_executable if not set
    #[serde(default = "ConfigFile::default_python_platform")]
    #[builder(default = "ConfigFile::default_python_platform()")]
    python_platform: String,

    /// The default Python version to use, likely `3.12.0`
    // TODO(connernilsen): use python_executable if not set
    #[serde(default)]
    #[builder(default, setter(custom))]
    python_version: PythonVersion,

    // TODO(connernilsen): use python_executable if not set
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    site_package_path: Option<Vec<PathBuf>>,
}

impl Default for ConfigFile {
    fn default() -> Self {
        ConfigFileBuilder::default()
            .build_internal()
            .expect("Default ConfigFile fields are invalid.")
            .into()
    }
}

impl Deref for ConfigFile {
    type Target = ConfigFileInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<ConfigFile> for ConfigFileBuilder {
    fn from(config: ConfigFile) -> Self {
        let mut builder = ConfigFileBuilder::create_empty();
        let inner = config.0;
        builder
            .search_roots(inner.search_roots.clone())
            .python_platform(inner.python_platform.clone());
        builder.python_version = Some(inner.python_version);
        if let Some(path) = &inner.site_package_path {
            builder.site_package_path(path.clone());
        }
        builder
    }
}

impl From<ConfigFileInner> for ConfigFile {
    fn from(inner: ConfigFileInner) -> Self {
        ConfigFile(Arc::new(inner))
    }
}

impl ConfigFile {
    pub fn default_python_platform() -> String {
        DEFAULT_PYTHON_PLATFORM.to_owned()
    }

    pub fn default_python_version() -> String {
        PythonVersion::default().to_string()
    }

    pub fn default_search_roots() -> Vec<PathBuf> {
        vec![PathBuf::from(".")]
    }
}

impl ConfigFileBuilder {
    #[allow(dead_code)] // Only used in tests so far
    pub fn build(&self) -> anyhow::Result<ConfigFile> {
        Ok(self.build_internal()?.into())
    }

    #[allow(dead_code)] // Only used in tests so far
    pub fn python_version(&mut self, value: String) -> &mut Self {
        match value.clone().try_into() {
            Ok(version) => {
                self.python_version = Some(version);
            }
            Err(error) => {
                let default = self.python_version.unwrap_or_default().to_string();
                eprintln!(
                    "Failed to parse `{value}` into Python version: {error} does not conform to. Falling back to
                    default: {default}."
                );
            }
        }
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_defaults() {
        let expected: ConfigFile = ConfigFileInner {
            search_roots: vec![PathBuf::from(".")],
            python_platform: DEFAULT_PYTHON_PLATFORM.to_owned(),
            python_version: PythonVersion::default(),
            site_package_path: None,
        }
        .into();
        assert_eq!(ConfigFileBuilder::default().build().unwrap(), expected);
    }

    #[test]
    fn test_from_config_file_into_builder() {
        let config: ConfigFile = ConfigFileInner {
            // NOTE! all of these fields should be different from the default!
            // this will help to ensure we aren't forgetting to add a field to
            // the Builder's From function.
            search_roots: vec![PathBuf::from("pyre2!")],
            python_platform: "my_computer".to_owned(),
            python_version: "1.2.3".to_owned().try_into().unwrap(),
            site_package_path: Some(vec![PathBuf::from(
                "relative/path/to/venv/lib/site-package",
            )]),
        }
        .into();
        let builder: ConfigFileBuilder = config.clone().into();
        assert_eq!(
            builder.build().unwrap(),
            config,
            "Look carefully at the fields. You most likely forgot to update the From impl for ConfigFileBuilder."
        );
    }
}
