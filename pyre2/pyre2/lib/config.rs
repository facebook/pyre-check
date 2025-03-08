/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;
use serde::Deserialize;

use crate::metadata::PythonVersion;
use crate::metadata::RuntimeMetadata;
use crate::metadata::DEFAULT_PYTHON_PLATFORM;

#[derive(Clone, Dupe, Debug, PartialEq, Eq, Hash)]
pub struct ConfigFile(Arc<ConfigFileInner>);

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Clone)]
pub struct ConfigFileInner {
    /// corresponds to --include (soon to be renamed to --search-path) in Args
    // TODO(connernilsen): set this to config directory when config is found
    #[serde(default = "ConfigFile::default_search_roots")]
    pub search_roots: Vec<PathBuf>,

    /// The default Python platform to use, likely `linux`
    // TODO(connernilsen): use python_executable if not set
    #[serde(default = "ConfigFile::default_python_platform")]
    pub python_platform: String,

    /// The default Python version to use, likely `3.12.0`
    // TODO(connernilsen): use python_executable if not set
    #[serde(default)]
    pub python_version: PythonVersion,

    // TODO(connernilsen): use python_executable if not set
    #[serde(default)]
    pub site_package_path: Option<Vec<PathBuf>>,
}

impl Default for ConfigFile {
    fn default() -> ConfigFile {
        ConfigFileInner {
            search_roots: Self::default_search_roots(),
            python_platform: Self::default_python_platform(),
            python_version: PythonVersion::default(),
            site_package_path: None,
        }
        .into()
    }
}

impl Deref for ConfigFile {
    type Target = ConfigFileInner;

    fn deref(&self) -> &Self::Target {
        &self.0
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

    pub fn get_runtime_metadata(&self) -> RuntimeMetadata {
        RuntimeMetadata::new(self.python_version, self.python_platform.clone())
    }
}
