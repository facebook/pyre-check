/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use serde::Deserialize;

use crate::metadata::PythonVersion;
use crate::metadata::RuntimeMetadata;
use crate::metadata::DEFAULT_PYTHON_PLATFORM;

pub fn set_if_some<T: Clone>(config_field: &mut T, value: Option<&T>) {
    if let Some(value) = value {
        *config_field = value.clone();
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Clone)]
pub struct ConfigFile {
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
    pub site_package_path: Vec<PathBuf>,
}

impl Default for ConfigFile {
    fn default() -> ConfigFile {
        ConfigFile {
            search_roots: Self::default_search_roots(),
            python_platform: Self::default_python_platform(),
            python_version: PythonVersion::default(),
            site_package_path: Vec::new(),
        }
    }
}

impl ConfigFile {
    pub fn default_python_platform() -> String {
        DEFAULT_PYTHON_PLATFORM.to_owned()
    }

    pub fn default_search_roots() -> Vec<PathBuf> {
        vec![PathBuf::from(".")]
    }

    pub fn get_runtime_metadata(&self) -> RuntimeMetadata {
        RuntimeMetadata::new(self.python_version, self.python_platform.clone())
    }
}
