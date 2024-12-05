/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::util::fs_anyhow;

/// A function that loads a module, given the `ModuleName`.
/// Returns a `LoadResult` and a boolean indicating whether to report errors from the module or not.
pub type Loader<'a> = dyn Fn(ModuleName) -> (LoadResult, ErrorStyle) + Sync + 'a;

/// The result of trying to load a file.
pub enum LoadResult {
    Loaded(PathBuf, String),
    FailedToLoad(PathBuf, anyhow::Error),
    FailedToFind(anyhow::Error),
}

pub struct LoadResultComponents {
    pub path: PathBuf,
    pub code: String,
    /// Found the file but failed to open it, raise an error in this file.
    pub self_error: Option<anyhow::Error>,
    /// File not found, raise an error in everyone who imports me.
    pub import_error: Option<anyhow::Error>,
}

static FAKE_MODULE: &str = r#"
from typing import Any
def __getattr__(name: str) -> Any: ...
__all__ = []
"#;

fn fake_path(module_name: ModuleName) -> PathBuf {
    // The generated fake module shouldn't have an errors, but lets make it clear
    // this is a fake path if it ever happens to leak into any output.
    PathBuf::from(format!("/fake/{module_name}.py"))
}

impl LoadResult {
    pub fn from_path_result(path: anyhow::Result<PathBuf>) -> Self {
        match path {
            Ok(path) => match fs_anyhow::read_to_string(&path) {
                Ok(code) => LoadResult::Loaded(path, code),
                Err(err) => LoadResult::FailedToLoad(path, err),
            },
            Err(err) => LoadResult::FailedToFind(err),
        }
    }

    pub fn components(self, module_name: ModuleName) -> LoadResultComponents {
        match self {
            LoadResult::Loaded(path, code) => LoadResultComponents {
                path,
                code,
                self_error: None,
                import_error: None,
            },
            LoadResult::FailedToLoad(path, err) => LoadResultComponents {
                path,
                code: FAKE_MODULE.to_owned(),
                self_error: Some(err),
                import_error: None,
            },
            LoadResult::FailedToFind(err) => LoadResultComponents {
                path: fake_path(module_name),
                code: FAKE_MODULE.to_owned(),
                self_error: None,
                import_error: Some(err),
            },
        }
    }
}
