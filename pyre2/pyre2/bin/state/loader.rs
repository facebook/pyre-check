/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::fs_anyhow;

/// A function that loads a module, given the `ModuleName`.
/// Returns a `LoadResult` and an enum indicating whether to report errors from the module or not.
pub trait Loader: Sync {
    fn load(&self, name: ModuleName) -> (LoadResult, ErrorStyle);
}

/// The result of trying to load a file.
pub enum LoadResult {
    Loaded(ModulePath, Arc<String>),
    FailedToLoad(ModulePath, anyhow::Error),
    FailedToFind(anyhow::Error),
}

pub struct LoadResultComponents {
    pub path: ModulePath,
    pub code: Arc<String>,
    /// Found the file but failed to open it, raise an error in this file.
    pub self_error: Option<anyhow::Error>,
    /// File not found, raise an error in everyone who imports me.
    pub import_error: Option<anyhow::Error>,
}

static FAKE_MODULE: &str = "";

impl LoadResult {
    pub fn from_path(path: PathBuf) -> Self {
        match fs_anyhow::read_to_string(&path) {
            Ok(code) => LoadResult::Loaded(ModulePath::filesystem(path), Arc::new(code)),
            Err(err) => LoadResult::FailedToLoad(ModulePath::filesystem(path), err),
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
                code: Arc::new(FAKE_MODULE.to_owned()),
                self_error: Some(err),
                import_error: None,
            },
            LoadResult::FailedToFind(err) => LoadResultComponents {
                path: ModulePath::not_found(module_name),
                code: Arc::new(FAKE_MODULE.to_owned()),
                self_error: None,
                import_error: Some(err),
            },
        }
    }
}
