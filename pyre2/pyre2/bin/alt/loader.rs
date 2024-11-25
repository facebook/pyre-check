/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use crate::util::fs_anyhow;

/// The result of trying to load a file.
pub enum LoadResult {
    Loaded(PathBuf, String),
    FailedToLoad(PathBuf, anyhow::Error),
    FailedToFind(anyhow::Error),
}

pub static FAKE_MODULE: &str = r#"
from typing import Any
def __getattr__(name: str) -> Any: ...
"#;

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
}
