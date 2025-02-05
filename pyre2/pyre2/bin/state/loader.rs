/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;
use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::arc_id::ArcId;

/// A function that loads a module, given the `ModuleName`.
pub trait Loader: Sync + Debug {
    /// Return `Err` to indicate the module could not be found.
    fn find(&self, name: ModuleName) -> anyhow::Result<(ModulePath, ErrorStyle)>;

    /// Load a file from memory, if you can find it. Only called if `find` returns
    /// a `ModulePath::memory`.
    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        let _path = path;
        None
    }
}

#[derive(Clone, Dupe, Debug, Hash, PartialEq, Eq)]
pub struct LoaderId(ArcId<dyn Loader + Send>);

impl Loader for LoaderId {
    fn find(&self, name: ModuleName) -> anyhow::Result<(ModulePath, ErrorStyle)> {
        self.0.find(name)
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        self.0.load_from_memory(path)
    }
}

impl LoaderId {
    pub fn new(loader: impl Loader + Send + 'static) -> Self {
        Self(ArcId::from_arc(Arc::new(loader)))
    }
}
