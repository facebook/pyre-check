/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

use dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::arc_id::ArcId;

#[derive(Debug, Clone, Dupe)]
pub struct FindError(pub Arc<anyhow::Error>);

impl Display for FindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#}", self.0)
    }
}

/// A function that loads a module, given the `ModuleName`.
pub trait Loader: Sync + Debug {
    /// Return `Err` to indicate the module could not be found.
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError>;

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
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        self.0.find(module)
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

#[derive(Debug)]
pub struct LoaderFindCache<T> {
    loader: T,
    cache: Mutex<SmallMap<ModuleName, Result<(ModulePath, ErrorStyle), FindError>>>,
}

impl<T: Loader> Loader for LoaderFindCache<T> {
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        {
            if let Some(result) = self.cache.lock().unwrap().get(&module) {
                return result.dupe();
            }
        }
        let res = self.loader.find(module.dupe());
        self.cache.lock().unwrap().insert(module, res.dupe());
        res
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        self.loader.load_from_memory(path)
    }
}

impl<T> LoaderFindCache<T> {
    pub fn new(loader: T) -> Self {
        Self {
            loader,
            cache: Mutex::new(SmallMap::new()),
        }
    }
}
