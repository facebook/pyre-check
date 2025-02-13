/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::anyhow;
use dupe::Dupe;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::arc_id::ArcId;
use crate::util::display::commas_iter;
use crate::util::locked_map::LockedMap;

#[derive(Debug, Clone, Dupe)]
pub struct FindError(Arc<anyhow::Error>);

impl FindError {
    pub fn new(err: anyhow::Error) -> Self {
        Self(Arc::new(err))
    }

    pub fn search_path(search_roots: &[PathBuf]) -> FindError {
        Self::new(anyhow!(
            "looked at search roots: {}",
            commas_iter(|| search_roots.iter().map(|x| x.display()))
        ))
    }

    pub fn display(&self, module: ModuleName) -> String {
        format!("Could not find import of `{module}`, {:#}", self.0)
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
    cache: LockedMap<ModuleName, Result<(ModulePath, ErrorStyle), FindError>>,
}

impl<T: Loader> Loader for LoaderFindCache<T> {
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        self.cache
            .ensure(&module, || self.loader.find(module))
            .dupe()
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        self.loader.load_from_memory(path)
    }
}

impl<T> LoaderFindCache<T> {
    pub fn new(loader: T) -> Self {
        Self {
            loader,
            cache: Default::default(),
        }
    }
}
