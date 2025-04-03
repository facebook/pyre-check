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

use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::arc_id::ArcId;
use crate::util::display::commas_iter;
use crate::util::locked_map::LockedMap;

#[derive(Debug, Clone, Dupe)]
pub enum FindError {
    /// This module could not be found, and we should emit an error
    NotFound(Arc<anyhow::Error>),
    /// This import could not be found, but the user configured it to be ignored
    Ignored,
}

impl FindError {
    pub fn not_found(err: anyhow::Error) -> Self {
        Self::NotFound(Arc::new(err))
    }

    pub fn search_path(search_roots: &[PathBuf], site_package_path: &[PathBuf]) -> FindError {
        Self::not_found(anyhow!(
            "looked at search roots ({}) and site package path ({})",
            commas_iter(|| search_roots.iter().map(|x| x.display())),
            commas_iter(|| site_package_path.iter().map(|x| x.display())),
        ))
    }

    pub fn display(err: Arc<anyhow::Error>, module: ModuleName) -> String {
        format!("Could not find import of `{module}`, {:#}", err)
    }
}

/// A function that loads a module, given the `ModuleName`.
pub trait Loader: Sync + Debug {
    /// Return `Err` to indicate the module could not be found.
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError>;

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
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        self.0.find_import(module)
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
    cache: LockedMap<ModuleName, Result<ModulePath, FindError>>,
}

impl<T: Loader> Loader for LoaderFindCache<T> {
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        self.cache
            .ensure(&module, || self.loader.find_import(module))
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
