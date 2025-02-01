/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;

use crate::dunder;

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, Hash)]
pub enum ModuleStyle {
    /// .py - executable code.
    Executable,
    /// .pyi - just types that form an interface.
    Interface,
}

/// Store information about where a module is sourced from.
#[derive(Debug, Clone, Dupe, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct ModulePath(Arc<ModulePathInner>);

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum ModulePathInner {
    /// The module source comes from a file on disk.
    FileSystem(PathBuf),
    /// The module source comes from typeshed bundled with Pyre (which gets stored in-memory).
    /// The path is relative to the root of the typeshed directory.
    BundledTypeshed(PathBuf),
}

fn is_path_init(path: &Path) -> bool {
    path.file_stem() == Some(OsStr::new(dunder::INIT.as_str()))
}

impl ModuleStyle {
    fn of_path(path: &Path) -> Self {
        if path.extension() == Some(OsStr::new("pyi")) {
            ModuleStyle::Interface
        } else {
            ModuleStyle::Executable
        }
    }
}

impl ModulePath {
    pub fn filesystem(path: PathBuf) -> Self {
        Self(Arc::new(ModulePathInner::FileSystem(path)))
    }

    pub fn bundled_typeshed(relative_path: PathBuf) -> Self {
        Self(Arc::new(ModulePathInner::BundledTypeshed(relative_path)))
    }

    pub fn display(&self) -> String {
        match &*self.0 {
            ModulePathInner::FileSystem(path) => path.display().to_string(),
            ModulePathInner::BundledTypeshed(relative_path) => {
                format!(
                    "bundled /pyre2/third_party/typeshed/{}",
                    relative_path.display()
                )
            }
        }
    }

    pub fn is_init(&self) -> bool {
        match &*self.0 {
            ModulePathInner::FileSystem(path) => is_path_init(path),
            ModulePathInner::BundledTypeshed(relative_path) => is_path_init(relative_path),
        }
    }

    /// Whether things imported by this module are reexported.
    pub fn style(&self) -> ModuleStyle {
        match &*self.0 {
            ModulePathInner::FileSystem(path) => ModuleStyle::of_path(path),
            ModulePathInner::BundledTypeshed(relative_path) => ModuleStyle::of_path(relative_path),
        }
    }

    pub fn is_interface(&self) -> bool {
        self.style() == ModuleStyle::Interface
    }

    pub fn as_filesystem_path(&self) -> Option<&Path> {
        match &*self.0 {
            ModulePathInner::FileSystem(path) => Some(path),
            ModulePathInner::BundledTypeshed(_) => None,
        }
    }
}
