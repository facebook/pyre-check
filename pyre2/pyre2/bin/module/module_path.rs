/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::fmt;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;

use crate::dunder;
use crate::module::module_name::ModuleName;

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, Hash)]
pub enum ModuleStyle {
    /// .py - executable code.
    Executable,
    /// .pyi - just types that form an interface.
    Interface,
}

/// Store information about where a module is sourced from.
#[derive(Debug, Clone, Dupe, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct ModulePath(Arc<ModulePathDetails>);

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum ModulePathDetails {
    /// The module source comes from a file on disk.
    FileSystem(PathBuf),
    /// The module source comes from typeshed bundled with Pyre (which gets stored in-memory).
    /// The path is relative to the root of the typeshed directory.
    BundledTypeshed(PathBuf),
    /// Representing the possibility that a module source doesn't exist.
    /// The module name indicates the module that is looked up but not found.
    NotFound(ModuleName),
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

impl Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0 {
            ModulePathDetails::FileSystem(path) => write!(f, "{}", path.display()),
            ModulePathDetails::BundledTypeshed(relative_path) => {
                write!(
                    f,
                    "bundled /pyre2/third_party/typeshed/{}",
                    relative_path.display()
                )
            }
            ModulePathDetails::NotFound(module_name) => {
                // This branch is not expected to be reachable since nonexistent module shouldn't have an errors,
                // but lets make it clear this is a fake path if it ever happens to leak into any output.
                write!(f, "empty {module_name}")
            }
        }
    }
}

impl ModulePath {
    pub fn filesystem(path: PathBuf) -> Self {
        Self(Arc::new(ModulePathDetails::FileSystem(path)))
    }

    pub fn bundled_typeshed(relative_path: PathBuf) -> Self {
        Self(Arc::new(ModulePathDetails::BundledTypeshed(relative_path)))
    }

    pub fn not_found(module_name: ModuleName) -> Self {
        Self(Arc::new(ModulePathDetails::NotFound(module_name)))
    }

    pub fn is_init(&self) -> bool {
        match &*self.0 {
            ModulePathDetails::FileSystem(path) => is_path_init(path),
            ModulePathDetails::BundledTypeshed(relative_path) => is_path_init(relative_path),
            ModulePathDetails::NotFound(_) => false,
        }
    }

    /// Whether things imported by this module are reexported.
    pub fn style(&self) -> ModuleStyle {
        match &*self.0 {
            ModulePathDetails::FileSystem(path) => ModuleStyle::of_path(path),
            ModulePathDetails::BundledTypeshed(relative_path) => {
                ModuleStyle::of_path(relative_path)
            }
            ModulePathDetails::NotFound(_) => ModuleStyle::Executable,
        }
    }

    pub fn is_interface(&self) -> bool {
        self.style() == ModuleStyle::Interface
    }

    pub fn as_filesystem_path(&self) -> Option<&Path> {
        match &*self.0 {
            ModulePathDetails::FileSystem(path) => Some(path),
            ModulePathDetails::BundledTypeshed(_) => None,
            ModulePathDetails::NotFound(_) => None,
        }
    }
}
