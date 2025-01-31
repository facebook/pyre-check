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
struct ModulePathInner {
    path: PathBuf,
}

impl ModulePath {
    pub fn filesystem(path: PathBuf) -> Self {
        Self(Arc::new(ModulePathInner { path }))
    }

    pub fn display(&self) -> String {
        self.0.path.to_string_lossy().into_owned()
    }

    pub fn is_init(&self) -> bool {
        self.0.path.file_stem() == Some(OsStr::new(dunder::INIT.as_str()))
    }

    /// Whether things imported by this module are reexported.
    pub fn style(&self) -> ModuleStyle {
        if self.0.path.extension() == Some(OsStr::new("pyi")) {
            ModuleStyle::Interface
        } else {
            ModuleStyle::Executable
        }
    }

    pub fn is_interface(&self) -> bool {
        self.style() == ModuleStyle::Interface
    }

    pub fn as_filesystem_path(&self) -> Option<&Path> {
        Some(&self.0.path)
    }
}
