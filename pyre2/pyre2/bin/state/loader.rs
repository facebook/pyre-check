/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;

/// A function that loads a module, given the `ModuleName`.
pub trait Loader: Sync {
    /// Return `Err` to indicate the module could not be found.
    /// The remaining components are the `ModulePath` where the module was found,
    /// optionally the file contents, and the `ErrorStyle` to use when reporting errors.
    /// If the contents are `None`, the file will be read from disk.
    /// It is an error to return a non-disk path and `None` for the contents.
    fn load(
        &self,
        name: ModuleName,
    ) -> anyhow::Result<(ModulePath, Option<Arc<String>>, ErrorStyle)>;
}
