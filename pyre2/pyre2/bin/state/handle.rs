/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::module::module_name::ModuleName;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct Handle {
    module: ModuleName,
}

impl Handle {
    pub fn new(module: ModuleName) -> Self {
        Self { module }
    }

    pub fn module(&self) -> ModuleName {
        self.module
    }
}
