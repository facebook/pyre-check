/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::metadata::RuntimeMetadata;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::loader::LoaderId;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct Handle {
    module: ModuleName,
    path: ModulePath,
    config: RuntimeMetadata,
    loader: LoaderId,
}

impl Handle {
    pub fn new(
        module: ModuleName,
        path: ModulePath,
        config: RuntimeMetadata,
        loader: LoaderId,
    ) -> Self {
        Self {
            module,
            path,
            config,
            loader,
        }
    }

    pub fn module(&self) -> ModuleName {
        self.module
    }

    pub fn path(&self) -> &ModulePath {
        &self.path
    }

    pub fn config(&self) -> &RuntimeMetadata {
        &self.config
    }

    pub fn loader(&self) -> &LoaderId {
        &self.loader
    }
}
