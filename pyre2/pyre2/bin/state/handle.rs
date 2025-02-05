/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::config::Config;
use crate::module::module_name::ModuleName;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct Handle {
    module: ModuleName,
    config: Config,
}

impl Handle {
    pub fn new(module: ModuleName, config: Config) -> Self {
        Self { module, config }
    }

    pub fn module(&self) -> ModuleName {
        self.module
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
}
