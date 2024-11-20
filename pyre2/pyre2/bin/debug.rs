/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Deserialize;
use serde::Serialize;
use starlark_map::small_map::SmallMap;

use crate::module::module_name::ModuleName;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Info {
    pub modules: SmallMap<ModuleName, Module>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub bindings: Vec<Binding>,
    pub errors: Vec<Error>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Binding {
    pub kind: String,
    pub key: String,
    pub location: String,
    pub binding: String,
    pub result: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Error {
    pub location: String,
    pub message: String,
}
