/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;

use crate::module::module_name::ModuleName;

/// These are names that are exported from the stdlib, but which take on
/// a more keyword-like quality. E.g. `x: TypeAlias = ...` meaningfully
/// changes the sense of the binding.
#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq)]
pub enum SpecialExport {
    TypeAlias,
    TypeVar,
    Annotated,
    Literal,
}

impl SpecialExport {
    pub fn new(name: &Name) -> Option<Self> {
        match name.as_str() {
            "TypeAlias" => Some(Self::TypeAlias),
            "TypeVar" => Some(Self::TypeVar),
            "Annotated" => Some(Self::Annotated),
            "Literal" => Some(Self::Literal),
            _ => None,
        }
    }

    pub fn defined_in(self, m: ModuleName) -> bool {
        match self {
            Self::TypeAlias | Self::TypeVar | Self::Annotated | Self::Literal => {
                matches!(m.as_str(), "typing" | "typing_extensions")
            }
        }
    }
}
