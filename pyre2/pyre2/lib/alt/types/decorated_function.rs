/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use ruff_text_size::TextRange;

use crate::types::types::Type;

/// The type of a function definition after decorators are applied. Metadata arising from the
/// decorators can be stored here. Note that the type might not be a function at all, since
/// decorators can produce any type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecoratedFunction {
    pub id_range: TextRange,
    pub ty: Type,
    pub is_overload: bool,
}

impl Display for DecoratedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.ty, f)
    }
}

impl DecoratedFunction {
    pub fn recursive() -> Self {
        DecoratedFunction {
            id_range: TextRange::default(),
            ty: Type::any_implicit(),
            is_overload: false,
        }
    }
}
