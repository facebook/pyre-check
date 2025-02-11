/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use crate::types::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionAnswer {
    pub ty: Type,
    pub is_overload: bool,
}

impl Display for FunctionAnswer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.ty, f)
    }
}

impl FunctionAnswer {
    pub fn recursive() -> Self {
        FunctionAnswer {
            ty: Type::any_implicit(),
            is_overload: false,
        }
    }
}
