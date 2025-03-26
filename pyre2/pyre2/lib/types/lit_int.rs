/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use parse_display::Display;
use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash, Display
)]
pub struct LitInt(i64);

impl LitInt {
    pub fn new(x: i64) -> Self {
        Self(x)
    }

    pub fn as_i64(&self) -> Option<i64> {
        Some(self.0)
    }

    pub fn negate(&self) -> Option<Self> {
        self.0.checked_neg().map(Self::new)
    }

    pub fn invert(&self) -> LitInt {
        Self(!self.0)
    }

    pub fn as_bool(&self) -> bool {
        self.0 != 0
    }
}
