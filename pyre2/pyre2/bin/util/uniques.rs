/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Unique values produced by a factory.
//! Typically used to produce fresh variables.

use std::fmt;
use std::fmt::Display;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use dupe::Dupe;

/// Vend fresh unique `Var`s.
/// Deliberately NOT Clone.
#[derive(Debug)]
pub struct UniqueFactory {
    unique: AtomicUsize,
}

impl Default for UniqueFactory {
    fn default() -> Self {
        Self::new()
    }
}

/// A unique value, provided two given values were produced by the same factory.
#[derive(Debug, Copy, Dupe, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unique(usize);

impl Display for Unique {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0 {
            write!(f, "_")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl Unique {
    pub fn zero() -> Self {
        // Safe because we create every UniqueFactory with a zero.
        Self(0)
    }
}

impl UniqueFactory {
    pub fn new() -> Self {
        Self {
            unique: AtomicUsize::new(1),
        }
    }

    pub fn fresh(&self) -> Unique {
        Unique(self.unique.fetch_add(1, Ordering::Relaxed))
    }
}
