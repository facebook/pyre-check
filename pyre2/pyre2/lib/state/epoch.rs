/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Epoch(u32);

impl Epoch {
    pub fn zero() -> Self {
        Self(0)
    }

    pub fn next(&mut self) {
        self.0 += 1;
    }
}

/// Invariant: checked >= computed >= changed
///
/// Whenever we confirm this data is up to date, we bump checked.
/// If to do so, we have to recompute everything, we bump computed.
/// If when computing it we change the data, we bump changed.
///
/// If A depends on B, and A.computed < B.changed, then A is stale.
#[derive(Debug, Clone, Copy)]
pub struct Epochs {
    /// The point at which we have validated that everything is correct.
    pub checked: Epoch,
    /// The point at which we last computed everything.
    pub computed: Epoch,
    /// The last point at which something changed.
    pub changed: Epoch,
}
impl Epochs {
    pub fn new(now: Epoch) -> Self {
        Self {
            checked: now,
            changed: now,
            computed: now,
        }
    }
}
