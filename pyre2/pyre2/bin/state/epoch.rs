/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Epoch(u32);

impl Epoch {
    pub fn zero() -> Self {
        Self(0)
    }

    pub fn next(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Epochs {
    /// The point at which we have validated that everything is correct.
    pub checked: Epoch,
    /// The last point at which something changed.
    pub changed: Epoch,
}
impl Epochs {
    pub fn new(now: Epoch) -> Self {
        Self {
            checked: now,
            changed: now,
        }
    }
}
