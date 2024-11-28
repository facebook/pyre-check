/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use human_bytes::human_bytes;
use memory_stats::memory_stats;

pub struct Bytes(usize);

impl Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", human_bytes(self.0 as f64))
    }
}

pub struct MemoryUsage {
    pub physical: Option<Bytes>,
}

impl MemoryUsage {
    pub fn new() -> Self {
        Self {
            physical: memory_stats().map(|x| Bytes(x.physical_mem)),
        }
    }
}
