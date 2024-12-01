/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use dupe::Dupe;
use human_bytes::human_bytes;
use memory_stats::memory_stats;

use crate::util::display::commas_iter;

#[derive(
    Debug, Clone, Copy, Dupe, Default, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub struct Bytes(u64);

impl Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", human_bytes(self.0 as f64))
    }
}

#[derive(Debug, Clone, Default)]
pub struct MemoryUsage {
    pub physical: Option<Bytes>,
    pub allocated: Option<Bytes>,
    pub active: Option<Bytes>,
}

impl Display for MemoryUsage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fields = [
            ("physical", self.physical),
            ("allocated", self.allocated),
            ("active", self.active),
        ];
        if fields.iter().all(|(_, v)| v.is_none()) {
            write!(f, "unknown")
        } else {
            write!(
                f,
                "{}",
                commas_iter(|| fields
                    .iter()
                    .filter_map(|(name, v)| v.map(|v| format!("{name} {v}"))))
            )
        }
    }
}

impl MemoryUsage {
    pub fn new() -> Self {
        let jemalloc = get_jemalloc_stats();
        Self {
            physical: memory_stats().map(|x| Bytes(x.physical_mem as u64)),
            allocated: jemalloc.allocated,
            active: jemalloc.active,
        }
    }
}

pub struct JemallocStats {
    pub active: Option<Bytes>,
    pub allocated: Option<Bytes>,
}

#[cfg(not(fbcode_build))]
fn get_jemalloc_stats() -> JemallocStats {
    JemallocStats {
        active: None,
        allocated: None,
    }
}

#[cfg(fbcode_build)]
fn get_jemalloc_stats() -> JemallocStats {
    fn get_all() -> Option<serde_json::Value> {
        // This options configuration flag string is passed to `malloc_stats_print()`.
        // Explanation: (<https://jemalloc.net/jemalloc.3.html>)
        // The default configuration prints minimal output, formatted as JSON.
        let alloc_stats_options = "Jmdablxg";
        serde_json::from_str(&allocator_stats::malloc_stats(alloc_stats_options).ok()?).ok()
    }

    fn get_field(stats: &serde_json::Value, val: &str) -> Option<Bytes> {
        Some(Bytes(
            stats.get("jemalloc")?.get("stats")?.get(val)?.as_u64()?,
        ))
    }

    let v = get_all().unwrap_or_default();
    JemallocStats {
        active: get_field(&v, "active"),
        allocated: get_field(&v, "allocated"),
    }
}
