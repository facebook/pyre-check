/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp;
use std::fmt;
use std::fmt::Display;
use std::sync::Arc;
use std::thread::sleep;
use std::thread::spawn;
use std::time::Duration;

use dupe::Dupe;
use human_bytes::human_bytes;
use memory_stats::memory_stats;

use crate::util::display::commas_iter;
use crate::util::lock::Mutex;

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
    pub fn now() -> Self {
        let jemalloc = get_jemalloc_stats();
        Self {
            physical: memory_stats().map(|x| Bytes(x.physical_mem as u64)),
            allocated: jemalloc.allocated,
            active: jemalloc.active,
        }
    }

    pub fn max_by_field(x: &Self, y: &Self) -> Self {
        Self {
            physical: cmp::max(x.physical, y.physical),
            allocated: cmp::max(x.allocated, y.allocated),
            active: cmp::max(x.active, y.active),
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

pub struct MemoryUsageTrace {
    /// Pair of (stop condition, values that were recorded).
    /// Once stop is set to `true` we won't record any more results.
    state: Arc<Mutex<(bool, MemoryUsage)>>,
}

impl MemoryUsageTrace {
    pub fn start(frequency: Duration) -> Self {
        let state = Arc::new(Mutex::new((false, MemoryUsage::now())));
        let state2 = state.dupe();
        spawn(move || {
            loop {
                sleep(frequency);
                let mut lock = state2.lock();
                if lock.0 {
                    break;
                }
                lock.1 = MemoryUsage::max_by_field(&lock.1, &MemoryUsage::now());
            }
        });
        Self { state }
    }

    pub fn stop(&mut self) {
        let mut lock = self.state.lock();
        if !lock.0 {
            lock.0 = true;
            // If we were running before, make sure we capture a final snapshot
            lock.1 = MemoryUsage::max_by_field(&lock.1, &MemoryUsage::now());
        }
    }

    /// Won't necessarily be a single MemoryUsage, but the peak across many.
    pub fn peak(&self) -> MemoryUsage {
        self.state.lock().1.clone()
    }
}
