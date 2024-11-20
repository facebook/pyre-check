/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::hash::Hash;
use std::time::Duration;
use std::time::Instant;

use starlark_map::small_map::SmallMap;

pub struct Timer {
    last: Instant,
}

impl Timer {
    pub fn new() -> Self {
        Self {
            last: Instant::now(),
        }
    }

    /// Returns the time since the last call to this function.
    pub fn elapsed(&mut self) -> Duration {
        let now = Instant::now();
        let elapsed = now.duration_since(self.last);
        self.last = now;
        elapsed
    }
}

pub struct TimerContext<K> {
    timer: Timer,
    items: Vec<(K, Duration)>,
}

impl<K> TimerContext<K> {
    pub fn new() -> Self {
        Self {
            timer: Timer::new(),
            items: Vec::new(),
        }
    }

    pub fn add(&mut self, key: K) -> Duration {
        let d = self.timer.elapsed();
        self.items.push((key, d));
        d
    }

    pub fn total(&self) -> Duration {
        self.items.iter().map(|(_, d)| d).sum()
    }

    pub fn ordered(&mut self) -> &[(K, Duration)] {
        // We never return the list anything other than ordered by duration,
        // so we can just sort in place.
        self.items.sort_by(|(_, d1), (_, d2)| d2.cmp(d1));
        &self.items
    }

    /// Group all the items by a key function, and sum the durations.
    pub fn grouped<Q: Eq + Hash>(&self, key: impl Fn(&K) -> Q) -> Vec<(Q, Duration)> {
        let mut mp = SmallMap::new();
        for (k, d) in &self.items {
            let q = key(k);
            *mp.entry(q).or_default() += *d;
        }
        let mut res: Vec<(Q, Duration)> = mp.into_iter().collect();
        res.sort_by(|(_, d1), (_, d2)| d2.cmp(d1));
        res
    }
}
