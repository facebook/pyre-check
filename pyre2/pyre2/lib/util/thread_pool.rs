/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Utilities for creating the initial thread pool.

use std::num::NonZeroUsize;
use std::str::FromStr;
use std::sync::LazyLock;

use tracing::debug;

use crate::util::lock::Mutex;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum ThreadCount {
    #[default]
    AllThreads,
    NumThreads(NonZeroUsize),
}

impl FromStr for ThreadCount {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<usize>() {
            Ok(n) => match NonZeroUsize::new(n) {
                None => Ok(ThreadCount::AllThreads),
                Some(n) => Ok(ThreadCount::NumThreads(n)),
            },
            Err(e) => Err(format!(
                "Failed to parse thread count, expected number, failed due to {e}"
            )),
        }
    }
}

static THREADS: LazyLock<Mutex<ThreadCount>> = LazyLock::new(|| Mutex::new(ThreadCount::default()));

/// Set up the global thread pool.
pub fn init_thread_pool(threads: ThreadCount) {
    *THREADS.lock() = threads;
}

/// A WASM compatible thread-pool.
pub struct ThreadPool(
    // Will be None on WASM
    Option<rayon::ThreadPool>,
);

impl ThreadPool {
    pub fn with_thread_count(count: ThreadCount) -> Self {
        if cfg!(target_arch = "wasm32") {
            // ThreadPool doesn't work on WASM
            return Self(None);
        }

        let mut builder = rayon::ThreadPoolBuilder::new().stack_size(4 * 1024 * 1024);
        if let ThreadCount::NumThreads(threads) = count {
            builder = builder.num_threads(threads.get());
        }
        let pool = builder.build().expect("To be able to build a thread pool");
        // Only print the message once
        debug!("Running with {} threads", pool.current_num_threads());
        Self(Some(pool))
    }

    pub fn new() -> Self {
        Self::with_thread_count(*THREADS.lock())
    }

    pub fn spawn_many(&self, f: impl Fn() + Sync) {
        match &self.0 {
            None => f(),
            Some(pool) => {
                pool.scope(|s| {
                    for _ in 0..pool.current_num_threads() {
                        // Only run work on Rayon threads, as we increased their stack limit
                        s.spawn(|_| f());
                    }
                })
            }
        }
    }
}
