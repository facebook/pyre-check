/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Utilities for creating the initial thread pool.

use std::sync::LazyLock;

use tracing::debug;

use crate::util::lock::Mutex;

static THREADS: LazyLock<Mutex<Option<usize>>> = LazyLock::new(|| Mutex::new(None));

/// Set up the global thread pool.
pub fn init_thread_pool(threads: Option<usize>) {
    *THREADS.lock() = threads;
}

/// A WASM compatible thread-pool.
pub struct ThreadPool(
    // Will be None on WASM
    Option<rayon::ThreadPool>,
);

impl ThreadPool {
    pub fn new() -> Self {
        if cfg!(target_arch = "wasm32") {
            // ThreadPool doesn't work on WASM
            return Self(None);
        }

        let mut builder = rayon::ThreadPoolBuilder::new().stack_size(4 * 1024 * 1024);
        if let Some(threads) = *THREADS.lock() {
            builder = builder.num_threads(threads);
        }
        let pool = builder.build().expect("To be able to build a thread pool");
        // Only print the message once
        debug!("Running with {} threads", pool.current_num_threads());
        Self(Some(pool))
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
