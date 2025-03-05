/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Utilities for creating the initial thread pool.

use tracing::debug;

/// Set up the global thread pool.
pub fn init_rayon(threads: Option<usize>) {
    let mut builder = rayon::ThreadPoolBuilder::new().stack_size(4 * 1024 * 1024);
    if let Some(threads) = threads {
        builder = builder.num_threads(threads);
    }
    // This fails if we call it twice, but we probably called it previously with the same
    // value, so don't worry about it.
    if builder.build_global().is_ok() {
        // Only print the message once
        debug!("Running with {} threads", rayon::current_num_threads());
    }
}
