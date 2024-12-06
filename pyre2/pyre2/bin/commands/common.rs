/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Parser;

#[derive(Debug, Parser, Clone)]
pub struct CommonArgs {
    #[clap(long, short = 'j', default_value = "0")]
    threads: usize,
}

impl CommonArgs {
    /// Sets up the parallelism and returns what you should pass to driver.
    /// You can call this function at most once.
    pub fn parallel(&self) -> bool {
        let mut builder = rayon::ThreadPoolBuilder::new();
        if self.threads != 0 {
            builder = builder.num_threads(self.threads);
        }
        // This fails if we call it twice, but we probably called it previously with the same
        // value, so don't worry about it.
        let _ = builder.build_global();
        self.threads != 1
    }
}
