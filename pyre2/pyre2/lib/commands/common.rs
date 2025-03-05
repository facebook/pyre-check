/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Parser;

use crate::clap_env;
use crate::util::rayon::init_rayon;

#[derive(Debug, Parser, Clone)]
pub struct CommonArgs {
    #[clap(long, short = 'j', default_value = "0", env = clap_env("THREADS"))]
    threads: usize,
}

impl CommonArgs {
    /// Sets up the parallelism and returns what you should pass to driver.
    /// You can call this function at most once.
    pub fn parallel(&self) -> bool {
        let threads = if self.threads == 0 {
            None
        } else {
            Some(self.threads)
        };
        init_rayon(threads);
        rayon::current_num_threads() != 1
    }
}
