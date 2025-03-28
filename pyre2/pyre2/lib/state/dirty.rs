/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

#[derive(Debug, Default, Dupe, Clone, Copy)]
pub struct Dirty {
    /// The result from loading has potentially changed, either
    /// `load_from_memory` on `Loader` (if a memory file path) or
    /// the underlying disk if a disk file path.
    pub load: bool,
    /// The result from finding has potentially changed.
    /// Given all data is indexed by `Handle`, the path in the `Handle` can't
    /// change or it would simply represent a different `Handle`.
    /// This instead represents the modules I found from my imports have changed.
    pub find: bool,
    /// The result I got from my dependencies have potentially changed.
    pub deps: bool,
    /// I have increased the amount of data I `Require`.
    pub require: bool,
}

impl Dirty {
    pub fn clean(&mut self) {
        *self = Dirty::default();
    }
}
