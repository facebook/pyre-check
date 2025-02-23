/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Default, Clone)]
pub struct Dirty {
    dirty_load: bool,
    dirty_find: bool,
    dirty_deps: bool,
}

impl Dirty {
    pub fn set_dirty_load(&mut self) {
        self.dirty_load = true;
    }

    pub fn set_dirty_find(&mut self) {
        self.dirty_find = true;
    }

    pub fn set_dirty_deps(&mut self) {
        self.dirty_deps = true;
    }

    pub fn is_dirty_load(&self) -> bool {
        self.dirty_load
    }

    pub fn is_dirty_find(&self) -> bool {
        self.dirty_find
    }

    pub fn is_dirty_deps(&self) -> bool {
        self.dirty_deps
    }

    pub fn clean(&mut self) {
        self.dirty_load = false;
        self.dirty_find = false;
        self.dirty_deps = false;
    }
}
