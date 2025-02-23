/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;
use std::sync::Arc;

use dupe::Dupe;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::state::handle::Handle;
use crate::state::steps::Load;
use crate::util::lock::Mutex;

/// Trait to capture which handles are executed by `State`.
/// Calls to `start_work` and `finish_work` will be paired.
/// It may be the case that a single `Handle` is computed multiple times within a single call to `run`.
///
/// The `Subscriber` will be invoked on a working thread, so should complete quickly.
pub trait Subscriber: Sync {
    /// We are starting work on a `Handle`, with the intention to computing at
    /// least some of its steps.
    fn start_work(&self, handle: Handle);

    /// We have finished work on a `Handle`, having computed its solutions.
    /// While we have computed the solutions, we return the `Load` as that contains
    /// the `ErrorCollector` and `ModuleInfo` which are useful context for the completion.
    fn finish_work(&self, handle: Handle, result: Arc<Load>);
}

/// A subscriber that validates all start/finish are paired and returns the final load states.
#[derive(Debug, Default, Clone, Dupe)]
pub struct TestSubscriber(Arc<Mutex<SmallMap<Handle, (usize, Option<Arc<Load>>)>>>);

impl Subscriber for TestSubscriber {
    fn start_work(&self, handle: Handle) {
        let mut value = self.0.lock();
        match value.entry(handle) {
            Entry::Vacant(e) => {
                e.insert((1, None));
            }
            Entry::Occupied(mut e) => {
                assert!(e.get().1.is_some());
                e.get_mut().0 += 1;
                e.get_mut().1 = None;
            }
        }
    }

    fn finish_work(&self, handle: Handle, result: Arc<Load>) {
        let mut value = self.0.lock();
        match value.entry(handle.dupe()) {
            Entry::Vacant(_) => panic!("Handle finished but never started: {handle:?}"),
            Entry::Occupied(mut e) => {
                assert!(e.get().1.is_none());
                e.get_mut().1 = Some(result);
            }
        }
    }
}

impl TestSubscriber {
    #[allow(dead_code)] // Only in test code
    pub fn new() -> Self {
        Self::default()
    }

    /// For each handle, return a pair of (the number of times each handle started, the final load state).
    /// Panics if any handle was started but not finished.
    #[allow(dead_code)] // Only in test code
    pub fn finish(self) -> SmallMap<Handle, (usize, Arc<Load>)> {
        let value = mem::take(&mut *self.0.lock());
        let mut res = SmallMap::with_capacity(value.len());
        for (k, v) in value {
            match v.1 {
                Some(load) => {
                    res.insert(k, (v.0, load));
                }
                None => panic!("Handle started but never finished: {k:?}"),
            }
        }
        res
    }
}
