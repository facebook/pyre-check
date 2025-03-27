/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;
use std::sync::Arc;

use dupe::Dupe;
use indicatif::ProgressBar;
use indicatif::ProgressStyle;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::state::handle::Handle;
use crate::state::load::Load;
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
                assert!(
                    e.get().1.is_some(),
                    "Handle started a second time without finishing: {:?}",
                    e.key()
                );
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

/// Progress bar subscriber that shows the progress of the computation.
/// Slightly tweaked to never show decreasing percentages.
pub struct ProgressBarSubscriber {
    /// The actual progress bar drawn to the screen.
    progress_bar: ProgressBar,
    /// The mutable data we have.
    state: Mutex<ProgressBarState>,
}

const PROGRESS_BAR_LENGTH: u64 = 1_000_000;

#[derive(Debug, Clone, Copy, Dupe)]
struct ProgressBarState {
    /// Number between 0 and PROGRESS_BAR_LENGTH representing how far I was last time.
    last_progress: u64,
    /// Number of `start_work` calls.
    started: u64,
    /// Number of `finish_work` calls.
    finished: u64,
}

impl ProgressBarSubscriber {
    fn event(&self, f: impl FnOnce(&mut ProgressBarState)) {
        let millis = self.progress_bar.elapsed().as_millis();

        // Do as little as possible with the lock held.
        let mut state = self.state.lock();
        f(&mut state);
        let mut progress = (state.finished * PROGRESS_BAR_LENGTH) / state.started.max(1);
        // In the first second, we don't want to complete more than 10%, as we might be discovering
        let limit = if millis > 1000 {
            PROGRESS_BAR_LENGTH
        } else {
            (PROGRESS_BAR_LENGTH * (millis as u64)) / 10000
        };
        progress = progress.min(limit).max(state.last_progress);
        state.last_progress = progress;
        let state_value = *state;
        drop(state);

        self.progress_bar.set_message(format!(
            "{:>7}/{:<7}",
            state_value.finished, state_value.started
        ));
        self.progress_bar.set_position(progress);
    }
}

impl Subscriber for ProgressBarSubscriber {
    fn start_work(&self, _: Handle) {
        self.event(|x| x.started += 1);
    }

    fn finish_work(&self, _: Handle, _: Arc<Load>) {
        self.event(|x| x.finished += 1);
    }
}

impl Drop for ProgressBarSubscriber {
    fn drop(&mut self) {
        self.progress_bar.finish_and_clear();
    }
}

impl ProgressBarSubscriber {
    pub fn new() -> Self {
        let progress_bar = ProgressBar::new(0);
        progress_bar.set_style(
            ProgressStyle::with_template("[{elapsed_precise}] {wide_bar:.yellow/red} {msg}")
                .unwrap(),
        );
        progress_bar.set_length(PROGRESS_BAR_LENGTH);
        let me = Self {
            progress_bar,
            state: Mutex::new(ProgressBarState {
                last_progress: 0,
                started: 0,
                finished: 0,
            }),
        };
        me.event(|_| ());
        me
    }
}
