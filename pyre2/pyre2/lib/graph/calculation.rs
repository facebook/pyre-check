/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::thread;
use std::thread::ThreadId;

use dupe::Dupe;
use starlark_map::small_set::SmallSet;
use starlark_map::smallset;

use crate::util::lock::Mutex;

/// Recursive calculations by the same thread return None, but
/// if they are different threads they may start calculating.
///
/// We have to allow multiple threads to calculate the same value
/// in parallel, as you may have A, B that mutually recurse.
/// If thread 1 starts on A, then thread 2 starts on B, they will
/// deadlock if they both wait for the other to finish.
///
/// Assumes we don't use async (where recursive context may change
/// which thread is being used).
///
/// The type `T` is the final result, while `R` is the value if you
/// hit a recursive loop.
#[derive(Clone, Debug)]
enum Status<T, R> {
    /// This value has not yet been calculated.
    NotCalculated,
    /// This value is currently being calculated by the following threads.
    // Use a Box so the size of the struct stays small
    Calculating(Box<(Option<R>, SmallSet<ThreadId>)>),
    /// This value has been calculated.
    Calculated(T),
}

/// A cached calculation where recursive calculation returns None.
#[derive(Debug)]
pub struct Calculation<T, R = ()>(Mutex<Status<T, R>>);

impl<T, R> Default for Calculation<T, R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, R> Calculation<T, R> {
    pub fn new() -> Self {
        Self(Mutex::new(Status::NotCalculated))
    }
}

impl<T: Dupe, R: Dupe> Calculation<T, R> {
    /// Get the value if it has been calculated, otherwise `None`.
    /// Does not block.
    pub fn get(&self) -> Option<T> {
        let lock = self.0.lock();
        match &*lock {
            Status::Calculated(v) => Some(v.dupe()),
            _ => None,
        }
    }

    /// Force calculation. Given a function for doing the calculation,
    /// return the result. If the calculation is already in process, returns None.
    pub fn calculate(&self, calculate: impl FnOnce() -> T) -> Option<T>
    where
        R: Default,
    {
        self.calculate_with_recursive(calculate, R::default)
            .ok()
            .map(|(r, _)| r)
    }

    /// Force calculation. In addition to the simple [calculate] function, it also takes a function
    /// to generate values if it hits a recursive case. The result will be either the recursive value,
    /// or the result. The recursive value will be returned in the `Ok` at most once.
    pub fn calculate_with_recursive(
        &self,
        calculate: impl FnOnce() -> T,
        recursive: impl FnOnce() -> R,
    ) -> Result<(T, Option<R>), R> {
        let mut lock = self.0.lock();
        let thread = thread::current().id();
        match &mut *lock {
            Status::NotCalculated => {
                *lock = Status::Calculating(Box::new((None, smallset! {thread})))
            }
            Status::Calculating(box (rec, threads)) => {
                if !threads.insert(thread) {
                    match rec {
                        None => {
                            let r = recursive();
                            *rec = Some(r.dupe());
                            return Err(r);
                        }
                        Some(r) => return Err(r.dupe()),
                    }
                }
            }
            Status::Calculated(v) => return Ok((v.dupe(), None)),
        }
        drop(lock);
        let result = calculate();
        let mut lock = self.0.lock();
        match &mut *lock {
            Status::NotCalculated => {
                unreachable!("Should have started calculating before we finished")
            }
            Status::Calculated(v) => {
                // If we raced, just return the first one, so we have consistent ArcId etc.
                Ok((v.dupe(), None))
            }
            Status::Calculating(c) => {
                let rec = c.0.take();
                *lock = Status::Calculated(result.dupe());
                Ok((result, rec))
            }
        }
    }
}
