/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Define exclusive locks, where only one thread can hold the lock at a time.

use std::sync::Arc;
use std::sync::Once;

use dupe::Dupe;

use crate::util::lock::Mutex;

/// Like a normal lock, but anyone who attempts to take the lock will block until the lock is
/// released, and then not take the lock. They are expected to retry (after checking they
/// still need the lock).
#[derive(Debug)]
pub struct ExclusiveLock<T> {
    exclusive: Mutex<Option<Arc<(T, Once)>>>,
}

impl<T> Default for ExclusiveLock<T> {
    fn default() -> Self {
        Self {
            exclusive: Mutex::new(None),
        }
    }
}

pub struct ExclusiveLockGuard<'a, T> {
    inner: Option<&'a ExclusiveLock<T>>,
}

impl<'a, T> Drop for ExclusiveLockGuard<'a, T> {
    fn drop(&mut self) {
        if let Some(inner) = self.inner.take() {
            let mut lock = inner.exclusive.lock();
            if let Some(once) = &*lock {
                once.1.call_once(|| ());
                *lock = None;
            }
        }
    }
}

impl<T: PartialEq> ExclusiveLock<T> {
    /// If the lock is not held, take it and return a guard.
    /// If the lock is held, and the value matches, wait for the lock to be released then return None.
    /// If the lock is held, and the value does not match, return None immediately.
    pub fn lock(&self, value: T) -> Option<ExclusiveLockGuard<T>> {
        let mut exclusive = self.exclusive.lock();
        match &*exclusive {
            None => {
                *exclusive = Some(Arc::new((value, Once::new())));
                Some(ExclusiveLockGuard { inner: Some(self) })
            }
            Some(m) => {
                let m = m.dupe();
                drop(exclusive);
                if m.0 == value {
                    m.1.wait();
                }
                None
            }
        }
    }
}
