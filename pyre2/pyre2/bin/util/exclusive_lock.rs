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
#[derive(Debug, Default)]
pub struct ExclusiveLock {
    exclusive: Mutex<Option<Arc<Once>>>,
}

pub struct ExclusiveLockGuard<'a> {
    inner: Option<&'a ExclusiveLock>,
}

impl<'a> Drop for ExclusiveLockGuard<'a> {
    fn drop(&mut self) {
        if let Some(inner) = self.inner.take() {
            let mut lock = inner.exclusive.lock();
            if let Some(once) = &*lock {
                once.call_once(|| ());
                *lock = None;
            }
        }
    }
}

impl ExclusiveLock {
    pub fn lock(&self) -> Option<ExclusiveLockGuard> {
        let mut exclusive = self.exclusive.lock();
        match &*exclusive {
            None => {
                *exclusive = Some(Arc::new(Once::new()));
                Some(ExclusiveLockGuard { inner: Some(self) })
            }
            Some(m) => {
                let m = m.dupe();
                drop(exclusive);
                m.wait();
                None
            }
        }
    }
}
