/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

use crate::util::exclusive_lock::ExclusiveLock;
use crate::util::exclusive_lock::ExclusiveLockGuard;

/// A lock which can be used in three modes:
///
/// * `read`, where you and many others can simultaneously
///   read the contents of the value. If someone else is writing, you wait.
/// * `exclusive`, given a read lock you can upgrade to an exclusive lock,
///   where you are the only person who can write. If someone else has an
///   exclusive lock then upgrading will fail after waiting til they lose
///   exclusivity.
/// * `write`, given an exclusive lock, you can write.
///
/// The aim is to allow other readers, even though you are mutating
/// a subset of the state.
#[derive(Debug, Default)]
pub struct UpgradeLock<T> {
    /// The underling lock used for read/write operations
    value: RwLock<T>,
    /// The thing providing exclusivity. None means noone has exclusivity.
    /// Some means someone is exclusive and will signal after they finish.
    exclusive: ExclusiveLock,
}

pub struct UpgradeLockReadGuard<'a, T> {
    read: RwLockReadGuard<'a, T>,
    inner: &'a UpgradeLock<T>,
}

pub struct UpgradeLockExclusiveGuard<'a, T> {
    read: RwLockReadGuard<'a, T>,
    exclusive: ExclusiveLockGuard<'a>,
    inner: &'a UpgradeLock<T>,
}

pub struct UpgradeLockWriteGuard<'a, T> {
    write: RwLockWriteGuard<'a, T>,
    exclusive: ExclusiveLockGuard<'a>,
    inner: &'a UpgradeLock<T>,
}

impl<T> Deref for UpgradeLockReadGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.read.deref()
    }
}

impl<T> Deref for UpgradeLockExclusiveGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.read.deref()
    }
}

impl<T> Deref for UpgradeLockWriteGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.write.deref()
    }
}

impl<T> DerefMut for UpgradeLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.write.deref_mut()
    }
}

impl<T> UpgradeLock<T> {
    pub fn read(&self) -> UpgradeLockReadGuard<T> {
        UpgradeLockReadGuard {
            read: self.value.read().unwrap(),
            inner: self,
        }
    }

    pub fn write(&self) -> Option<UpgradeLockWriteGuard<T>> {
        Some(self.read().exclusive()?.write())
    }
}

impl<'a, T> UpgradeLockReadGuard<'a, T> {
    pub fn exclusive(self) -> Option<UpgradeLockExclusiveGuard<'a, T>> {
        drop(self.read);
        let exclusive = self.inner.exclusive.lock()?;
        let read = self.inner.value.read().unwrap();
        Some(UpgradeLockExclusiveGuard {
            read,
            exclusive,
            inner: self.inner,
        })
    }
}

impl<'a, T> UpgradeLockExclusiveGuard<'a, T> {
    pub fn write(self) -> UpgradeLockWriteGuard<'a, T> {
        drop(self.read);
        let write = self.inner.value.write().unwrap();
        UpgradeLockWriteGuard {
            write,
            exclusive: self.exclusive,
            inner: self.inner,
        }
    }
}

impl<'a, T> UpgradeLockWriteGuard<'a, T> {
    pub fn exclusive(self) -> UpgradeLockExclusiveGuard<'a, T> {
        // there is currently no downgrade on a write lock (it's experimental, but not in our version)
        // so we have to release the write then reacquire the read
        drop(self.write);
        let read = self.inner.value.read().unwrap();
        UpgradeLockExclusiveGuard {
            read,
            exclusive: self.exclusive,
            inner: self.inner,
        }
    }
}
