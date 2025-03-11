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
///
/// When upgrading to an exclusive lock, you provide a value which must
/// match the currently locked value, or you will be rejected.
/// See the documentation around `ExclusiveLock` for more details.
#[derive(Debug)]
pub struct UpgradeLock<E, T> {
    /// The underling lock used for read/write operations
    value: RwLock<T>,
    /// The thing providing exclusivity. None means no one has exclusivity.
    /// Some means someone is exclusive and will signal after they finish.
    exclusive: ExclusiveLock<E>,
}

impl<E, T: Default> Default for UpgradeLock<E, T> {
    fn default() -> Self {
        UpgradeLock::new(T::default())
    }
}

pub struct UpgradeLockReadGuard<'a, E, T> {
    read: RwLockReadGuard<'a, T>,
    inner: &'a UpgradeLock<E, T>,
}

pub struct UpgradeLockExclusiveGuard<'a, E, T> {
    read: RwLockReadGuard<'a, T>,
    exclusive: ExclusiveLockGuard<'a, E>,
    inner: &'a UpgradeLock<E, T>,
}

pub struct UpgradeLockWriteGuard<'a, E, T> {
    write: RwLockWriteGuard<'a, T>,
    exclusive: ExclusiveLockGuard<'a, E>,
    inner: &'a UpgradeLock<E, T>,
}

impl<E, T> Deref for UpgradeLockReadGuard<'_, E, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.read.deref()
    }
}

impl<E, T> Deref for UpgradeLockExclusiveGuard<'_, E, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.read.deref()
    }
}

impl<E, T> Deref for UpgradeLockWriteGuard<'_, E, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.write.deref()
    }
}

impl<E, T> DerefMut for UpgradeLockWriteGuard<'_, E, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.write.deref_mut()
    }
}

impl<E, T> UpgradeLock<E, T> {
    pub fn new(value: T) -> Self {
        Self {
            value: RwLock::new(value),
            exclusive: ExclusiveLock::default(),
        }
    }

    pub fn read(&self) -> UpgradeLockReadGuard<E, T> {
        UpgradeLockReadGuard {
            read: self.value.read().unwrap(),
            inner: self,
        }
    }

    pub fn exclusive(&self, value: E) -> Option<UpgradeLockExclusiveGuard<E, T>>
    where
        E: PartialEq,
    {
        self.read().exclusive(value)
    }

    pub fn write(&self, value: E) -> Option<UpgradeLockWriteGuard<E, T>>
    where
        E: PartialEq,
    {
        Some(self.exclusive(value)?.write())
    }
}

impl<'a, E: PartialEq, T> UpgradeLockReadGuard<'a, E, T> {
    pub fn exclusive(self, value: E) -> Option<UpgradeLockExclusiveGuard<'a, E, T>> {
        drop(self.read);
        let exclusive = self.inner.exclusive.lock(value)?;
        let read = self.inner.value.read().unwrap();
        Some(UpgradeLockExclusiveGuard {
            read,
            exclusive,
            inner: self.inner,
        })
    }
}

impl<'a, E, T> UpgradeLockExclusiveGuard<'a, E, T> {
    pub fn write(self) -> UpgradeLockWriteGuard<'a, E, T> {
        drop(self.read);
        let write: RwLockWriteGuard<'_, T> = self.inner.value.write().unwrap();
        UpgradeLockWriteGuard {
            write,
            exclusive: self.exclusive,
            inner: self.inner,
        }
    }
}

impl<'a, E, T> UpgradeLockWriteGuard<'a, E, T> {
    pub fn exclusive(self) -> UpgradeLockExclusiveGuard<'a, E, T> {
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
