/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem::ManuallyDrop;

/// Type that might be forgotten when it goes out of scope.
pub struct Forgetter<T> {
    forget: bool,
    value: ManuallyDrop<T>,
}

impl<T> Drop for Forgetter<T> {
    fn drop(&mut self) {
        if !self.forget {
            unsafe {
                ManuallyDrop::drop(&mut self.value);
            }
        }
    }
}

impl<T> AsRef<T> for Forgetter<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Forgetter<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Forgetter<T> {
    /// `forget` is `true` to call `forget`, `false` to call `drop`.
    pub fn new(value: T, forget: bool) -> Self {
        Self {
            forget,
            value: ManuallyDrop::new(value),
        }
    }
}
