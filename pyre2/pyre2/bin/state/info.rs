/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::time::Instant;

#[derive(Debug)]
pub struct Info<T> {
    time: Option<(Instant, Instant)>,
    value: Option<T>,
}

impl<T> Default for Info<T> {
    fn default() -> Self {
        Self {
            time: Default::default(),
            value: Default::default(),
        }
    }
}

impl<T> Info<T> {
    pub fn take(&mut self) -> Option<T> {
        self.value.take()
    }

    pub fn get(&self) -> Option<&T> {
        self.value.as_ref()
    }

    pub fn is_some(&self) -> bool {
        self.value.is_some()
    }

    pub fn start() -> Self {
        let now = Instant::now();
        Self {
            time: Some((now, now)),
            value: None,
        }
    }

    pub fn stop(self, v: T) -> Self {
        let now = Instant::now();
        Self {
            time: Some((self.time.map_or(now, |x| x.0), now)),
            value: Some(v),
        }
    }

    pub fn with(f: impl FnOnce() -> T) -> Self {
        let info = Self::start();
        info.stop(f())
    }
}
