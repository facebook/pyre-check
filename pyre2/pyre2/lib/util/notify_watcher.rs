/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)] // A library, so need a complete API

use std::path::Path;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::time::Duration;
use std::time::Instant;

use notify::recommended_watcher;
use notify::Event;
use notify::RecommendedWatcher;
use notify::RecursiveMode;
use notify::Watcher as _;

use crate::util::watcher::Watcher;

pub struct NotifyWatcher {
    receiver: Receiver<notify::Result<Event>>,
    watcher: RecommendedWatcher,
}

impl NotifyWatcher {
    pub fn new() -> anyhow::Result<Self> {
        let (sender, receiver) = channel();
        let watcher = recommended_watcher(sender)?;
        Ok(Self { receiver, watcher })
    }
}

impl Watcher for NotifyWatcher {
    fn watch_dir(&mut self, path: &Path) -> anyhow::Result<()> {
        Ok(self.watcher.watch(path, RecursiveMode::Recursive)?)
    }

    fn unwatch_dir(&mut self, path: &Path) -> anyhow::Result<()> {
        Ok(self.watcher.unwatch(path)?)
    }

    fn wait(&mut self) -> anyhow::Result<Vec<Event>> {
        let mut res = Vec::new();
        res.push(self.receiver.recv()??);
        // Wait up to 0.1s to buffer up events
        let end = Instant::now() + Duration::from_secs_f32(0.1);
        while let Some(remaining) = end.checked_duration_since(Instant::now()) {
            match self.receiver.recv_timeout(remaining) {
                Ok(event) => res.push(event?),
                Err(_) => break,
            }
        }
        Ok(res)
    }
}
