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

use notify::recommended_watcher;
use notify::Event;
use notify::RecommendedWatcher;
use notify::RecursiveMode;
use notify::Watcher as _;

pub struct Watcher {
    receiver: Receiver<notify::Result<Event>>,
    watcher: RecommendedWatcher,
}

impl Watcher {
    pub fn new() -> anyhow::Result<Self> {
        let (sender, receiver) = channel();
        let watcher = recommended_watcher(sender)?;
        Ok(Self { receiver, watcher })
    }

    pub fn watch_dir(&mut self, path: &Path) -> anyhow::Result<()> {
        Ok(self.watcher.watch(path, RecursiveMode::Recursive)?)
    }

    pub fn unwatch_dir(&mut self, path: &Path) -> anyhow::Result<()> {
        Ok(self.watcher.unwatch(path)?)
    }

    pub fn wait(&mut self) -> anyhow::Result<Event> {
        Ok(self.receiver.recv()??)
    }
}
