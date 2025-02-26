/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use notify::Event;

pub trait Watcher {
    fn watch_dir(&mut self, path: &Path) -> anyhow::Result<()>;
    #[allow(unused)] // May be used in the future
    fn unwatch_dir(&mut self, path: &Path) -> anyhow::Result<()>;
    fn wait(&mut self) -> anyhow::Result<Vec<Event>>;
}
