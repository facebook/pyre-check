/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use notify::Event;

pub trait Watcher {
    fn wait(&mut self) -> anyhow::Result<Vec<Event>>;
}
