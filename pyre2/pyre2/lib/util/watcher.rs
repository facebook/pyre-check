/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use async_trait::async_trait;
use notify::Event;

#[async_trait]
pub trait Watcher {
    async fn wait(&mut self) -> anyhow::Result<Vec<Event>>;
}
