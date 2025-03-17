/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use async_trait::async_trait;
use notify::Event;
use notify::EventKind;

#[async_trait]
pub trait Watcher {
    async fn wait(&mut self) -> anyhow::Result<Vec<Event>>;
}

pub struct CategorizedEvents {
    pub created: Vec<PathBuf>,
    pub modified: Vec<PathBuf>,
    pub removed: Vec<PathBuf>,
    pub unknown: Vec<PathBuf>,
}

impl CategorizedEvents {
    pub fn new(events: Vec<Event>) -> Self {
        let mut created = Vec::new();
        let mut modified = Vec::new();
        let mut removed = Vec::new();
        let mut unknown = Vec::new();

        for event in events {
            match event.kind {
                EventKind::Create(_) => {
                    created.extend(event.paths);
                }
                EventKind::Modify(_) => {
                    modified.extend(event.paths);
                }
                EventKind::Remove(_) => {
                    removed.extend(event.paths);
                }
                EventKind::Any => {
                    unknown.extend(event.paths);
                }
                EventKind::Access(_) | EventKind::Other => {}
            }
        }

        CategorizedEvents {
            created,
            modified,
            removed,
            unknown,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.created.is_empty()
            && self.modified.is_empty()
            && self.removed.is_empty()
            && self.unknown.is_empty()
    }
}
