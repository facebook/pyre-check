/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::LazyLock;

use anyhow::anyhow;
use anyhow::Context as _;
use dupe::Dupe;
use starlark_map::small_map::SmallMap;
use tar::Archive;
use zstd::stream::read::Decoder;

use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;

const BUNDLED_TYPESHED_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/typeshed.tar.zst"));

#[derive(Debug, Clone)]
struct BundledTypeshedEntry {
    relative_path: PathBuf,
    content: Arc<String>,
}

#[derive(Debug, Clone)]
pub struct BundledTypeshed {
    index: SmallMap<ModuleName, BundledTypeshedEntry>,
}

impl BundledTypeshed {
    fn unpack() -> anyhow::Result<SmallMap<PathBuf, String>> {
        let decoder = Decoder::new(BUNDLED_TYPESHED_BYTES)?;
        let mut archive = Archive::new(decoder);
        let entries = archive
            .entries()
            .context("Cannot query all entries in typehsed archive")?;

        let mut items = SmallMap::new();
        for maybe_entry in entries {
            let mut entry =
                maybe_entry.context("Cannot read individual entry in typeshed archive")?;
            if entry.header().entry_type().is_dir() {
                // Skip directories
                continue;
            }
            let relative_path = entry
                .path()
                .context("Cannot extract path from archive entry")?
                .components()
                .collect::<PathBuf>();
            let size = entry.size();
            let mut content = String::with_capacity(size as usize);
            entry
                .read_to_string(&mut content)
                .context("Cannot read content of archive entry")?;
            items.entry(relative_path).or_insert(content);
        }
        Ok(items)
    }

    fn new() -> anyhow::Result<Self> {
        let content = Self::unpack()?;
        let mut index = SmallMap::new();
        for (relative_path, content) in content {
            let module_name = ModuleName::from_relative_path(&relative_path)?;
            index.insert(
                module_name,
                BundledTypeshedEntry {
                    relative_path,
                    content: Arc::new(content),
                },
            );
        }
        Ok(Self { index })
    }

    pub fn find(&self, name: ModuleName) -> Option<(ModulePath, Arc<String>)> {
        let entry = self.index.get(&name)?;
        let fake_path = ModulePath::bundled_typeshed(entry.relative_path.clone());
        Some((fake_path, entry.content.dupe()))
    }
}

static BUNDLED_TYPESHED: LazyLock<anyhow::Result<BundledTypeshed>> =
    LazyLock::new(BundledTypeshed::new);

pub fn typeshed() -> anyhow::Result<&'static BundledTypeshed> {
    match &*BUNDLED_TYPESHED {
        Ok(typeshed) => Ok(typeshed),
        Err(error) => Err(anyhow!("{error:#}")),
    }
}
