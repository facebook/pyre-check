/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Read;
use std::path::PathBuf;

use anyhow::Context as _;
use starlark_map::small_map::SmallMap;
use tar::Archive;
use tracing::debug;
use zstd::stream::read::Decoder;

use crate::module::module_name::ModuleName;

const BUNDLED_TYPESHED_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/typeshed.tar.zst"));

#[derive(Debug, Clone)]
struct BundledTypeshedEntry {
    relative_path: PathBuf,
    content: String,
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

    pub fn new() -> anyhow::Result<Self> {
        let content = Self::unpack()?;
        let mut index = SmallMap::new();
        for (relative_path, content) in content {
            let module_name = ModuleName::from_relative_path(&relative_path)?;
            index.insert(
                module_name,
                BundledTypeshedEntry {
                    relative_path,
                    content,
                },
            );
        }
        Ok(Self { index })
    }

    pub fn find(&self, name: ModuleName) -> Option<(PathBuf, String)> {
        let entry = self.index.get(&name)?;
        // TODO(grievejia): Properly model paths for in-memory sources
        let fake_path =
            PathBuf::from("bundled /pyre2/third_party/typeshed").join(&entry.relative_path);
        Some((fake_path, entry.content.clone()))
    }
}

pub fn find_module(name: ModuleName, include: &[PathBuf]) -> Option<PathBuf> {
    let parts = name.components();
    let possibilities = vec![
        parts.join("/") + ".pyi",
        parts.join("/") + ".py",
        parts.join("/") + "/__init__.pyi",
        parts.join("/") + "/__init__.py",
    ];

    for include in include {
        for suffix in &possibilities {
            let path = include.join(suffix);
            if path.exists() {
                debug!("Found {name} at {}", path.display());
                return Some(path);
            }
        }
    }
    None
}
