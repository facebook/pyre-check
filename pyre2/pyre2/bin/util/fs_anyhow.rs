/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::fs::ReadDir;
use std::path::Path;

use anyhow::Context as _;

pub fn read_to_string(path: &Path) -> anyhow::Result<String> {
    fs::read_to_string(path).with_context(|| format!("When reading file `{}`", path.display()))
}

pub fn read(path: &Path) -> anyhow::Result<Vec<u8>> {
    fs::read(path).with_context(|| format!("When reading file `{}`", path.display()))
}

pub fn write(path: &Path, contents: &[u8]) -> Result<(), anyhow::Error> {
    fs::write(path, contents).with_context(|| format!("When writing file `{}`", path.display()))
}

pub fn read_dir(path: &Path) -> anyhow::Result<ReadDir> {
    fs::read_dir(path).with_context(|| format!("When reading directory `{}`", path.display()))
}
