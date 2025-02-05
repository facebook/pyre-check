/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use tracing::debug;

use crate::module::module_name::ModuleName;

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
