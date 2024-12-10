/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::env::current_dir;
use std::ffi::OsStr;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use anyhow::Context as _;
use tracing::debug;

use crate::dunder;
use crate::module::module_name::ModuleName;

pub fn default_include() -> anyhow::Result<Vec<PathBuf>> {
    default_include_inner().context("calculating the default include path")
}

fn default_include_inner() -> anyhow::Result<Vec<PathBuf>> {
    // run the command hg root and get the output
    let result = Command::new("hg")
        .arg("root")
        .output()
        .context("running `hg root`")?;
    let root = String::from_utf8(result.stdout)?;
    let root = root.trim();
    let stdlib = PathBuf::from(format!(
        "{root}/fbcode/tools/pyre/stubs/typeshed/typeshed/stdlib"
    ));
    let stdlib = pathdiff::diff_paths(&stdlib, current_dir()?).unwrap_or(stdlib);
    Ok(vec![stdlib])
}

pub fn module_from_path(path: &Path, includes: &[PathBuf]) -> ModuleName {
    // Return a module name, and a boolean as to whether it is any good.
    fn path_to_module(mut path: &Path) -> (ModuleName, bool) {
        if path.file_stem() == Some(OsStr::new(dunder::INIT.as_str())) {
            match path.parent() {
                Some(parent) => path = parent,
                None => return (ModuleName::from_name(&dunder::INIT), false),
            }
        }
        let mut out = Vec::new();
        let path = path.with_extension("");
        for x in path.components() {
            if let Component::Normal(x) = x
                && !x.is_empty()
            {
                out.push(x.to_string_lossy());
            }
        }
        if out.is_empty() {
            (ModuleName::from_str("main"), false)
        } else {
            (ModuleName::from_string(out.join(".")), true)
        }
    }

    for include in includes {
        if let Ok(x) = path.strip_prefix(include) {
            let (res, good) = path_to_module(x);
            if good {
                return res;
            }
        }
    }
    path_to_module(path).0
}

pub fn find_module(name: ModuleName, include: &[PathBuf]) -> anyhow::Result<PathBuf> {
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
                return Ok(path);
            }
        }
    }
    Err(anyhow::anyhow!("Could not find path for `{name}`"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_from_path() {
        let includes = vec![PathBuf::from("/foo/bar")];
        assert_eq!(
            module_from_path(&PathBuf::from("/foo/bar/baz.py"), &includes),
            ModuleName::from_str("baz")
        );
        assert_eq!(
            module_from_path(&PathBuf::from("/foo/bar/baz/qux.pyi"), &includes),
            ModuleName::from_str("baz.qux")
        );
        assert_eq!(
            module_from_path(&PathBuf::from("/foo/bar/baz/test/magic.py"), &includes),
            ModuleName::from_str("baz.test.magic")
        );
        assert_eq!(
            module_from_path(&PathBuf::from("/foo/bar/baz/__init__.pyi"), &includes),
            ModuleName::from_str("baz")
        );
        assert_eq!(
            module_from_path(&PathBuf::from("/test.py"), &includes),
            ModuleName::from_str("test")
        );
        assert_eq!(
            module_from_path(&PathBuf::from("/not_foo/test.py"), &includes),
            ModuleName::from_str("not_foo.test")
        );
    }
}
