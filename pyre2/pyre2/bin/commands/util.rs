/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;

use crate::dunder;
use crate::module::module_name::ModuleName;

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
