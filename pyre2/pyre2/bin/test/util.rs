/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use starlark_map::small_map::SmallMap;

use crate::alt::driver::Driver;
use crate::alt::driver::LoadResult;
use crate::config::Config;
use crate::module::module_name::ModuleName;
use crate::test::stdlib::Stdlib;
use crate::util::trace::init_tracing;

#[macro_export]
macro_rules! simple_test {
    ($name:ident, $imports:expr, $contents:expr, ) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::simple_test_for_macro($imports, $contents, file!(), line!())
        }
    };
    ($name:ident, $contents:expr,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::simple_test_for_macro(
                $crate::test::util::TestEnv::new(),
                $contents,
                file!(),
                line!(),
            )
        }
    };
}

fn default_path(name: ModuleName) -> PathBuf {
    PathBuf::from(format!("{}.py", name.as_str().replace('.', "/")))
}

#[derive(Debug, Default, Clone)]
pub struct TestEnv(SmallMap<ModuleName, (PathBuf, String)>);

impl TestEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_with_path(&mut self, name: &str, code: &str, path: &str) {
        self.0.insert(
            ModuleName::from_str(name),
            (PathBuf::from(path), code.to_owned()),
        );
    }

    pub fn add(&mut self, name: &str, code: &str) {
        let module_name = ModuleName::from_str(name);
        let relative_path = default_path(module_name);
        self.0.insert(module_name, (relative_path, code.to_owned()));
    }

    pub fn one(name: &str, code: &str) -> Self {
        let mut res = Self::new();
        res.add(name, code);
        res
    }

    pub fn one_with_path(name: &str, code: &str, path: &str) -> Self {
        let mut res = Self::new();
        res.add_with_path(name, code, path);
        res
    }
}

pub fn simple_test_driver(stdlib: Stdlib, env: TestEnv) -> Driver {
    let modules = stdlib
        .modules()
        .copied()
        .chain(env.0.keys().copied())
        .collect::<Vec<_>>();
    let lookup = |name: ModuleName| {
        let loaded = if let Some((path, contents)) = env.0.get(&name) {
            LoadResult::Loaded(path.to_owned(), contents.to_owned())
        } else if let Some(contents) = stdlib.lookup_content(name) {
            LoadResult::Loaded(default_path(name), contents.to_owned())
        } else {
            LoadResult::FailedToFind(anyhow::anyhow!("Module not given in test suite"))
        };
        (loaded, true)
    };
    let config = Config::default();
    Driver::new(&modules, &config, false, None, true, lookup)
}

/// Should only be used from the `simple_test!` macro.
pub fn simple_test_for_macro(
    mut env: TestEnv,
    contents: &str,
    file: &str,
    line: u32,
) -> anyhow::Result<()> {
    init_tracing(true, true);
    env.add_with_path(
        "main",
        &format!("{}{}", "\n".repeat(line as usize + 1), contents),
        file,
    );
    simple_test_driver(Stdlib::new(), env).check_against_expectations()
}
