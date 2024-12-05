/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use anyhow::anyhow;
use starlark_map::small_map::SmallMap;

use crate::config::Config;
use crate::error::error::Error;
use crate::module::module_name::ModuleName;
use crate::state::loader::LoadResult;
use crate::state::loader::Loader;
use crate::state::state::State;
use crate::test::stdlib::Stdlib;
use crate::util::trace::init_tracing;

#[macro_export]
macro_rules! simple_test {
    ($name:ident, $imports:expr, $contents:expr, $error_check:expr, ) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::simple_test_for_macro(
                $imports,
                $contents,
                file!(),
                line!(),
                Some($error_check),
            )
        }
    };
    ($name:ident, $imports:expr, $contents:expr, ) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::simple_test_for_macro($imports, $contents, file!(), line!(), None)
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
                None,
            )
        }
    };
}

fn default_path(name: ModuleName) -> PathBuf {
    PathBuf::from(format!("{}.py", name.as_str().replace('.', "/")))
}

#[derive(Debug, Default)]
pub struct TestEnv(SmallMap<ModuleName, (PathBuf, Result<String, String>)>);

impl TestEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_with_path(&mut self, name: &str, code: &str, path: &str) {
        self.0.insert(
            ModuleName::from_str(name),
            (PathBuf::from(path), Ok(code.to_owned())),
        );
    }

    pub fn add(&mut self, name: &str, code: &str) {
        let module_name = ModuleName::from_str(name);
        let relative_path = default_path(module_name);
        self.0
            .insert(module_name, (relative_path, Ok(code.to_owned())));
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

    pub fn add_error(&mut self, name: &str, err: &str) {
        let module_name = ModuleName::from_str(name);
        self.0.insert(
            module_name,
            (default_path(module_name), Err(err.to_owned())),
        );
    }

    pub fn to_loader(self, stdlib: Stdlib) -> Box<Loader<'static>> {
        Box::new(move |name: ModuleName| {
            let loaded = if let Some((path, contents)) = self.0.get(&name) {
                match contents {
                    Ok(contents) => LoadResult::Loaded(path.to_owned(), contents.to_owned()),
                    Err(err) => LoadResult::FailedToLoad(path.to_owned(), anyhow!(err.to_owned())),
                }
            } else if let Some(contents) = stdlib.lookup_content(name) {
                LoadResult::Loaded(default_path(name), contents.to_owned())
            } else {
                LoadResult::FailedToFind(anyhow!("Module not given in test suite"))
            };
            (loaded, true)
        })
    }
}

pub fn simple_test_driver(env: TestEnv) -> State<'static> {
    let stdlib = Stdlib::new();
    let modules = stdlib
        .modules()
        .copied()
        .chain(env.0.keys().copied())
        .collect::<Vec<_>>();
    let mut state = State::new(
        &modules,
        env.to_loader(stdlib),
        Config::default(),
        true,
        true,
    );
    state.run();
    state
}

/// Should only be used from the `simple_test!` macro.
pub fn simple_test_for_macro(
    mut env: TestEnv,
    contents: &str,
    file: &str,
    line: u32,
    error_check: Option<fn(&[Error]) -> anyhow::Result<()>>,
) -> anyhow::Result<()> {
    init_tracing(true, true);
    let mut start_line = line as usize + 1;
    if !env.0.is_empty() {
        start_line += 1;
    }
    env.add_with_path(
        "main",
        &format!("{}{}", "\n".repeat(start_line), contents),
        file,
    );
    let state = simple_test_driver(env);
    match error_check {
        None => state.check_against_expectations(),
        Some(check) => check(&state.collect_errors()),
    }
}
