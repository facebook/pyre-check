/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
use std::sync::Once;
use std::thread::sleep;
use std::time::Duration;
use std::time::Instant;

use anyhow::anyhow;
use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;

use crate::binding::binding::KeyExport;
use crate::config::Config;
use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::loader::LoadResult;
use crate::state::loader::Loader;
use crate::state::state::State;
use crate::test::stdlib::lookup_test_stdlib;
use crate::types::class::Class;
use crate::types::types::Type;
use crate::util::trace::init_tracing;

#[macro_export]
macro_rules! testcase {
    ($name:ident, $imports:expr, $contents:expr,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!())
        }
    };
    ($name:ident, $contents:expr,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro(
                $crate::test::util::TestEnv::new(),
                $contents,
                file!(),
                line!(),
            )
        }
    };
}

#[macro_export]
macro_rules! testcase_with_bug {
    ($explanatation:literal, $name:ident, $imports:expr, $contents:expr,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!() + 1)
        }
    };
    ($explanatation:literal, $name:ident, $contents:expr,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro(
                $crate::test::util::TestEnv::new(),
                $contents,
                file!(),
                line!() + 1,
            )
        }
    };
}

fn default_path(name: ModuleName) -> PathBuf {
    PathBuf::from(format!("{}.py", name.as_str().replace('.', "/")))
}

#[derive(Debug, Default, Clone)]
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

    pub fn to_state(self) -> State {
        let modules = self.0.keys().copied().collect::<Vec<_>>();
        let mut state = State::new(Box::new(self), Config::default(), true);
        state.run(&modules);
        state
    }
}

impl Loader for TestEnv {
    fn load(&self, name: ModuleName) -> (LoadResult, ErrorStyle) {
        let loaded = if let Some((path, contents)) = self.0.get(&name) {
            match contents {
                Ok(contents) => {
                    // TODO(grievejia): Properly model paths for in-memory sources
                    LoadResult::Loaded(ModulePath::filesystem(path.to_owned()), contents.to_owned())
                }
                Err(err) => LoadResult::FailedToLoad(
                    ModulePath::filesystem(path.to_owned()),
                    anyhow!(err.to_owned()),
                ),
            }
        } else if let Some(contents) = lookup_test_stdlib(name) {
            LoadResult::Loaded(
                ModulePath::filesystem(default_path(name)),
                contents.to_owned(),
            )
        } else {
            LoadResult::FailedToFind(anyhow!("Module not given in test suite"))
        };
        (loaded, ErrorStyle::Immediate)
    }
}

static INIT_TRACING_ONCE: Once = Once::new();

/// Should only be used from the `testcase!` macro.
pub fn testcase_for_macro(
    mut env: TestEnv,
    contents: &str,
    file: &str,
    line: u32,
) -> anyhow::Result<()> {
    INIT_TRACING_ONCE.call_once(|| init_tracing(true, true));
    let mut start_line = line as usize + 1;
    if !env.0.is_empty() {
        start_line += 1;
    }
    env.add_with_path(
        "main",
        &format!("{}{}", "\n".repeat(start_line), contents),
        file,
    );
    // If any given test regularly takes > 10s, that's probably a bug.
    // Currently all are less than 3s in debug, even when running in parallel.
    let limit = 10;
    for _ in 0..3 {
        let start = Instant::now();
        env.clone().to_state().check_against_expectations()?;
        if start.elapsed().as_secs() <= limit {
            return Ok(());
        }
        // Give a bit of a buffer if the machine is very busy
        sleep(Duration::from_secs(limit / 2));
    }
    Err(anyhow!("Test took too long (> {limit}s)"))
}

pub fn mk_state(code: &str) -> (ModuleName, State) {
    let state = TestEnv::one("main", code).to_state();
    (ModuleName::from_str("main"), state)
}

pub fn get_class(name: &str, module_name: ModuleName, state: &State) -> Option<Class> {
    let solutions = state.get_solutions(module_name).unwrap();

    match solutions.exports.get(&KeyExport(Name::new(name))) {
        Some(Type::ClassDef(cls)) => Some(cls.clone()),
        _ => None,
    }
}
