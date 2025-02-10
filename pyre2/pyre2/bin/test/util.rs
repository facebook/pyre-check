/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Once;
use std::thread::sleep;
use std::time::Duration;
use std::time::Instant;

use anyhow::anyhow;
use dupe::Dupe;
use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;

use crate::binding::binding::KeyExport;
use crate::config::Config;
use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::state::State;
use crate::test::stdlib::lookup_test_stdlib;
use crate::types::class::Class;
use crate::types::types::Type;
use crate::util::trace::init_tracing;

#[macro_export]
macro_rules! testcase {
    ($name:ident, $imports:expr, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!())
        }
    };
    ($name:ident, $contents:literal,) => {
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
    ($explanation:literal, $name:ident, $imports:expr, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!() + 1)
        }
    };
    ($explanation:literal, $name:ident, $contents:literal,) => {
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

fn default_path(module: ModuleName) -> PathBuf {
    PathBuf::from(format!("{}.py", module.as_str().replace('.', "/")))
}

#[derive(Debug, Default, Clone)]
pub struct TestEnv(SmallMap<ModuleName, (ModulePath, Option<String>)>);

impl TestEnv {
    pub fn new() -> Self {
        // We aim to init the tracing before now, but if not, better now than never
        test_init_tracing();
        Self::default()
    }

    pub fn add_with_path(&mut self, name: &str, code: &str, path: &str) {
        self.0.insert(
            ModuleName::from_str(name),
            (
                ModulePath::memory(PathBuf::from(path)),
                Some(code.to_owned()),
            ),
        );
    }

    pub fn add(&mut self, name: &str, code: &str) {
        let module_name = ModuleName::from_str(name);
        let relative_path = ModulePath::memory(default_path(module_name));
        self.0
            .insert(module_name, (relative_path, Some(code.to_owned())));
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

    pub fn add_real_path(&mut self, name: &str, path: PathBuf) {
        let module_name = ModuleName::from_str(name);
        self.0
            .insert(module_name, (ModulePath::filesystem(path), None));
    }

    pub fn config() -> Config {
        Config::default()
    }

    pub fn to_state(self) -> (State, impl Fn(&str) -> Handle) {
        let config = Self::config();
        let loader = LoaderId::new(self.clone());
        let handles = self
            .0
            .into_iter()
            .map(|(x, (path, _))| Handle::new(x, path, config.dupe(), loader.dupe()))
            .collect();
        let mut state = State::new(true);
        state.run(handles);
        (state, move |module| {
            let name = ModuleName::from_str(module);
            Handle::new(
                name,
                loader.find(name).unwrap().0,
                Self::config(),
                loader.dupe(),
            )
        })
    }
}

impl Loader for TestEnv {
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        let style = ErrorStyle::Immediate;
        if let Some((path, _)) = self.0.get(&module) {
            Ok((path.dupe(), style))
        } else if lookup_test_stdlib(module).is_some() {
            Ok((ModulePath::memory(default_path(module)), style))
        } else {
            Err(FindError::new(anyhow!("Module not given in test suite")))
        }
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        // This function involves scanning all paths to find what matches.
        // Not super efficient, but fine for tests, and we don't have many modules.
        let memory_path = ModulePath::memory(path.to_owned());
        for (p, contents) in self.0.values() {
            if p == &memory_path
                && let Some(c) = contents
            {
                return Some(Arc::new(c.clone()));
            }
        }
        Some(Arc::new(
            lookup_test_stdlib(ModuleName::from_str(path.file_stem()?.to_str()?))?.to_owned(),
        ))
    }
}

static INIT_TRACING_ONCE: Once = Once::new();

pub fn test_init_tracing() {
    INIT_TRACING_ONCE.call_once(|| init_tracing(true, true));
}

/// Should only be used from the `testcase!` macro.
pub fn testcase_for_macro(
    mut env: TestEnv,
    contents: &str,
    file: &str,
    line: u32,
) -> anyhow::Result<()> {
    test_init_tracing();
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
        env.clone().to_state().0.check_against_expectations()?;
        if start.elapsed().as_secs() <= limit {
            return Ok(());
        }
        // Give a bit of a buffer if the machine is very busy
        sleep(Duration::from_secs(limit / 2));
    }
    Err(anyhow!("Test took too long (> {limit}s)"))
}

pub fn mk_state(code: &str) -> (Handle, State) {
    let (state, handle) = TestEnv::one("main", code).to_state();
    (handle("main"), state)
}

pub fn get_class(name: &str, handle: &Handle, state: &State) -> Option<Class> {
    let solutions = state.get_solutions(handle).unwrap();

    match solutions.exports.get(&KeyExport(Name::new(name))) {
        Some(Type::ClassDef(cls)) => Some(cls.clone()),
        _ => None,
    }
}
