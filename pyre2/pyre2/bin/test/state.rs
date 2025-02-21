/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tests of the `State` object.

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;

use crate::config::Config;
use crate::config::PythonVersion;
use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::state::State;
use crate::test::util::TestEnv;
use crate::util::lock::Mutex;
use crate::util::prelude::SliceExt;

#[test]
fn test_multiple_config() {
    let linux = Config::new(PythonVersion::default(), "linux".to_owned());
    let windows = Config::new(PythonVersion::default(), "windows".to_owned());

    const LIB: &str = r#"
import sys
if sys.platform == "linux":
    value = 42
else:
    value = "hello"
"#;
    let mut test_env = TestEnv::new();
    test_env.add("lib", LIB);
    test_env.add("windows", "import lib; x: str = lib.value");
    test_env.add("linux", "import lib; x: int = lib.value");
    test_env.add(
        "main",
        "import lib; x: str = lib.value  # E: EXPECTED Literal[42] <: str",
    );
    let mut state = State::new(true);
    let loader = LoaderId::new(test_env);

    let f = |name: &str, config: &Config| {
        let name = ModuleName::from_str(name);
        let path = loader.find(name).unwrap().0;
        Handle::new(name, path, config.dupe(), loader.dupe())
    };

    state.run(&[
        f("linux", &linux),
        f("windows", &windows),
        f("main", &linux),
    ]);
    state.check_against_expectations().unwrap();
}

#[test]
fn test_multiple_path() {
    const LIB_PYI: &str = "x: int";
    const LIB_PY: &str = "x: str = 1  # E: EXPECTED Literal[1] <: str";
    const MAIN_PYI: &str = "import lib; y: list[int] = lib.x  # E: EXPECTED int <: list[int]";
    const MAIN_PY: &str = "import lib; y: list[str] = lib.x  # E: EXPECTED int <: list[str]";

    const FILES: &[(&str, &str, &str)] = &[
        ("lib", "lib.pyi", LIB_PYI),
        ("lib", "lib.py", LIB_PY),
        ("main", "main.pyi", MAIN_PYI),
        ("main", "main.py", MAIN_PY),
    ];

    #[derive(Debug)]
    struct Load(TestEnv);

    impl Loader for Load {
        fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
            match FILES.iter().find(|x| x.0 == module.as_str()) {
                Some((_, path, _)) => {
                    Ok((ModulePath::memory(PathBuf::from(path)), ErrorStyle::Delayed))
                }
                None => self.0.find(module),
            }
        }

        fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
            match FILES.iter().find(|x| x.1 == path.to_str().unwrap()) {
                Some((_, _, content)) => Some(Arc::new((*content).to_owned())),
                None => self.0.load_from_memory(path),
            }
        }
    }

    let loader = LoaderId::new(Load(TestEnv::new()));

    let mut state = State::new(true);
    state.run(&FILES.map(|(name, path, _)| {
        Handle::new(
            ModuleName::from_str(name),
            ModulePath::memory(PathBuf::from(path)),
            TestEnv::config(),
            loader.dupe(),
        )
    }));
    state.print_errors();
    state.check_against_expectations().unwrap();
    assert_eq!(state.collect_errors().len(), 3);
}

#[test]
fn test_in_memory_updated_content_recheck() {
    #[derive(Debug)]
    struct Load(Arc<Mutex<TestEnv>>);

    impl Loader for Load {
        fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
            self.0.lock().find(module)
        }

        fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
            self.0.lock().load_from_memory(path)
        }
    }

    let test_env = {
        let mut test_env = TestEnv::new();
        test_env.add("main", "unbound_name");
        Arc::new(Mutex::new(test_env))
    };
    let load = Load(test_env.dupe());
    let loader = LoaderId::new(load);

    let mut state = State::new(true);
    state.run(&[Handle::new(
        ModuleName::from_str("main"),
        ModulePath::memory(PathBuf::from("main.py")),
        TestEnv::config(),
        loader.dupe(),
    )]);
    assert_eq!(state.collect_errors().len(), 1);
    test_env.lock().add("main", "bound_name = 3");
    state.invalidate_memory(loader.dupe(), &[PathBuf::from("main.py")]);
    state.run(&[Handle::new(
        ModuleName::from_str("main"),
        ModulePath::memory(PathBuf::from("main.py")),
        TestEnv::config(),
        loader.dupe(),
    )]);
    assert_eq!(state.collect_errors().len(), 0);
}
