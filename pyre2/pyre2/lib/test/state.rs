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
use starlark_map::small_map::SmallMap;

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
use crate::state::subscriber::TestSubscriber;
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

    state.run(
        &[
            f("linux", &linux),
            f("windows", &windows),
            f("main", &linux),
        ],
        None,
    );
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
    state.run(
        &FILES.map(|(name, path, _)| {
            Handle::new(
                ModuleName::from_str(name),
                ModulePath::memory(PathBuf::from(path)),
                TestEnv::config(),
                loader.dupe(),
            )
        }),
        None,
    );
    state.print_errors();
    state.check_against_expectations().unwrap();
    assert_eq!(state.collect_errors().len(), 3);
}

#[derive(Default, Clone, Dupe, Debug)]
struct IncrementalData(Arc<Mutex<SmallMap<ModuleName, Arc<String>>>>);

impl Loader for IncrementalData {
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        match self.0.lock().get(&module) {
            Some(_) => Ok((
                ModulePath::memory(PathBuf::from(module.as_str())),
                ErrorStyle::Delayed,
            )),
            None => TestEnv::new().find(module),
        }
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        match self
            .0
            .lock()
            .get(&ModuleName::from_str(path.to_str().unwrap()))
        {
            Some(x) => Some(x.dupe()),
            None => TestEnv::new().load_from_memory(path),
        }
    }
}

/// Helper for writing incrementality tests.
struct Incremental {
    data: IncrementalData,
    loader: LoaderId,
    state: State,
}

impl Incremental {
    fn new() -> Self {
        let data = IncrementalData::default();
        let loader = LoaderId::new(data.dupe());
        let state = State::new(true);
        Self {
            data,
            loader,
            state,
        }
    }

    /// Change this file to these contents, expecting this number of errors.
    fn set(&mut self, file: &str, content: &str) {
        self.data
            .0
            .lock()
            .insert(ModuleName::from_str(file), Arc::new(content.to_owned()));
        self.state
            .invalidate_memory(self.loader.dupe(), &[PathBuf::from(file)]);
    }

    fn handle(&self, x: &str) -> Handle {
        Handle::new(
            ModuleName::from_str(x),
            ModulePath::memory(PathBuf::from(x)),
            Config::default(),
            self.loader.dupe(),
        )
    }

    /// Run a check. Expect recompute things to have changed.
    fn check(&mut self, want: &[&str], recompute: &[&str]) {
        let subscriber = TestSubscriber::new();
        self.state.run(
            &want.map(|x| self.handle(x)),
            Some(Box::new(subscriber.dupe())),
        );
        self.state.check_against_expectations().unwrap();

        let mut recompute = recompute.map(|x| (*x).to_owned());
        recompute.sort();

        let mut changed = Vec::new();
        for (x, (count, _)) in subscriber.finish() {
            let m = x.module();
            if self.data.0.lock().contains_key(&m) {
                for _ in 0..count {
                    changed.push(m.as_str().to_owned());
                }
            }
        }
        changed.sort();
        assert_eq!(recompute, changed);
    }
}

#[test]
fn test_in_memory_updated_content_recheck() {
    let mut i = Incremental::new();
    i.set("main", "unbound_name # E:");
    i.check(&["main"], &["main"]);
    i.set("main", "bound_name = 3");
    i.check(&["main"], &["main"]);
}

#[test]
fn test_incremental_minimal_recompute() {
    let mut i = Incremental::new();
    i.set("main", "import foo; x = foo.x");
    i.set("foo", "x = 7");
    i.check(&["main"], &["main", "foo"]);
    i.set("foo", "x = 'test'");
    i.check(&["main"], &["main", "foo"]);
    i.set("foo", "x = 'test' # still");
    i.check(&["main"], &["foo"]);
    i.set("main", "import foo; x = foo.x # still");
    i.check(&["main"], &["main"]);

    // Now check that we change foo, but stop depending on it, so won't recompute.
    i.set("foo", "x = True");
    i.set("main", "x = 7");
    i.check(&["main"], &["main"]);
    i.set("main", "import foo; x = foo.x # still");
    i.check(&["main"], &["main", "foo"]);
}

#[test]
fn test_incremental_cyclic() {
    let mut i = Incremental::new();
    i.set("foo", "import bar; x = 1; y = bar.x");
    i.set("bar", "import foo; x = True; y = foo.x");
    i.check(&["foo"], &["foo", "bar"]);
    i.set("foo", "import bar; x = 1; y = bar.x # still");
    i.check(&["foo"], &["foo"]);
    i.set("foo", "import bar; x = 'test'; y = bar.x");
    i.check(&["foo"], &["foo", "foo", "bar"]);
}

#[test]
fn test_incremental_class() {
    // Class has equality with ArcId, so need to make sure they have equality
    let mut i = Incremental::new();
    i.set("main", "import foo; x = foo.X()");
    i.set("foo", "class X: pass");
    i.check(&["main"], &["main", "foo"]);
    i.set("foo", "class X: pass # still");
    // TODO: should not recompute main
    i.check(&["main"], &["foo", "main"]);
    i.set("foo", "# new range\nclass X: pass");
    // TODO: should not recompute main
    i.check(&["main"], &["foo", "main"]);
}
