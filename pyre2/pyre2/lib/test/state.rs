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

use crate::metadata::PythonVersion;
use crate::metadata::RuntimeMetadata;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::require::Require;
use crate::state::state::State;
use crate::state::subscriber::TestSubscriber;
use crate::test::util::init_test;
use crate::test::util::TestEnv;
use crate::util::lock::Mutex;
use crate::util::prelude::SliceExt;

#[test]
fn test_multiple_config() {
    let linux = RuntimeMetadata::new(PythonVersion::default(), "linux".to_owned());
    let windows = RuntimeMetadata::new(PythonVersion::default(), "windows".to_owned());

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
        "import lib; x: str = lib.value  # E: `Literal[42]` is not assignable to `str`",
    );
    let mut state = State::new();
    let loader = LoaderId::new(test_env);

    let f = |name: &str, config: &RuntimeMetadata| {
        let name = ModuleName::from_str(name);
        let path = loader.find_import(name).unwrap();
        (
            Handle::new(name, path, config.dupe(), loader.dupe()),
            Require::Everything,
        )
    };

    state.run(
        &[
            f("linux", &linux),
            f("windows", &windows),
            f("main", &linux),
        ],
        Require::Exports,
        None,
    );
    state.check_against_expectations().unwrap();
}

#[test]
fn test_multiple_path() {
    const LIB_PYI: &str = "x: int";
    const LIB_PY: &str = "x: str = 1  # E: `Literal[1]` is not assignable to `str`";
    const MAIN_PYI: &str =
        "import lib; y: list[int] = lib.x  # E: `int` is not assignable to `list[int]`";
    const MAIN_PY: &str =
        "import lib; y: list[str] = lib.x  # E: `int` is not assignable to `list[str]`";

    const FILES: &[(&str, &str, &str)] = &[
        ("lib", "lib.pyi", LIB_PYI),
        ("lib", "lib.py", LIB_PY),
        ("main", "main.pyi", MAIN_PYI),
        ("main", "main.py", MAIN_PY),
    ];

    #[derive(Debug)]
    struct Load(TestEnv);

    impl Loader for Load {
        fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
            match FILES.iter().find(|x| x.0 == module.as_str()) {
                Some((_, path, _)) => Ok(ModulePath::memory(PathBuf::from(path))),
                None => self.0.find_import(module),
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

    let mut state = State::new();
    state.run(
        &FILES.map(|(name, path, _)| {
            (
                Handle::new(
                    ModuleName::from_str(name),
                    ModulePath::memory(PathBuf::from(path)),
                    TestEnv::config(),
                    loader.dupe(),
                ),
                Require::Everything,
            )
        }),
        Require::Exports,
        None,
    );
    state.print_errors();
    state.check_against_expectations().unwrap();
    assert_eq!(state.collect_errors().len(), 3);
}

#[derive(Default, Clone, Dupe, Debug)]
struct IncrementalData(Arc<Mutex<SmallMap<ModuleName, Arc<String>>>>);

impl Loader for IncrementalData {
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        match self.0.lock().get(&module) {
            Some(_) => Ok(ModulePath::memory(PathBuf::from(module.as_str()))),
            None => TestEnv::new().find_import(module),
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
        init_test();
        let data = IncrementalData::default();
        let loader = LoaderId::new(data.dupe());
        let state = State::new();
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
            RuntimeMetadata::default(),
            self.loader.dupe(),
        )
    }

    /// Run a check. Expect recompute things to have changed.
    fn check(&mut self, want: &[&str], recompute: &[&str]) {
        let subscriber = TestSubscriber::new();
        self.state.run(
            &want.map(|x| (self.handle(x), Require::Everything)),
            Require::Exports,
            Some(Box::new(subscriber.dupe())),
        );
        self.state.print_errors();
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

    // We stop depending on `foo`, so no longer have to recompute it even though it is dirty.
    // However, our current state algorithm does so anyway as it can be cheaper to compute
    // everything than do careful graph traversal.
    i.set("foo", "x = True");
    i.set("main", "x = 7");
    i.check(&["main"], &["main", "foo"]); // `foo` is not required here
    i.set("main", "import foo; x = foo.x # still");
    i.check(&["main"], &["main"]); // `foo` is required by this point
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

/// Check that the interface is consistent as we change things.
fn test_interface_consistent(code: &str, broken: bool) {
    let mut i = Incremental::new();
    i.set("main", code);
    i.check(&["main"], &["main"]);
    let base = i.state.get_solutions(&i.handle("main")).unwrap();

    i.set("main", &format!("{code} # after"));
    i.check(&["main"], &["main"]);
    let suffix = i.state.get_solutions(&i.handle("main")).unwrap();

    i.set("main", &format!("# before\n{code}"));
    i.check(&["main"], &["main"]);
    let prefix = i.state.get_solutions(&i.handle("main")).unwrap();

    let same = base.first_difference(&base);
    let suffix = suffix.first_difference(&base);
    let prefix = prefix.first_difference(&base);
    if !broken {
        assert!(same.is_none(), "{code:?} led to {same:?}");
        assert!(suffix.is_none(), "{code:?} led to {suffix:?}");
        assert!(prefix.is_none(), "{code:?} led to {prefix:?}");
    } else {
        assert!(
            same.is_some() || suffix.is_some() || prefix.is_some(),
            "{code:?} now works"
        );
    }
}

#[test]
fn test_interfaces() {
    #[allow(dead_code)]
    const BROKEN: bool = true;

    test_interface_consistent("x: int = 1\ndef f(y: bool) -> list[str]: return []", false);

    // Important to have a class with a field, as those also have positions
    test_interface_consistent("class X: y: int", false);

    // These should not change, but do because the quality algorithm doesn't deal
    // well with Forall.
    test_interface_consistent("def f[X](x: X) -> X: ...", false);

    // These should not change, but do because the quality algorithm doesn't deal
    // well with Forall.
    test_interface_consistent(
        "
from typing import TypeVar, Generic
T = TypeVar('T')
class C(Generic[T]): pass",
        false,
    );

    // Another failing example
    test_interface_consistent("class C[T]: x: T", false);

    // Another failing example
    test_interface_consistent(
        "
from typing import TypeVar, Generic
T = TypeVar('T')
class C(Generic[T]): x: T",
        false,
    );

    test_interface_consistent(
        "
from typing import TypeVar, Generic
T = TypeVar('T')
class C(Generic[T]): pass
class D(C[T]): pass",
        false,
    );

    test_interface_consistent(
        "
from typing import TypeVar
class C: pass
T = TypeVar('T', bound=C)",
        false,
    );

    test_interface_consistent(
        "
class C:
    def __init__[R](self, field: R) -> None:
        self.field = R
",
        false,
    );
}

#[test]
fn test_change_require() {
    let t = TestEnv::one("foo", "x: str = 1");
    let mut state = State::new();
    let handle = Handle::new(
        ModuleName::from_str("foo"),
        ModulePath::memory(PathBuf::from("foo")),
        TestEnv::config(),
        LoaderId::new(t),
    );
    state.run(&[(handle.dupe(), Require::Exports)], Require::Exports, None);
    assert_eq!(state.count_errors(), 0);
    assert!(state.get_bindings(&handle).is_none());
    state.run(&[(handle.dupe(), Require::Errors)], Require::Exports, None);
    assert_eq!(state.count_errors(), 1);
    assert!(state.get_bindings(&handle).is_none());
    state.run(
        &[(handle.dupe(), Require::Everything)],
        Require::Exports,
        None,
    );
    assert_eq!(state.count_errors(), 1);
    assert!(state.get_bindings(&handle).is_some());
}
