/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tests of the `State` object.

use dupe::Dupe;

use crate::config::Config;
use crate::config::PythonVersion;
use crate::module::module_name::ModuleName;
use crate::state::handle::Handle;
use crate::state::loader::LoaderId;
use crate::state::state::State;
use crate::test::util::TestEnv;

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
    state.run(vec![
        Handle::new(ModuleName::from_str("linux"), linux.dupe(), loader.dupe()),
        Handle::new(ModuleName::from_str("windows"), windows, loader.dupe()),
        Handle::new(ModuleName::from_str("main"), linux, loader),
    ]);
    state.check_against_expectations().unwrap();
}
