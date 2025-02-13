/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;
use crate::testcase_with_bug;

testcase_with_bug!(
    "TODO: support overloads defined in py files",
    test_py,
    r#"
from typing import overload, assert_type

@overload
def f(x: int) -> int: ...

@overload
def f(x: str) -> str: ...

def f(x):
    return x

assert_type(f(1), int)

def anywhere():
    assert_type(f(1), int)  # E: assert_type # E: EXPECTED Literal[1] <: str
    "#,
);

fn env_with_stub() -> TestEnv {
    let mut t = TestEnv::new();
    t.add_with_path(
        "foo",
        r#"
from typing import overload

@overload
def f(x: int) -> int: ...

@overload
def f(x: str) -> str: ...
    "#,
        "foo.pyi",
    );
    t
}

testcase!(
    test_pyi,
    env_with_stub(),
    r#"
from typing import assert_type
import foo
assert_type(foo.f(1), int)
    "#,
);
