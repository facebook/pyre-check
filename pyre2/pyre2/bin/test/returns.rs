/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_missing_return,
    r#"

def f() -> int:
    pass  # E: EXPECTED None <: int
"#,
);

testcase!(
    test_missing_return_none,
    r#"
def f() -> None:
    pass
"#,
);

testcase!(
    test_missing_return_implicit,
    r#"
from typing import assert_type

def f():
    pass
assert_type(f(), None)
"#,
);

testcase!(
    test_return_unions,
    r#"
from typing import assert_type, Literal

def f(b: bool):
    if b:
        return 1
    else:
        return "test"
assert_type(f(True), Literal['test', 1])
"#,
);

testcase!(
    test_return_some_return,
    r#"
from typing import assert_type

def f(b: bool) -> int:
    if b:
        return 1
    else:
        pass  # E: EXPECTED None <: int
"#,
);

testcase!(
    test_return_catch,
    r#"
def f(b: bool) -> int:
    try:
        return 1
    except Exception:
        return 2
"#,
);

testcase_with_bug!(
    "Should not require a return statement",
    test_return_never,
    r#"
from typing import NoReturn

def fail() -> NoReturn:
    raise Exception()

def f(b: bool) -> int:
    if b:
        return 1
    else:
        fail() # E: EXPECTED None <: int
"#,
);

testcase!(
    test_return_if_no_else_real,
    r#"
def f(b: bool) -> int:
    if b:  # E: EXPECTED None <: int
        return 1
"#,
);

testcase!(
    test_return_if_no_else_none,
    r#"
def f(b: bool) -> None:
    if b:
        return None
"#,
);
