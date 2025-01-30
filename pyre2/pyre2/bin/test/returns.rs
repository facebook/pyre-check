/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

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
