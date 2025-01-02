/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_is,
    r#"
from typing import assert_type
def f(x: str | None):
    if x is None:
        assert_type(x, None)
    assert_type(x, str | None)
    "#,
);

testcase!(
    test_if_else,
    r#"
from typing import assert_type
def f(x: str | None):
    if x is None:
        assert_type(x, None)
    else:
        assert_type(x, str)
    "#,
);

// TODO: fix the assert_type bug that is causing this assertion to fail.
testcase_with_bug!(
    test_is_subtype,
    r#"
from typing import assert_type
class A: pass
class B(A): pass
def f(x: type[A]):
    if x is B:
        assert_type(x, type[B])  # E: assert_type(type[B], type[B]) failed
    "#,
);

testcase!(
    test_is_never,
    r#"
from typing import assert_type, Never
def f(x: str):
    if x is None:
        assert_type(x, Never)
    "#,
);
