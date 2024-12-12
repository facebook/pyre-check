/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

fn sys() -> TestEnv {
    TestEnv::one(
        "sys",
        r#"
version_info = (3, 12)
platform = "linux"
"#,
    )
}

testcase!(
    test_sys_version,
    sys(),
    r#"
from typing import assert_type
import sys
if sys.version_info >= (3, 12):
    X = str
else:
    X = int
assert_type(X(), str)

if sys.version_info == (3, 7):
    Y = str
else:
    Y = int
assert_type(Y(), int)

if sys.version_info < (3, 11, 3):
    Z = str
else:
    Z = int
assert_type(Z(), int)
"#,
);

testcase!(
    test_class_under_version,
    sys(),
    r#"
from typing import assert_type
import sys
if sys.version_info >= (3, 10):
    class Bar:
        def magic(self) -> Foo:
            return Foo()
    class Foo: ...

    assert_type(Bar().magic(), Foo)
"#,
);
