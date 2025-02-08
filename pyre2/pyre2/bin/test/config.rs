/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_sys_version,
    r#"
from typing import assert_type
import sys
if sys.version_info >= (3, 0):
    X = str
else:
    X = int
assert_type(X(), str)

if sys.version_info == (2, 7):
    Y = str
else:
    Y = int
assert_type(Y(), int)

if sys.version_info < (3, 0, 0):
    Z = str
else:
    Z = int
assert_type(Z(), int)
"#,
);

testcase!(
    test_class_under_version,
    r#"
from typing import assert_type
import sys
if sys.version_info >= (3, 0):
    class Bar:
        def magic(self) -> Foo:
            return Foo()
    class Foo: ...

    assert_type(Bar().magic(), Foo)
"#,
);
