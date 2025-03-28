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

testcase!(
    test_bool_literals,
    r#"
from typing import assert_type
if True:
    X = str
else:
    X = int
assert_type(X(), str)

if False:
    Y = str
else:
    Y = int
assert_type(Y(), int)

if not(False):
    X = str
else:
    X = int
assert_type(X(), str)

if not(True):
    Y = str
else:
    Y = int
assert_type(Y(), int)
"#,
);

testcase!(
    test_typechecking_constant,
    r#"
import typing
from typing import TYPE_CHECKING, assert_type
if TYPE_CHECKING:
    X0 = str
else:
    X0 = int
assert_type(X0(), str)

if typing.TYPE_CHECKING:
    X1 = str
else:
    X1 = int
assert_type(X1(), str)

if not TYPE_CHECKING:
    Y0 = str
else:
    Y0 = int
assert_type(Y0(), int)

if not typing.TYPE_CHECKING:
    Y1 = str
else:
    Y1 = int
assert_type(Y1(), int)
"#,
);
