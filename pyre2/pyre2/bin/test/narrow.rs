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
    test_truthy_falsy,
    r#"
from typing import assert_type, Literal
def f(x: str | None, y: bool):
    if x:
        assert_type(x, str)
    if y:
        assert_type(y, Literal[True])
    else:
        assert_type(y, Literal[False])
    "#,
);

testcase!(
    test_eq,
    r#"
from typing import assert_type
def f(x: str | None):
    if x == None:
        assert_type(x, None)
    "#,
);

testcase!(
    test_neq,
    r#"
from typing import assert_type
def f(x: str | None):
    if x != None:
        assert_type(x, str)
    "#,
);

testcase!(
    test_is_not,
    r#"
from typing import assert_type
def f(x: str | None):
    if x is not None:
        assert_type(x, str)
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

testcase!(
    test_is_subtype,
    r#"
from typing import assert_type
class A: pass
class B(A): pass
def f(x: type[A]):
    if x is B:
        assert_type(x, type[B])
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

testcase!(
    test_is_not_bool_literal,
    r#"
from typing import assert_type, Literal, Never
def f1(x: bool):
    if x is not True:
        assert_type(x, Literal[False])
def f2(x: Literal[True] | str):
    if x is not True:
        assert_type(x, str)
    "#,
);

testcase!(
    test_is_not_enum_literal,
    r#"
from typing import assert_type, Literal
import enum
class E(enum.Enum):
    X = 1
    Y = 2
def f1(x: Literal[E.X, E.Y]):
    if x is not E.X:
        assert_type(x, Literal[E.Y])
def f2(x: E | int):
    if x is not E.X:
        assert_type(x, Literal[E.Y] | int)
    "#,
);

testcase!(
    test_tri_enum,
    r#"
from typing import assert_type, Literal
import enum
class E(enum.Enum):
    X = 1
    Y = 2
    Z = 3
def f(x: E):
    if x is E.X:
       assert_type(x, Literal[E.X])
    elif x is E.Y:
       assert_type(x, Literal[E.Y])
    else:
       assert_type(x, Literal[E.Z])
    "#,
);

testcase!(
    test_is_classdef,
    r#"
from typing import assert_type
class A: pass
class B: pass
def f1(x: type[A] | type[B]):
    if x is A:
        assert_type(x, type[A])
    else:
        # Note that we cannot narrow to `type[B]` here, as `type` is covariant and `x` may be a
        # subtype of `A`.
        assert_type(x, type[A] | type[B])
    "#,
);

testcase!(
    test_and,
    r#"
from typing import assert_type, Never
def f(x: bool | None):
    if x is True and x is None:
        assert_type(x, Never)
    else:
        assert_type(x, bool | None)
    "#,
);

testcase!(
    test_and_multiple_vars,
    r#"
from typing import assert_type, Literal
def f(x: bool | None, y: bool | None):
    if x is True and y is False:
        assert_type(x, Literal[True])
        assert_type(y, Literal[False])
    "#,
);

testcase!(
    test_elif,
    r#"
from typing import assert_type
def f(x: str | None, y: int | None):
    if x is None:
        assert_type(x, None)
        assert_type(y, int | None)
    elif y is None:
        assert_type(x, str)
        assert_type(y, None)
    else:
        assert_type(x, str)
        assert_type(y, int)
    "#,
);

testcase!(
    test_not,
    r#"
from typing import assert_type
def f(x: str | None):
    if not x is None:
        assert_type(x, str)
    else:
        assert_type(x, None)
    "#,
);

testcase!(
    test_not_and,
    r#"
from typing import assert_type
def f(x: bool | None):
    if not (x is True and x is None):
        assert_type(x, bool | None)
    "#,
);

testcase!(
    test_assert,
    r#"
from typing import assert_type
def f(x: str | None):
    assert x is not None
    assert_type(x, str)
    "#,
);

testcase!(
    test_while_else,
    r#"
from typing import assert_type
def f() -> str | None: ...
x = f()
while x is None:
    assert_type(x, None)
    x = f()
    assert_type(x, str | None)
else:
    assert_type(x, str)
assert_type(x, str)
    "#,
);

testcase!(
    test_while_break,
    r#"
from typing import assert_type
def f() -> str | None: ...
x = f()
while x is None:
    break
assert_type(x, str | None)
    "#,
);

testcase!(
    test_while_break_else,
    r#"
from typing import assert_type
def f() -> str | None: ...
x = f()
while x is None:
    if f():
        break
else:
    assert_type(x, str)
assert_type(x, str | None)
    "#,
);

testcase_with_bug!(
    "Unwanted EXPECTED error",
    test_while_overwrite,
    r#"
from typing import assert_type, Literal
def f() -> str | None: ...
x = f()
while x is None:  # E: EXPECTED None <: Literal[42] | str
    if f():
        x = 42
        break
assert_type(x, Literal[42] | str)
    "#,
);

testcase!(
    test_nested_function,
    r#"
from typing import assert_type
def foo(x: int | None) -> None:
    def include():
        if x is not None:
            assert_type(x, int)
    "#,
);

testcase!(
    test_multiple_is,
    r#"
from typing import assert_type, Never
def f(x: bool | None, y: bool | None):
    if x is None is None:
        assert_type(x, None)
    if y is None is True:
        assert_type(y, Never)
    "#,
);

testcase!(
    test_class_body,
    r#"
from typing import assert_type
def f() -> str | None: ...
x = f()
class C:
    if x is None:
        assert_type(x, None)
    "#,
);
