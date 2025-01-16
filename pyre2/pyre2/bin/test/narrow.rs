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

testcase_with_bug!(
    "`Literal[False] | bool` should collapse to `bool`",
    test_and,
    r#"
from typing import assert_type, Literal, Never
def f(x: bool | None):
    if x is True and x is None:
        assert_type(x, Never)
    else:
        assert_type(x, Literal[False] | bool | None)
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
    test_or,
    r#"
from typing import assert_type, Literal
def f(x: bool | None):
    if x == True or x is None:
        assert_type(x, Literal[True] | None)
    else:
        assert_type(x, Literal[False])
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

testcase_with_bug!(
    "`Literal[False, True] | bool` should collapse to `bool`",
    test_not_and,
    r#"
from typing import assert_type, Literal
def f(x: bool | None):
    if not (x is True and x is None):
        assert_type(x, Literal[False, True] | bool | None)
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

testcase!(
    test_walrus_target,
    r#"
from typing import assert_type
def f() -> str | None:
    pass
if x := f():
    assert_type(x, str)
    "#,
);

testcase!(
    test_walrus_value,
    r#"
from typing import assert_type
def f(x: int | None):
    if y := x:
        assert_type(x, int)
        assert_type(y, int)
    "#,
);

testcase!(
    test_walrus_comparison,
    r#"
from typing import assert_type
def f() -> str | None:
    pass
if (x := f()) is None:
    assert_type(x, None)
    "#,
);

testcase!(
    test_match_enum_fallback,
    r#"
from typing import assert_type, Literal
from enum import Enum
class E(Enum):
    X = 1
    Y = 2
    Z = 3
def f(e: E):
    match e:
        case E.X:
            assert_type(e, Literal[E.X])
        case E.Y:
            assert_type(e, Literal[E.Y])
        case _:
            assert_type(e, Literal[E.Z])
    "#,
);

testcase!(
    test_match_or,
    r#"
from typing import assert_type, Literal
def f(e: bool | None):
    match e:
        case True | None:
            assert_type(e, Literal[True] | None)
        case _:
            assert_type(e, Literal[False])
    "#,
);

testcase!(
    test_ternary,
    r#"
from typing import assert_type
def f(x: str | None, y: int):
    z = x if x else y
    assert_type(x, str | None)
    assert_type(y, int)
    assert_type(z, str | int)
    "#,
);

testcase!(
    test_is_supertype,
    r#"
from typing import Literal, assert_type
import enum
class E(enum.Enum):
    X = 1
def f(x: Literal[E.X], y: E):
    if x is y:
        assert_type(x, Literal[E.X])
    "#,
);

testcase!(
    test_isinstance,
    r#"
from typing import assert_type
def f(x: str | int):
    if isinstance(x, str):
        assert_type(x, str)
    else:
        assert_type(x, int)
    "#,
);

testcase!(
    test_isinstance_tuple,
    r#"
from typing import assert_type
def f(x: str | int | None):
    if isinstance(x, (str, int)):
        assert_type(x, str | int)
    else:
        assert_type(x, None)
    "#,
);

testcase!(
    test_isinstance_alias,
    r#"
from typing import assert_type
X = int
def f(x: str | int):
    if isinstance(x, X):
        assert_type(x, int)
    "#,
);

testcase_with_bug!(
    "isinstance() should reject parameterized classes",
    test_isinstance_error,
    r#"
from typing import assert_type
def f(x: int | list[int]):
    if isinstance(x, list[int]):  # this should be an error
        assert_type(x, list[int])
    "#,
);

testcase!(
    test_guarded_attribute_access_and,
    r#"
class A:
    x: str
class B:
    pass
def f(x: A | B):
    return isinstance(x, A) and x.x
    "#,
);

testcase!(
    test_guarded_attribute_access_or,
    r#"
class A:
    x: str
def f(x: A | None):
    return x is None or x.x
    "#,
);

testcase!(
    test_and_chain_with_walrus,
    r#"
from typing import assert_type, Literal

class A: ...
class B: ...

x: A | B
y = isinstance(x, A) and (z := True)
assert_type(x, A | B)
assert_type(z, Literal[True])
    "#,
);

testcase!(
    test_typeguard,
    r#"
from typing import TypeGuard, assert_type
class Cat:
    color: str
class Dog:
    pass
def is_black_cat(x: Cat | Dog) -> TypeGuard[Cat]:
    return isinstance(x, Cat) and x.color == "black"
def f(x: Cat | Dog):
    if is_black_cat(x):
        assert_type(x, Cat)
    else:
        assert_type(x, Cat | Dog)
    "#,
);

testcase_with_bug!(
    "TODO",
    test_issubclass,
    r#"
from typing import assert_type
class A: ...
class B(A): ...
def f(x: type[B] | type[int]):
    if issubclass(x, A):
        assert_type(x, type[B])  # E: assert_type
    else:
        assert_type(x, type[int])  # E: assert_type
    "#,
);

testcase!(
    test_typeguard_instance_method,
    r#"
from typing import TypeGuard, assert_type
class C:
    def is_positive_int(self, x: object) -> TypeGuard[int]:
        return isinstance(x, int) and x > 0
def f(c: C, x: int | str):
    if c.is_positive_int(x):
        assert_type(x, int)
    "#,
);

testcase!(
    test_typeguard_generic_function,
    r#"
from typing import TypeGuard, assert_type
def f[T](x: object, y: T, z: T) -> TypeGuard[int]: ...
def g(x: int | str):
    if f(x, 0, 0):
        assert_type(x, int)
    "#,
);
