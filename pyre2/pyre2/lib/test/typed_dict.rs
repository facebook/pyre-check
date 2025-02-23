/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_typed_dict,
    r#"
from typing import TypedDict, Mapping
class Coord(TypedDict):
    x: int
    y: int
def foo(c: Coord) -> Mapping[str, object]:
    return c
    "#,
);

testcase!(
    test_typed_dict_invalid_inheritance,
    r#"
from typing import TypedDict
class Coord(TypedDict, object):  # E: Typed dictionary definitions may only extend other typed dictionaries.
    x: int
    y: int
    "#,
);

testcase!(
    test_typed_dict_literal,
    r#"
from typing import TypedDict, NotRequired
class Coord(TypedDict):
    x: int
    y: int
    z: NotRequired[int]

c1: Coord = {"x": 1, "y": 2}
c2: Coord = {"x": 1, "y": 2, "z": 3}
c3: Coord = {"x": 1, "y": 2, "a": 4}  # E: Key `a` is not defined in TypedDict `Coord`
c4: Coord = {"x": 1, "y": "foo"}  # E: EXPECTED Literal['foo'] <: int
c5: Coord = {"x": 1}  # E: Missing required key `y` for TypedDict `Coord`
c6: Coord = {"x": 1, **{"y": 2, **{"z": 3}}}
d: dict[str, int] = {}
c7: Coord = {"x": 1, **d}  # E: EXPECTED dict[str, int] <: TypedDict[Coord]

def foo(c: Coord) -> None:
    pass
foo({"x": 1, "y": 2})
    "#,
);

testcase!(
    test_typed_dict_callable,
    r#"
from typing import TypedDict

class Movie(TypedDict):
    name: str
    year: int
m = Movie(name='Blade Runner', year=1982)
    "#,
);

testcase!(
    test_typed_dict_readonly,
    r#"
from typing import TypedDict, ReadOnly
class Coord(TypedDict):
    x: int
    y: ReadOnly[int]
def foo(c: Coord) -> None:
    c["x"] = 1
    c["x"] = "foo"  # E: Expected int, got Literal['foo']
    c["y"] = 3  # E: Key `y` in TypedDict `Coord` is read-only
    c["z"] = 4  # E: TypedDict `Coord` does not have key `z`
    "#,
);

testcase!(
    test_typed_dict_metaclass,
    r#"
from enum import EnumMeta
from typing import TypedDict
class Coord(TypedDict, metaclass=EnumMeta):  # E: Typed dictionary definitions may not specify a metaclass.
    x: int
    y: int
    "#,
);

testcase!(
    test_typed_dict_iterate,
    r#"
from typing import TypedDict, assert_type
class Coord(TypedDict):
    x: int
    y: int
def foo(c: Coord) -> None:
    for x in Coord:  # E: Type `type[Coord]` is not iterable
        pass
    for x in c:
        assert_type(x, str)
    "#,
);

testcase!(
    test_typed_dict_generic,
    r#"
from typing import TypedDict
class Coord[T](TypedDict):
    x: T
    y: T
def foo(c: Coord[int]):
    x: int = c["x"]
    y: str = c["y"]  # E: EXPECTED int <: str
    "#,
);

testcase!(
    test_typed_dict_initialized_field,
    r#"
from typing import TypedDict
class Coord(TypedDict):
    x: int
    y: int = 2  # E: TypedDict item `y` may not be initialized.
    "#,
);

testcase!(
    test_typed_dict_access,
    r#"
from typing import TypedDict, Literal, assert_type
class Coord(TypedDict):
    x: int
    y: str
    z: bool
def foo(c: Coord, key: str, key2: Literal["x", "y"]):
    x: int = c["x"]
    x2: int = c.x  # E: Object of class `Mapping` has no attribute `x`
    x3: int = c[key]  # E: Invalid key for TypedDict `Coord`, got `str`
    x4: int = c["aaaaaa"]  # E: TypedDict `Coord` does not have key `aaaaaa`
    assert_type(c[key2], int | str)
    "#,
);

testcase!(
    test_typed_dict_functional,
    r#"
from typing import TypedDict
Coord = TypedDict("Coord", { "x": int, " illegal ": int })
c: Coord = {"x": 1, " illegal ": 2}
def test(c: Coord):
    x: int = c[" illegal "]
Invalid = TypedDict()  # E: Expected a callable, got type[TypedDict]
    "#,
);

testcase!(
    test_typed_dict_subtype,
    r#"
from typing import TypedDict
class Coord(TypedDict):
    x: int
    y: int
class Coord3D(TypedDict):
    x: int
    y: int
    z: int
class Pair(TypedDict):
    x: object
    y: object

def foo(a: Coord, b: Coord3D, c: Pair):
    coord: Coord = b
    coord2: Coord3D = a  # E: EXPECTED TypedDict[Coord] <: TypedDict[Coord3D]
    coord3: Coord = c  # E: TypedDict[Pair] <: TypedDict[Coord]
    coord4: Pair = a
    "#,
);

testcase!(
    test_typed_dict_not_required,
    r#"
from typing import TypedDict, NotRequired
class Coord(TypedDict):
    x: int
    y: int
class CoordNotRequired(TypedDict):
    x: NotRequired[int]
    y: NotRequired[int]

def foo(a: Coord, b: CoordNotRequired):
    coord: Coord = b  # E: EXPECTED TypedDict[CoordNotRequired] <: TypedDict[Coord]
    coord2: CoordNotRequired = a  # E: EXPECTED TypedDict[Coord] <: TypedDict[CoordNotRequired]
    "#,
);

testcase!(
    test_typed_dict_totality,
    r#"
from typing import TypedDict, NotRequired

class CoordXY(TypedDict, total=True):
    x: int
    y: int
class CoordZ(TypedDict, total=False):
    z: int

class Coord(CoordZ):
    x: int
    y: int
class Coord2(CoordXY, total=False):
    z: int
class Coord3(TypedDict):
    x: int
    y: int
    z: NotRequired[int]
class Coord4(TypedDict, CoordXY, CoordZ):
    pass

def foo(a: Coord, b: Coord2, c: Coord3, d: Coord4):
    coord: Coord = b
    coord = c
    coord = d
    coord2: Coord2 = a
    coord2 = c
    coord2 = d
    coord3: Coord3 = a
    coord3 = b
    coord3 = d
    coord4: Coord4 = a
    coord4 = b
    coord4 = c
    "#,
);

testcase!(
    test_typed_dict_kwargs_expansion,
    r#"
from typing import TypedDict, NotRequired
class Coord(TypedDict):
    x: int
    y: int
    z: NotRequired[int]

def f1(x: int, y: int, z: int = 1): ...
def f2(x: int, y: int, **kwargs: str): ...
def f3(x: int, y: int): ...
def f4(x: int, y: int = 2, z: int = 3): ...
def f5(x: int, y: int, z: int): ...
def f6(x: int, y: int, **kwargs: str): ...

x: Coord = {"x": 1, "y": 2}
f1(**x)
f2(**x)  # E: EXPECTED int <: str
f3(**x)  # E: Unexpected keyword argument `z`
f4(**x)
f5(**x)  # E: Expected key `z` to be required
f6(**x)  # E: EXPECTED int <: str
f1(1, **x)  # E: Multiple values for argument `x`
    "#,
);

testcase!(
    test_typed_dict_kwargs_unpack,
    r#"
from typing import TypedDict, NotRequired, Unpack, assert_type
class Coord(TypedDict):
    x: int
    y: int
    z: NotRequired[int]

class Coord2(TypedDict):
    x: int
    y: int

class Coord3(TypedDict):
    x: int
    y: int
    z: int

class Coord4(TypedDict):
    w: int
    x: int
    y: int
    z: NotRequired[int]

class Coord5(TypedDict):
    y: int
    z: NotRequired[int]

def f(**kwargs: Unpack[Coord]):
    assert_type(kwargs["x"], int)

def g(x: Coord, x2: Coord2, x3: Coord3, x4: Coord4, x5: Coord5):
    f(**x)
    f(**x2)
    f(**x3)
    f(**x4)
    f(**x5)  # E: Missing argument `x`
f(x=1, y=2)
f(x=1, y=2, z=3)
f(x=1, y=2, z=3, a=4)  # E: Unexpected keyword argument `a`
f(x="", y=2)  # E: EXPECTED Literal[''] <: int
    "#,
);

testcase!(
    test_inheritance,
    r#"
from typing import TypedDict
class A(TypedDict):
    x: int
class B(A):
    y: str
B(x=0, y='1')  # OK
B(x=0, y=1)  # E: EXPECTED Literal[1] <: str
    "#,
);

testcase!(
    test_generic_instantiation,
    r#"
from typing import TypedDict, assert_type
class C[T](TypedDict):
     x: T
assert_type(C(x=0), C[int])
assert_type(C[str](x=""), C[str])
    "#,
);

testcase!(
    test_unpacked_typed_dict_assert_type,
    r#"
from typing import TypedDict, Unpack, assert_type
class Coord(TypedDict):
    x: int
    y: int
def foo(x: Coord, **kwargs: Unpack[Coord]):
    assert_type(x, Coord)
    assert_type(kwargs, Coord)
    "#,
);

testcase_with_bug!(
    "TODO(stroxler) We should correctly account for requireness of a field in requiredness of `__init__` args",
    test_requireness_in_init,
    r#"
from typing import TypedDict, NotRequired
class D(TypedDict):
     x: int
     y: int = 5  # E: TypedDict item `y` may not be initialized.
     z: NotRequired[int]
D(x=5)  # E: Missing argument `z`
    "#,
);
