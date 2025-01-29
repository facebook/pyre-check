/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

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
    for x in Coord:  # E: Type `type[Coord]` (a `TypedDict` class object) is not iterable
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
from typing import TypedDict
class Coord(TypedDict):
    x: int
    y: int
def foo(c: Coord, key: str):
    x: int = c["x"]
    x2: int = c.x  # E: Object of class `Mapping` has no attribute `x`
    x3: int = c[key]  # E: Invalid key for TypedDict `Coord`, got `str`
    x4: int = c["aaaaaa"]  # E: TypedDict `Coord` does not have key `aaaaaa`
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
