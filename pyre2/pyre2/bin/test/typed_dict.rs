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
    x3: int = c[key]  # E: Invalid key for typed dictionary `Coord`, got `str`
    x4: int = c["aaaaaa"]  # E: Object of class `Coord` has no attribute `aaaaaa`
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
    coord2: Coord3D = a  # E: EXPECTED Coord <: Coord3D
    coord3: Coord = c  # E: Pair <: Coord
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
    coord: Coord = b  # E: EXPECTED CoordNotRequired <: Coord
    coord2: CoordNotRequired = a  # E: EXPECTED Coord <: CoordNotRequired
    "#,
);
