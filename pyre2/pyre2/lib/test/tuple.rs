/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_tuple,
    r#"
from typing import assert_type, Literal

x = (1, "2")
assert_type(x, tuple[Literal[1], Literal["2"]])

y: tuple[int, Literal["3"]] = (1, "3")
"#,
);

testcase!(
    test_index_literal,
    r#"
from typing import assert_type, Literal

x = (1, "2")
assert_type(x[0], Literal[1])
assert_type(x[1], Literal["2"])
assert_type(x[-2], Literal[1])
assert_type(x[-1], Literal["2"])
"#,
);

testcase!(
    test_invalid_ellipsis,
    r#"
from typing import assert_type, Any
x1: tuple[int, ...]  # OK
x2: tuple[...]  # E: Invalid position for `...`
x3: tuple[int, ..., ...]  # E: Invalid position for `...`
x4: tuple[int, ..., int]  # E: Invalid position for `...`
x5: tuple[int, int, ...]  # E: Invalid position for `...`
x6: tuple[..., int]  # E: Invalid position for `...`
x7: tuple[*tuple[int], ...]  # E: `...` cannot be used with an unpacked TypeVarTuple or tuple
assert_type(x2, tuple[Any, ...])
assert_type(x3, tuple[Any, ...])
assert_type(x4, tuple[Any, ...])
assert_type(x5, tuple[Any, ...])
assert_type(x6, tuple[Any, ...])
assert_type(x7, tuple[Any, ...])
"#,
);

testcase!(
    test_index,
    r#"
from typing import assert_type

def foo(x: tuple[int, str], y: tuple[int, ...], z: tuple[int, *tuple[str, ...], bool], idx: int) -> None:
    assert_type(x[idx], int | str)
    assert_type(y[idx], int)
    assert_type(z[idx], bool | int | str)
"#,
);

testcase!(
    test_empty_tuple,
    r#"
from typing import assert_type
assert_type((), tuple[()])
"#,
);

testcase!(
    test_unparameterized,
    r#"
from typing import assert_type, Any, Tuple
def foo(x: tuple, y: Tuple) -> None:
    assert_type(x, tuple[Any, ...])
    assert_type(y, tuple[Any, ...])
"#,
);

testcase!(
    test_unpack_index_out_of_bounds,
    r#"
def test(x: tuple[int]) -> None:
  y, z = x  # E: Cannot unpack
"#,
);

testcase!(
    test_unpack_in_literal,
    r#"
from typing import Any, assert_type, Literal
def test(x: tuple[int, ...], y: str) -> None:
  assert_type(("foo", *(1, 1)), tuple[Literal['foo'], Literal[1], Literal[1]])
  assert_type((1, *x, 2), tuple[Literal[1], *tuple[int, ...], Literal[2]])
  assert_type((1, *x, *x, 3), tuple[Literal[1], *tuple[int, ...], Literal[3]])
  assert_type((1, *x, y, *x, 3), tuple[Literal[1], *tuple[int | str, ...], Literal[3]])
"#,
);

testcase!(
    test_unbounded_solve,
    r#"
from typing import Any
def test(x: tuple[int, str], y: tuple[int, ...], z: tuple[Any, ...]) -> None:
  a: tuple[int, int] = z
  b: tuple[int | str, ...] = x
  c: tuple[int | str, ...] = y
  d: tuple[int, ...] = x  # E: EXPECTED tuple[int, str] <: tuple[int, ...]
"#,
);

testcase!(
    test_unpacked_solve,
    r#"
from typing import Any
def test(a: tuple[int, bool, str], b: tuple[Any, ...], c: tuple[int, *tuple[bool, ...], str]) -> None:
  x1: tuple[int, *tuple[bool, ...], str] = a
  x2: tuple[int, *tuple[bool | str, ...]] = a
  x3: tuple[*tuple[int | bool, ...], str] = a
  x4: tuple[int, bool, *tuple[str, ...]] = a
  x5: tuple[*tuple[int, ...], bool, str] = a
  x6: tuple[int, *tuple[bool, ...], str] = b
  x7: tuple[int, *tuple[bool, ...], str] = c
  x8: tuple[int, *tuple[bool | str, ...]] = c
  x9: tuple[*tuple[int | bool, ...], str] = c
  x10: tuple[*tuple[int], *tuple[bool], *tuple[str]] = a
  x11: tuple[int, *tuple[bool, str]] = a
  x12: tuple[*tuple[int, bool, str]] = a
  x13: tuple[*tuple[int, ...], *tuple[bool], *tuple[str]] = a
  x14: tuple[*tuple[int, ...], *tuple[bool, ...], *tuple[str]] = a  # E: Only one unbounded type is allowed to be unpacked
"#,
);

testcase!(
    test_slice_literal,
    r#"
from typing import assert_type, Literal

x = (5, 6, 7)

assert_type(x[0:0], tuple[()])
assert_type(x[0:1], tuple[Literal[5]])
assert_type(x[0:2], tuple[Literal[5], Literal[6]])
assert_type(x[0:3], tuple[Literal[5], Literal[6], Literal[7]])

assert_type(x[1:1], tuple[()])
assert_type(x[1:2], tuple[Literal[6]])
assert_type(x[1:3], tuple[Literal[6], Literal[7]])

assert_type(x[2:2], tuple[()])
assert_type(x[2:3], tuple[Literal[7]])

assert_type(x[3:3], tuple[()])

assert_type(x[:0], tuple[()])
assert_type(x[:1], tuple[Literal[5]])
assert_type(x[:2], tuple[Literal[5], Literal[6]])
assert_type(x[:3], tuple[Literal[5], Literal[6], Literal[7]])

assert_type(x[0:], tuple[Literal[5], Literal[6], Literal[7]])
assert_type(x[1:], tuple[Literal[6], Literal[7]])
assert_type(x[2:], tuple[Literal[7]])
assert_type(x[3:], tuple[()])
"#,
);

testcase!(
    test_unbounded_tuple_hint,
    r#"
x1: tuple[str, ...] = ("ok",)
x2: tuple[int, ...] = ("err",)  # E: EXPECTED tuple[Literal['err']] <: tuple[int, ...]
    "#,
);

testcase!(
    test_superclass_tuple_hint,
    r#"
from typing import Iterable, Literal
x1: Iterable[Literal['ok']] = ("ok",)
x2: Iterable = ("ok",)
x3: object = ("ok",)
x4: Iterable[int] = ("err",)  # E: EXPECTED tuple[Literal['err']] <: Iterable[int]
x5: list[int] = ("err",)  # E: EXPECTED tuple[Literal['err']] <: list[int]
    "#,
);

testcase!(
    test_empty_tuple_hint,
    r#"
from typing import Iterable
x: Iterable[str] = ()
    "#,
);

testcase!(
    test_unpack_union,
    r#"
from typing import assert_type
def f() -> tuple[int, str] | tuple[bool, ...]: ...
(x, y) = f()
assert_type(x, int | bool)
assert_type(y, str | bool)

(x, y, z) = f()  # E: Cannot unpack
    "#,
);

testcase!(
    test_iterate_union,
    r#"
from typing import assert_type
def f() -> tuple[int, str] | tuple[bool, ...]: ...
for x in f():
    assert_type(x, int | bool | str)
    "#,
);

testcase!(
    test_tuple_parent,
    r#"
from typing import assert_type
class C1(tuple[int, ...]):
    pass
class C2(tuple[int, int]):
    pass
for x in C1():
    assert_type(x, int)
for x in C2():
    assert_type(x, int)
    "#,
);
