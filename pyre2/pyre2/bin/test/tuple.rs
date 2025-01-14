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
    test_index,
    r#"
from typing import assert_type

def foo(x: tuple[int, str], y: tuple[int, ...], idx: int) -> None:
    assert_type(x[idx], int | str)
    assert_type(y[idx], int)
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
    test_unpack_index_out_of_bounds,
    r#"
def test(x: tuple[int]) -> None:
  y, z = x  # E: Cannot unpack
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
x: tuple[str, ...] = ("ok",)
x: tuple[int, ...] = ("err",)  # E: EXPECTED tuple[Literal['err']] <: tuple[int, ...]
    "#,
);

testcase!(
    test_superclass_tuple_hint,
    r#"
from typing import Iterable, Literal
x: Iterable[Literal['ok']] = ("ok",)
x: Iterable = ("ok",)
x: object = ("ok",)
x: Iterable[int] = ("err",)  # E: EXPECTED Literal['err'] <: int
x: list[int] = ("err",)  # E: EXPECTED tuple[Literal['err']] <: list[int]
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
    test_named_tuple,
    r#"
from typing import NamedTuple
class Pair(NamedTuple):
    x: int
    y: int
    "#,
);
