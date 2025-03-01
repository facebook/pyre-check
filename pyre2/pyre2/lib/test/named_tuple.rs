/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_named_tuple,
    r#"
from typing import NamedTuple, assert_type
class Pair(NamedTuple):
    x: int
    y: str
p: Pair = Pair(1, "")
p = Pair(x=1, y="")
x, y = p
assert_type(x, int)
assert_type(y, str)
assert_type(p[0], int)
assert_type(p[1], str)
assert_type(p[:2], tuple[int, str])
    "#,
);

testcase!(
    test_named_tuple_functional,
    r#"
from typing import NamedTuple, Any, assert_type
from collections import namedtuple
Point1 = namedtuple('Point1', ['x', 'y'])
Point2 = namedtuple('Point2', ('x', 'y'))
Point3 = namedtuple('Point3', 'x y')
Point4 = namedtuple('Point4', 'x, y')
Point5 = NamedTuple('Point5', [('x', int), ('y', int)])
Point6 = NamedTuple('Point6', (('x', int), ('y', int)))
assert_type(Point1(1, 2).x, Any)
assert_type(Point2(1, 2).x, Any)
assert_type(Point3(1, 2).x, Any)
assert_type(Point4(1, 2).x, Any)
assert_type(Point5(1, 2).x, int)
assert_type(Point5(x=1, y=2).x, int)
assert_type(Point6(1, 2).x, int)
    "#,
);

testcase!(
    test_named_tuple_functional_rename,
    r#"
from collections import namedtuple
NT1 = namedtuple("NT1", ["a", "a"])  # E: Duplicate field `a`
NT2 = namedtuple("NT2", ["abc", "def"], rename=False)  # E: `def` is not a valid identifier
NT3 = namedtuple("NT3", ["abc", "def"], rename=True)
NT4 = namedtuple("NT4", ["def", "ghi"], rename=True)
NT3(abc="", _1="")
NT4(_0="", ghi="")
    "#,
);

testcase!(
    test_named_tuple_functional_duplicate,
    r#"
from typing import NamedTuple
Point = NamedTuple('Point', [('x', int), ('x', int)])  # E: Duplicate field `x`
    "#,
);

testcase!(
    test_named_tuple_subtype,
    r#"
from typing import NamedTuple
class Pair(NamedTuple):
    x: int
    y: str
p: Pair = Pair(1, "")
def func1(x: tuple[int | str, ...]) -> None: ...
def func2(x: tuple[int, str]) -> None: ...
func1(p)
func2(p)
    "#,
);

testcase!(
    test_named_tuple_match,
    r#"
from typing import NamedTuple, assert_type
class Pair(NamedTuple):
    x: int
    y: int
def test(p: Pair):
    match p:
        case Pair(x, y):
            assert_type(x, int)
            assert_type(y, int)
    match p:
        case x, y:
            assert_type(x, int)
            assert_type(y, int)
    "#,
);

testcase!(
    test_named_tuple_iter,
    r#"
from typing import NamedTuple, reveal_type
class Pair(NamedTuple):
    x: int
    y: str

class Pair2[T](NamedTuple):
    x: int
    y: T
    
def test(p: Pair, p2: Pair2[bytes]):
    reveal_type(p.__iter__)  # E: BoundMethod[Pair, (self: Pair) -> Iterable[int | str]]
    reveal_type(p2.__iter__)  # E: BoundMethod[Pair2[bytes], (self: Pair2[bytes]) -> Iterable[bytes | int]]
    "#,
);

testcase_with_bug!(
    "NamedTuple extends tuple[Any, ...], making it a subtype of too many things",
    test_named_tuple_subclass,
    r#"
from typing import NamedTuple, Sequence, Never
class Pair(NamedTuple):
    x: int
    y: str
p: Pair = Pair(1, "")
x1: Sequence[int|str] = p # should succeed
x2: Sequence[Never] = p # should fail
    "#,
);

testcase!(
    test_named_tuple_multiple_inheritance,
    r#"
from typing import NamedTuple
class Foo: pass
class Pair(NamedTuple, Foo):  # E: Named tuples do not support multiple inheritance
    x: int
    y: int
class Pair2(NamedTuple):
    x: int
    y: int
class Pair3(Pair2, Foo):  # E: Named tuples do not support multiple inheritance
    pass
    "#,
);

testcase!(
    test_named_tuple_init_requiredness,
    r#"
from typing import NamedTuple
class Pair(NamedTuple):
    x: int
    y: str = "y"
Pair(x=5)
Pair(y="foo")  # E: Missing argument `x`
    "#,
);
