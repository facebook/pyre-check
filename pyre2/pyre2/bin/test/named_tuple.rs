/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_named_tuple,
    r#"
from typing import NamedTuple
class Pair(NamedTuple):
    x: int
    y: int
p: Pair = Pair(1, 2)
p = Pair(x=1, y=2)
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
