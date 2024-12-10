/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_class_init,
    r#"
from typing import assert_type
class Foo:
    def __init__(self, x: int): pass
v = Foo(1)
assert_type(v, Foo)
"#,
);

simple_test!(
    test_generic_class,
    r#"
from typing import assert_type
class Box[T]:
    def __init__(self, x: T): pass

    def wrap(self) -> Box[Box[T]]:
        return Box(self)

def f() -> int:
    return 1
b3 = Box(f()).wrap().wrap()
assert_type(b3, Box[Box[Box[int]]])

assert_type(Box[int](1), Box[int])
Box[int]("oops")  # E: Literal['oops'] <: int
"#,
);

simple_test!(
    test_generic_init_in_generic_class,
    r#"
from typing import assert_type
class Box[T]:
    def __init__[S](self, x: S, y: S):
        pass
    def wrap(self, x: bool) -> Box[Box[T]]:
        if x:
            return Box(self, self)  # ok
        else:
            return Box(self, 42)  # E: EXPECTED Literal[42] <: Box[?_]
b = Box[int]("hello", "world")
assert_type(b, Box[int])
assert_type(b.wrap(True), Box[Box[int]])
    "#,
);

// TODO: support this
simple_test!(
    test_metaclass_call,
    r#"
class Meta(type):
    def __call__[T](cls: type[T], x: int) -> T: ...
class C(metaclass=Meta):
    pass
C(5)    # E: Expected 0 positional argument(s)
C()     # Should be an error
C("5")  # E: Expected 0 positional argument(s)
    "#,
);

// TODO: support this
simple_test!(
    test_new,
    r#"
class C:
    def __new__[T](cls: type[T], x: int) -> T: ...
C(5)    # E: Expected 0 positional argument(s)
C()     # Should be an error
C("5")  # E: Expected 0 positional argument(s)
    "#,
);
