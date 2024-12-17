/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_class_init,
    r#"
from typing import assert_type
class Foo:
    def __init__(self, x: int): pass
v = Foo(1)
assert_type(v, Foo)
"#,
);

testcase!(
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

testcase!(
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

testcase!(
    test_init_self_annotation,
    r#"
class C:
    def __init__[T](self: T, x: T):
        pass

c: C
C(c)  # OK
C(0)  # E: EXPECTED Literal[0] <: C
    "#,
);

testcase!(
    test_init_self_annotation_in_generic_class,
    r#"
class C[T1]:
    def __init__[T2](self: T2, x: T2):
        pass
c: C[int]
C[int](c)  # OK
C[str](c)  # E: EXPECTED C[int] <: C[str]
    "#,
);

// TODO: support this
testcase_with_bug!(
    test_metaclass_call,
    r#"
class Meta(type):
    def __call__[T](cls: type[T], x: int) -> T: ...
class C(metaclass=Meta):
    pass
C(5)    # E: Expected 1 positional argument
C()     # Should be an error
C("5")  # E: Expected 1 positional argument
    "#,
);

// TODO: support this
testcase_with_bug!(
    test_new,
    r#"
class C:
    def __new__[T](cls: type[T], x: int) -> T: ...
C(5)    # E: Expected 1 positional argument
C()     # Should be an error
C("5")  # E: Expected 1 positional argument
    "#,
);

testcase!(
    test_inherit_dunder_init,
    r#"
class A:
    def __init__(self, x: int): pass
class B(A): pass
B(1)
B("")  # E: EXPECTED Literal[''] <: int
    "#,
);
