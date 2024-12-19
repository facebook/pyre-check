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

testcase!(
    test_metaclass_call,
    r#"
class Meta(type):
    def __call__[T](cls: type[T], x: int) -> T: ...
class C(metaclass=Meta):
    def __init__(self, *args, **kwargs):
        pass
C(5)
C()     # E: Missing argument 'x'
C("5")  # E: EXPECTED Literal['5'] <: int
    "#,
);

testcase!(
    test_metaclass_call_bad_classdef,
    r#"
class Meta(type):
    def __call__[T](cls: type[T], x: int) -> T: ...
# C needs to define __new__ and/or __init__ taking `x: int` to be compatible with Meta.
class C(metaclass=Meta):
    pass
# Both of these calls error at runtime.
C()   # E: Missing argument 'x'
C(0)  # E: Expected 0 positional arguments
    "#,
);

testcase!(
    test_metaclass_call_returns_something_else,
    r#"
from typing import assert_type
class Meta(type):
    def __call__(cls) -> int:
        return 0
class C(metaclass=Meta):
    pass
x = C()
assert_type(x, int)
    "#,
);

testcase!(
    test_new,
    r#"
class C:
    def __new__[T](cls: type[T], x: int) -> T: ...
C(5)
C()     # E: Missing argument 'x'
C("5")  # E: EXPECTED Literal['5'] <: int
    "#,
);

testcase!(
    test_new_and_init,
    r#"
class C:
    def __new__[T](cls: type[T], x: int) -> T: ...
    def __init__(self, x: int):
        pass
C(5)
C()     # E: Missing argument 'x'
C("5")  # E: EXPECTED Literal['5'] <: int
    "#,
);

testcase!(
    test_new_and_inherited_init,
    r#"
class Parent1:
    def __init__(self):
        pass
class Parent2:
    def __init__(self, x: int):
        pass
class GoodChild(Parent2):
    def __new__[T](cls: type[T], x: int) -> T: ...
class BadChild(Parent1):
    # Incompatible with inherited __init__
    def __new__[T](cls: type[T], x: int) -> T: ...
GoodChild(0)
GoodChild()  # E: Missing argument 'x'
# Both of these calls error at runtime.
BadChild()   # E: Missing argument 'x'
BadChild(0)  # E: Expected 0 positional arguments
    "#,
);

testcase!(
    test_new_returns_something_else,
    r#"
from typing import assert_type
class C:
    def __new__(cls) -> int:
        return 0
x = C()
assert_type(x, int)
    "#,
);

testcase_with_bug!(
    test_generic_new,
    r#"
class C[T]:
    def __new__(cls, x: T): ...
C(0)  # Should we allow this? # E: TODO # E: Expected 0 positional arguments
C[bool](True)  # This should be ok # E: TODO # E: Expected 0 positional arguments
C[bool](0)  # This should be an error # E: TODO # E: Expected 0 positional arguments
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
