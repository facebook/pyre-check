/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;
use crate::test::util::TestEnv;

simple_test!(
    test_subscript_unpack_assign,
    r#"
from typing import assert_type

x: list[int] = [0, 1, 2]
x[0], x[1] = 3, 4
x[0], x[1] = 3, "foo"  # E: EXPECTED Literal['foo'] <: int
"#,
);

simple_test!(
    test_subscript_assign,
    r#"
from typing import assert_type

x = []
x[0] = 1
assert_type(x, list[int])

y = [1, 2, 3]
y[0] = 1
assert_type(y, list[int])

z = [1, 2, 3]
z[0] = "oops"  # E: Literal['oops'] <: int

a: int = 1
a[0] = 1  # E: Object of class `int` has no attribute `__setitem__`

def f(x: int) -> None:
    x[0] = 1  # E: Object of class `int` has no attribute `__setitem__`
"#,
);

simple_test!(
    test_error_assign,
    r#"
x: str = 1  # E: Literal[1] <: str
y = x
"#,
);

simple_test!(
    test_assign_twice_empty,
    r#"
from typing import assert_type
def b() -> bool:
    return True

if b():
    x = []
else:
    x = [3]
y = x
assert_type(y, list[int])
"#,
);

simple_test!(
    test_assign_widen,
    r#"
from typing import Literal, LiteralString, Any
a: Literal['test'] = "test"
b: LiteralString = "test"
c: str = "test"
d: Any = "test"
"#,
);

simple_test!(
    test_assign_widen_list,
    r#"
from typing import Literal, LiteralString, Any
a: list[Literal['test']] = ["test"]
b: list[LiteralString] = ["test"]
c: list[str] = ["test"]
d: list[Any] = ["test"]
"#,
);

simple_test!(
    test_assign_at_types,
    r#"
a: int = 3
a = "test"  # E: Literal['test'] <: int
"#,
);

simple_test!(
    test_optional_assign,
    r#"
from typing import Optional
x: Optional[int] = 42
y: Optional[str] = 43  # E: Literal[43] <: str | None
    "#,
);

simple_test!(
    test_assign_ellipse,
    TestEnv::one_with_path("foo", "x: int = ...", "foo.pyi"),
    r#"
from typing import assert_type
from types import EllipsisType
from foo import x
assert_type(x, int)
y: int = ...  # E: Ellipsis <: int
z: EllipsisType = ...
"#,
);

simple_test!(
    test_assign_unpack,
    r#"
from typing import assert_type, Literal
a, b = (1, "test")
assert_type(a, Literal[1])
assert_type(b, Literal["test"])
    "#,
);

simple_test!(
    test_assign_unpack_unpack,
    r#"
from typing import assert_type, Literal
(a, b), c, d = ((1, "test"), 2, 3)
assert_type(a, Literal[1])
assert_type(b, Literal["test"])
assert_type(c, Literal[2])
assert_type(d, Literal[3])
    "#,
);

simple_test!(
    test_assign_unpack_ambiguous,
    r#"
from typing import assert_type
def f(x: list[str]):
    a, b = x
    assert_type(a, str)
    assert_type(b, str)
    "#,
);

simple_test!(
    test_assign_multiple,
    r#"
from typing import assert_type, Literal
a = b = 1
assert_type(a, Literal[1])
assert_type(b, Literal[1])
    "#,
);

simple_test!(
    test_assign_list,
    r#"
from typing import assert_type, Literal
[a, b] = (1, "test")
assert_type(a, Literal[1])
assert_type(b, Literal["test"])
    "#,
);

simple_test!(
    test_unpack_too_many,
    r#"
(a, b, c, d) = (1, 2)  # E: Cannot unpack tuple[Literal[1], Literal[2]] (of size 2) into 4 values
    "#,
);

simple_test!(
    test_unpack_not_enough,
    r#"
(a,) = (1, 2)  # E: Cannot unpack tuple[Literal[1], Literal[2]] (of size 2) into 1 value
() = (1, 2)  # E: Cannot unpack tuple[Literal[1], Literal[2]] (of size 2) into 0 values
    "#,
);

simple_test!(
    test_splat_back,
    r#"
from typing import assert_type, Literal
(a, b, *c) = (1, 2, 3, "test")
assert_type(a, Literal[1])
assert_type(b, Literal[2])
assert_type(c, list[Literal["test", 3]])
    "#,
);

simple_test!(
    test_splat_front,
    r#"
from typing import assert_type, Literal
(*a, b, c) = (1, 2, 3, "test")
assert_type(a, list[Literal[1, 2]])
assert_type(b, Literal[3])
assert_type(c, Literal["test"])
    "#,
);

simple_test!(
    test_splat_middle,
    r#"
from typing import assert_type, Literal
(a, *b, c) = (1, True, 2, "test")
assert_type(a, Literal[1])
assert_type(b, list[Literal[True, 2]])
assert_type(c, Literal["test"])
    "#,
);

simple_test!(
    test_splat_unpack,
    r#"
from typing import assert_type, Literal
(a, *(b,)) = (1, 2)
assert_type(a, Literal[1])
assert_type(b, Literal[2])
    "#,
);

simple_test!(
    test_splat_nothing,
    r#"
from typing import assert_type, Never
(*a,) = ()
assert_type(a, list[Never])
    "#,
);

simple_test!(
    test_never,
    r#"
from typing import Any, Never, NoReturn
def foo(x: Never) -> Any:
    y: NoReturn = x
    z: int = x
    return x
def bar(x: Never) -> NoReturn:
    return x
    "#,
);

simple_test!(
    test_splat_ambiguous,
    r#"
from typing import assert_type
def f(x: list[str]):
    a, *b = x
    assert_type(a, str)
    assert_type(b, list[str])
    "#,
);

simple_test!(
    test_splat_error,
    r#"
a, *b = (1,)  # OK
a, *b = ()  # E: Cannot unpack tuple[()] (of size 0) into 1+ values
    "#,
);

simple_test!(
    test_set_attribute,
    r#"
class A:
    x: int
def f(a: A):
    a.x = 1  # OK
    a.x = "oops"  # E: EXPECTED Literal['oops'] <: int
    "#,
);

simple_test!(
    test_self_attribute_unannotated,
    r#"
from typing import assert_type
class A:
    def __init__(self, x: int):
        self.x = x
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_self_attribute_assign_twice,
    r#"
from typing import assert_type
class A:
    def f(self, x: str):
        self.x = x  # E: EXPECTED str <: int
    def __init__(self, x: int):
        self.x = x
    "#,
);

simple_test!(
    test_self_attribute_annotated_in_class_body,
    r#"
from typing import assert_type
class A:
    x: str
    def __init__(self, x: int):
        self.x = x  # E: EXPECTED int <: str
    "#,
);

simple_test!(
    test_self_attribute_annotated_assignment,
    r#"
from typing import assert_type

class A:
    def __init__(self, x: str):
        self.x: int = x  # E: EXPECTED str <: int
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_self_attribute_annotated_twice,
    r#"
from typing import assert_type, Literal
class A:
    x: int
    def __init__(self):
        self.x: Literal[1] = 1  # E: Inconsistent type annotations for x: Literal[1], int
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_self_attribute_qualified,
    r#"
from typing import assert_type, Final, Literal
class A:
    def __init__(self):
        self.x: Final = 0
def f(a: A):
    assert_type(a.x, Literal[0])
    "#,
);

simple_test!(
    test_self_attribute_bare_annotation,
    r#"
from typing import assert_type
class A:
    def __init__(self, x: str):
        self.x: int
        self.x = x  # E: EXPECTED str <: int
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_multiple_annotations,
    r#"
from typing import Literal
def f(cond: bool):
    x: int = 0
    if cond:
        x: int = 1  # OK
    y: int = 0
    if cond:
        y: str = "oops"  # E: Inconsistent type annotations for y
    z: int = 0
    if cond:
        z: Literal[1] = 1  # E: Inconsistent type annotations for z
    "#,
);

simple_test!(
    test_attribute_inference,
    r#"
class C:
    x: list[int | str]
def f(c: C):
    c.x = [5]
    "#,
);

simple_test!(
    test_set_attribute_in_init_nested,
    r#"
from typing import assert_type, Literal
class C:
    def __init__(self):
        def f():
            self.x = 0
        f()
def f(c: C):
    assert_type(c.x, Literal[0])
    "#,
);

// TODO: Should `C.x` exist?
simple_test!(
    test_set_attribute_in_init_indirect,
    r#"
class C:
    def __init__(self):
        self.f()
    def f(self):
        self.x = 0  # E: Object of class `C` has no attribute `x`
def f(c: C):
    return c.x  # E: Object of class `C` has no attribute `x`
    "#,
);

simple_test!(
    test_type_alias_simple,
    r#"
from typing import assert_type
type X = int
def f(x: X):
    assert_type(x, int)
    "#,
);

simple_test!(
    test_type_alias_generic,
    r#"
from typing import assert_type
type X[T] = list[T]
def f(x: X[int]):
    assert_type(x, list[int])
    "#,
);

simple_test!(
    test_aug_assign_simple,
    r#"
x: list[int] = []
x += [1]
x += ["foo"]  # E: EXPECTED Literal['foo'] <: int
"#,
);

simple_test!(
    test_aug_assign_function,
    r#"
def foo(y: list[int]) -> None:
    y += [1]
    y += ["foo"]  # E: EXPECTED Literal['foo'] <: int
    z: list[int] = []
    z += [1]
    z += ["foo"]  # E: EXPECTED Literal['foo'] <: int
"#,
);

simple_test!(
    test_aug_assign_attr,
    r#"
class C:
    foo: list[int]

    def __init__(self) -> None:
        self.foo = []

c: C = C()
c.foo += [1]
c.foo += ["foo"]  # E: EXPECTED Literal['foo'] <: int
"#,
);

simple_test!(
    test_aug_assign_attr_self,
    r#"
class C:
    foo: list[int]

    def __init__(self) -> None:
        self.foo = []
        self.foo += [1]
        self.foo += ["foo"]  # E: EXPECTED Literal['foo'] <: int
"#,
);

simple_test!(
    test_aug_assign_subscript,
    r#"
x: list[list[int]] = []
x += [[1]]
x[0] += [1]
x += [1]  # E: EXPECTED Literal[1] <: list[int]
"#,
);

simple_test!(
    test_assign_special_subtype,
    r#"
from types import NoneType, EllipsisType

def foo(a: tuple[int, ...], b: NoneType, c: EllipsisType) -> None:
    a2: tuple = a
    b = None
    b2: object = b
    b3: None = b
    b4: int | None = b
    c = ...
    c2: object = c
"#,
);

simple_test!(
    test_subscript_assign_any_check_rhs,
    r#"
from typing import Any
def expect_str(x: str): ...
def test(x: Any):
    x[0] += expect_str(0) # E: EXPECTED Literal[0] <: str
"#,
);

simple_test!(
    test_aug_assign_any_check_rhs,
    r#"
from typing import Any
def expect_str(x: str): ...
def test(x: Any):
    x += expect_str(0) # E: EXPECTED Literal[0] <: str
"#,
);
