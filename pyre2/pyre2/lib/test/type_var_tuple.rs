/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_type_var_tuple,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
class Array(Generic[*Ts]): ...
def foo(*args: *Ts): ...
"#,
);

testcase!(
    test_type_var_tuple_multiple,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
class ArrayTwoParams(Generic[*Ts, *Ts]): ...  # E: There cannot be more than one TypeVarTuple type parameter
class ArrayTwoParams2[*Ts1, *Ts2](): ...  # E: There cannot be more than one TypeVarTuple type parameter
"#,
);

testcase!(
    test_illegal_unpack,
    r#"
from typing import Unpack
x: Unpack[int] = 1  # E: Unpack is not allowed in this context.
class X(Unpack[int]): ...  # E: Unpack is not allowed in this context.
y: tuple[Unpack[tuple[int, str]]] = (1, "2")  # OK
"#,
);

testcase!(
    bug = "We should disallow star-unpacking in invalid contexts",
    test_invalid_star,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
*Ts
"#,
);

testcase!(
    test_type_var_tuple_class_field_and_constructor,
    r#"
class C1[T]:
    x: tuple[T, ...]
    def __init__(self, x: tuple[T, ...]) -> None:
        self.x = x
        self.y: T = x  # E: `tuple[TypeVar[T], ...]` is not assignable to attribute `y` with type `TypeVar[T]`
class C2[*Ts]:
    x: tuple[*Ts]
    def __init__(self, x: tuple[*Ts]) -> None:
        self.x = x
        self.y: tuple[*Ts] = x
"#,
);

testcase!(
    test_require_unpack,
    r#"
from typing import TypeVarTuple, Unpack, Generic
class A[*Ts]: ...
class B[*Ts]:
    def test1(self) -> A[Ts]: ...  # E: TypeVarTuple must be unpacked
    def test2(self) -> A[*Ts]: ...
    def test3(self) -> A[Unpack[Ts]]: ...
    def test4(self) -> tuple[Ts]: ...  # E: TypeVarTuple must be unpacked
    def test5(self) -> tuple[*Ts]: ...
    def test6(self) -> tuple[Unpack[Ts]]: ...
Ts = TypeVarTuple('Ts')
class C(Ts): ...  # E: TypeVarTuple must be unpacked
class D(Generic[Ts]): ...  # E: TypeVarTuple must be unpacked
"#,
);

testcase!(
    test_type_var_tuple_instantiation,
    r#"
from typing import assert_type
class A[*Ts]:
    def x(self) -> tuple[*Ts]:
        raise Exception()
class B[T, *Ts]:
    def x(self) -> tuple[*Ts, T]:
        raise Exception()
def test(a1: A[int], a2: A[int, str], b: B[int, str, int]):
    assert_type(a1.x(), tuple[int])
    assert_type(a2.x(), tuple[int, str])
    assert_type(b.x(), tuple[str, int, int])
"#,
);

testcase!(
    test_type_var_tuple_solve,
    r#"
from typing import assert_type
class A[*Ts]:
    def x(self) -> tuple[*Ts]: ...
def test[*Ts](x: tuple[*Ts]) -> tuple[*Ts]:
    return x
assert_type(test((1, 2, 3)), tuple[int, int, int])
"#,
);

testcase!(
    test_type_var_tuple_subtype,
    r#"
from typing import assert_type
class A[*Ts]:
    def x(self) -> tuple[*Ts]:
        raise Exception()
def helper(x: A[int, str]): ...
def test[*Ts](x: A[int, str], y: A[str, str, str], z: A[*Ts]):
    helper(x)
    helper(y)  # E: Argument `A[tuple[str, str, str]]` is not assignable to parameter `x` with type `A[tuple[int, str]]`
    helper(z)  # E: Argument `A[TypeVarTuple[Ts]]` is not assignable to parameter `x` with type `A[tuple[int, str]]`
"#,
);
