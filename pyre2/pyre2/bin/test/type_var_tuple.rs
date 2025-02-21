/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_type_var_tuple,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
class Array(Generic[*Ts]): ...
def foo(*args: *Ts): ...
"#,
);

testcase_with_bug!(
    "We should disallow star-unpacking in invalid contexts",
    test_invalid_star,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
*Ts
"#,
);

testcase!(
    test_require_unpack,
    r#"
from typing import TypeVarTuple, Unpack
class A[*Ts]: ...
class B[*Ts]:
    def test1(self) -> A[Ts]: ...  # E: TypeVarTuple must be unpacked
    def test2(self) -> A[*Ts]: ...
    def test3(self) -> A[Unpack[Ts]]: ...
    def test4(self) -> tuple[Ts]: ...  # E: TypeVarTuple must be unpacked
    def test5(self) -> tuple[*Ts]: ...
    def test6(self) -> tuple[Unpack[Ts]]: ...
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

// TODO: improve display of type var tuples
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
    helper(y)  # E: EXPECTED A[tuple[str, str, str]] <: A[tuple[int, str]]
    helper(z)  # E: EXPECTED A[tuple[*?_TypeVarTuple]] <: A[tuple[int, str]]
"#,
);
