/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_type_var_tuple_default,
    r#"
from typing import TypeVarTuple, Unpack

Ts1 = TypeVarTuple("Ts1", default=Unpack[tuple[int, int]])

def test[*Ts = Unpack[tuple[int, int]]](x: tuple[*Ts]) -> tuple[*Ts]:
    return x
 "#,
);

testcase!(
    bug = "Parameter list literals do not get handled correctly",
    test_param_spec_default,
    r#"
from typing import ParamSpec, Callable

P1 = ParamSpec("P1", default=...)
P2 = ParamSpec("P2", default=[str, int])  # E: (str, int) is not allowed in this context.

def test[**P = ...](x: Callable[P, None]) -> Callable[P, None]:
    return x
def test2[**P = [str, int]](x: Callable[P, None]) -> Callable[P, None]:  # E: Expected a type form, got instance of `list[type[int] | type[str]]`
    return x
 "#,
);

testcase!(
    test_generic_bounds,
    r#"
class A: ...
class B(A): ...
class C(B): ...

def test[T: B](x: T) -> None:
    a: A = x  # OK  # E: `?T` is not assignable to `A`
    b: B = x  # OK  # E: `?T` is not assignable to `B`
    c: C = x  # E: `?T` is not assignable to `C`

test(A())  # Not OK
test(B())
test(C())
 "#,
);

testcase!(
    test_generic_constraints,
    r#"
class A: ...
class B(A): ...
class C(A): ...
class D(C): ...

def test[T: (B, C)](x: T) -> None:
    a: A = x  # OK  # E: `?T` is not assignable to `A`
    b: B = x  # E: `?T` is not assignable to `B`
    c: C = x  # E: `?T` is not assignable to `C`

test(A())  # Not OK
test(B())
test(C())
test(D())
 "#,
);

testcase!(
    test_generic_constraint_with_default,
    r#"
from typing import TypeVar
class A: ...
class B(A): ...
class C(A): ...
class D(C): ...

def test1[T: (B, C) = A](x: T) -> None:  # E: Expected default `A` of `T` to be one of the following constraints: `B`, `C`
    pass
def test2[T: (B, C) = B](x: T) -> None:
    pass
def test3[T: (B, C) = C](x: T) -> None:
    pass
def test4[T: (B, C) = D](x: T) -> None:  # E: Expected default `D` of `T` to be one of the following constraints: `B`, `C`
    pass

T1 = TypeVar("T1", B, C, default=A)  # E: Expected default `A` of `T1` to be one of the following constraints: `B`, `C`
T2 = TypeVar("T2", B, C, default=B)
T3 = TypeVar("T3", B, C, default=C)
T4 = TypeVar("T4", B, C, default=D)  # E: Expected default `D` of `T4` to be one of the following constraints: `B`, `C`
 "#,
);

testcase!(
    test_generic_bound_with_default,
    r#"
from typing import TypeVar
class A: ...
class B(A): ...
class C(A): ...
class D(C): ...

def test1[T: C = A](x: T) -> None:  # E: Expected default `A` of `T` to be assignable to the upper bound of `C`
    pass
def test2[T: C = B](x: T) -> None:  # E: Expected default `B` of `T` to be assignable to the upper bound of `C`
    pass
def test3[T: C = C](x: T) -> None:
    pass
def test4[T: C = D](x: T) -> None:
    pass

T1 = TypeVar("T1", bound=C, default=A)  # E: Expected default `A` of `T1` to be assignable to the upper bound of `C`
T2 = TypeVar("T2", bound=C, default=B)  # E: Expected default `B` of `T2` to be assignable to the upper bound of `C`
T3 = TypeVar("T3", bound=C, default=C)
T4 = TypeVar("T4", bound=C, default=D)
 "#,
);
