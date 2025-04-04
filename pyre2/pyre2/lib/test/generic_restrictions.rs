/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_quantified_subtyping_no_constraint,
    r#"
def test[T](x: T) -> None:
    y: int = x  # E: `TypeVar[T]` is not assignable to `int`
    z: object = x  # OK
 "#,
);

testcase!(
    test_type_var_tuple_default,
    r#"
from typing import TypeVarTuple, Unpack, assert_type

Ts1 = TypeVarTuple("Ts1", default=Unpack[tuple[int, int]])
Ts2 = TypeVarTuple("Ts2", default=int)  # E: Default for TypeVarTuple must be an unpacked tuple form or another TypeVarTuple, got `int`

def test[*Ts = Unpack[tuple[int, int]]](x: tuple[*Ts]) -> tuple[*Ts]:
    return x
def test2[*Ts = int](x: tuple[*Ts]) -> tuple[*Ts]:  # E: Default for TypeVarTuple must be an unpacked tuple form or another TypeVarTuple, got `int`
    return x

class C[*Ts = Unpack[tuple[int, int]]]:
    def foo(self) -> tuple[*Ts]: ...
assert_type(C().foo(), tuple[int, int])
 "#,
);

testcase!(
    test_param_spec_default,
    r#"
from typing import ParamSpec, Callable

P1 = ParamSpec("P1", default=...)
P2 = ParamSpec("P2", default=[str, int])
P3 = ParamSpec("P3", default=int)  # E: Default for ParamSpec must be a parameter list, `...`, or another ParamSpec, got `int`

def test[**P = ...](x: Callable[P, None]) -> Callable[P, None]:
    return x
def test2[**P = [str, int]](x: Callable[P, None]) -> Callable[P, None]:
    return x
 "#,
);

testcase!(
    test_var_subtype_deadlock,
    r#"
from typing import Iterator

def iter_iter[T](x: Iterator[T]) -> Iterator[T]:
    return iter(x)

iter_iter(iter([1, 2, 3]))
 "#,
);

testcase!(
    test_generic_bounds,
    r#"
class A: ...
class B(A): ...
class C(B): ...

def test[T: B](x: T) -> None:
    a: A = x  # OK
    b: B = x  # OK
    c: C = x  # E: `TypeVar[T]` is not assignable to `C`

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
    a: A = x  # OK
    b: B = x  # E: `TypeVar[T]` is not assignable to `B`
    c: C = x  # E: `TypeVar[T]` is not assignable to `C`

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

testcase!(
    test_bounded_callable,
    r#"
from typing import Callable, TypeVar, assert_type
T = TypeVar('T', bound=Callable[[int], int])
def func(a: T, b: int) -> T:
    assert_type(a(b), int)
    return a
 "#,
);

testcase!(
    test_bounded_typevar_attribute_access,
    r#"
from typing import TypeVar, assert_type
class C:
    x: int
T = TypeVar('T', bound=C)
def func(c: T) -> C:
    assert_type(c.x, int)
    return c
 "#,
);

testcase!(
    bug = "Type params with defaults should be instantiated when accessed from the class",
    test_instantiate_default_typevar,
    r#"
from typing import assert_type, Callable, Self
class C[T = int]:
    def meth(self, /) -> Self:
        return self
    attr: T
assert_type(C.meth, Callable[[C[int]], C[int]])  # E: assert_type(Any, (C[int]) -> C[int]) failed  # E: Generic attribute `meth` of class `C` is not visible on the class
assert_type(C.attr, int)  # E: assert_type(Any, int) failed  # E: Instance-only attribute `attr` of class `C` is not visible on the class
 "#,
);
