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
    a: A = x  # OK
    b: B = x  # OK
    c: C = x  # E: `?T` is not assignable to `C`

test(A())  # E: Argument `A` is not assignable to parameter `x` with type `@_` in function `test`
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
    b: B = x  # E: `?T` is not assignable to `B`
    c: C = x  # E: `?T` is not assignable to `C`

test(A())  # E: Argument `A` is not assignable to parameter `x` with type `@_` in function `test`
test(B())
test(C())
test(D())
 "#,
);
