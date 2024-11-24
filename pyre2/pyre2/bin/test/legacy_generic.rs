/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_tyvar_function,
    r#"
from typing import TypeVar, assert_type

T = TypeVar("T")

def foo(x: T) -> T:
    y: T = x
    return y

assert_type(foo(1), int)
"#,
);

simple_test!(
    test_tyvar_alias,
    r#"
from typing import assert_type
import typing

T = typing.TypeVar("T")

def foo(x: T) -> T:
    return x

assert_type(foo(1), int)
"#,
);

simple_test!(
    test_tyvar_mix,
    r#"
from typing import TypeVar, assert_type
U = TypeVar("U")
def foo[T](
      x: U  # E: Type parameter U is not included in the type parameter list
    ) -> U:  # E: Type parameter U is not included in the type parameter list
    return x

assert_type(foo(1), int)
"#,
);

simple_test!(
    test_legacy_generic_syntax,
    r#"
from typing import Generic, TypeVar, assert_type

T = TypeVar("T")

class C(Generic[T]):
    x: T

c: C[int]
assert_type(c.x, int)
    "#,
);

simple_test!(
    test_legacy_generic_syntax_inheritance,
    r#"
from typing import Generic, TypeVar, assert_type

T = TypeVar("T")
S = TypeVar("S")

class C(Generic[T]):
    x: T

class D(Generic[S], C[list[S]]):
    pass

d: D[int]
assert_type(d.x, list[int])
    "#,
);

simple_test!(
    test_legacy_generic_syntax_inherit_twice,
    r#"
from typing import Generic, TypeVar
_T = TypeVar('_T')
class A(Generic[_T]):
    pass
class B(A[_T]):
    pass
class C(B[int]):
    pass
    "#,
);

simple_test!(
    test_legacy_generic_syntax_multiple_implicit_tparams,
    r#"
from typing import Generic, TypeVar, assert_type
_T = TypeVar('_T')
_U = TypeVar('_U')
class A(Generic[_T]):
    a: _T
class B(Generic[_T]):
    b: _T
class C(Generic[_T]):
    c: _T
class D(A[_T], C[_U], B[_T]):
    pass
x: D[int, str]
assert_type(x.a, int)
assert_type(x.b, int)
assert_type(x.c, str)
    "#,
);

simple_test!(
    test_legacy_generic_syntax_filtered_tparams,
    r#"
from typing import Generic, TypeVar
_T1 = TypeVar('_T1')
_T2 = TypeVar('_T2')
class A(Generic[_T1, _T2]):
    pass
class B(A[_T1, int]):
    pass
class C(B[str]):
    pass
    "#,
);

// The TODO here is because we implemented but have temporarily disabled a check
// for the use of a generic class without type arguments as a type annotation;
// this check needs to be configurable and we don't have the plumbing yet.
simple_test!(
    test_legacy_generic_syntax_implicit_targs,
    r#"
from typing import Any, Generic, TypeVar, assert_type
T = TypeVar('T')
class A(Generic[T]):
    x: T
def f(a: A):  # TODO: The generic class `A` is missing type arguments.
    assert_type(a.x, Any)
    "#,
);

simple_test!(
    test_tvar_missing_name,
    r#"
from typing import TypeVar
T = TypeVar()  # E: Missing `name` argument
    "#,
);

simple_test!(
    test_tvar_unexpected_keyword,
    r#"
from typing import TypeVar
T = TypeVar('T', foo=True)  # E: Unexpected keyword argument `foo`
    "#,
);

simple_test!(
    test_tvar_constraints_and_bound,
    r#"
from typing import TypeVar
T = TypeVar('T', int, bound=int)  # E: TypeVar cannot have both constraints and bound
    "#,
);

simple_test!(
    test_tvar_variance,
    r#"
from typing import TypeVar
T1 = TypeVar('T1', covariant=True, contravariant=True)  # E: Contradictory variance specifications
T2 = TypeVar('T2', covariant=True, contravariant=False)
T3 = TypeVar('T3', covariant="lunch")  # E: Expected literal True or False
    "#,
);

simple_test!(
    test_tvar_forward_ref,
    r#"
from typing import TypeVar
T1 = TypeVar('T1', bound='A')
T2 = TypeVar('T2', bound='B')  # E: Could not find name `B`
T3 = TypeVar('T3', 'A', int)
T4 = TypeVar('T4', 'B', int)  # E: Could not find name `B`

class A:
    pass
    "#,
);

simple_test!(
    test_tvar_class_constraint,
    r#"
from typing import TypeVar
class A:
    pass
T1 = TypeVar('T1', int, A)
T2 = TypeVar('T2', int, B)  # E: Could not find name `B`
    "#,
);

simple_test!(
    test_ordering_of_tparams_on_generic_base,
    r#"
from typing import Generic, TypeVar, assert_type

T = TypeVar("T")
S = TypeVar("S")

class Base(Generic[T]):
    x: T

class Child(Base[S], Generic[T, S]):
    y: T

def f(c: Child[int, str]):
    assert_type(c.x, str)
    assert_type(c.y, int)
    "#,
);

simple_test!(
    test_ordering_of_tparams_on_protocol_base,
    r#"
from typing import Protocol, TypeVar, assert_type

T = TypeVar("T")
S = TypeVar("S")

class Base(Protocol[T]):
    x: T

class Child(Base[S], Protocol[T, S]):
    y: T

def f(c: Child[int, str]):
    assert_type(c.x, str)
    assert_type(c.y, int)
    "#,
);

simple_test!(
    test_both_generic_and_protocol,
    r#"
from typing import Generic, Protocol, TypeVar, assert_type

T = TypeVar("T")
S = TypeVar("S")
U = TypeVar("U")
V = TypeVar("V")

class C(Protocol[V, T], Generic[S, T, U]):  # E: Class `C` specifies type parameters in both `Generic` and `Protocol` bases
    s: S
    t: T
    u: U
    v: V

def f(c: C[int, str, bool, bytes]):
    assert_type(c.s, int)
    assert_type(c.t, str)
    assert_type(c.u, bool)
    assert_type(c.v, bytes)
    "#,
);
