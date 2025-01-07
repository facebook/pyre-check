/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase_with_bug!(
    r#"
The TODO here is because we implemented but have temporarily disabled a check
for the use of a generic class without type arguments as a type annotation;
this check needs to be configurable and we don't have the plumbing yet.
    "#,
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

testcase!(
    test_tvar_missing_name,
    r#"
from typing import TypeVar
T = TypeVar()  # E: Missing `name` argument
    "#,
);

testcase!(
    test_tvar_wrong_name,
    r#"
from typing import TypeVar
T = TypeVar("Z")  # E: TypeVar must be assigned to a variable named Z
    "#,
);

testcase!(
    test_tvar_wrong_name_expr,
    r#"
from typing import TypeVar
T = TypeVar(17)  # E: Expected first argument of TypeVar to be a string literal
    "#,
);

testcase!(
    test_tvar_wrong_name_bind,
    r#"
from typing import TypeVar
x = "test"
T = TypeVar(x)  # E: Expected first argument of TypeVar to be a string literal
    "#,
);

testcase!(
    test_tvar_keyword_name,
    r#"
from typing import TypeVar
T = TypeVar(name = "T")
    "#,
);

testcase!(
    test_tvar_unexpected_keyword,
    r#"
from typing import TypeVar
T = TypeVar('T', foo=True)  # E: Unexpected keyword argument `foo`
    "#,
);

testcase!(
    test_tvar_constraints_and_bound,
    r#"
from typing import TypeVar
T = TypeVar('T', int, bound=int)  # E: TypeVar cannot have both constraints and bound
    "#,
);

testcase!(
    test_tvar_variance,
    r#"
from typing import TypeVar
T1 = TypeVar('T1', covariant=True, contravariant=True)  # E: Contradictory variance specifications
T2 = TypeVar('T2', covariant=True, contravariant=False)
T3 = TypeVar('T3', covariant="lunch")  # E: Expected literal True or False
    "#,
);

testcase!(
    test_tvar_forward_ref,
    r#"
from typing import TypeVar
T1 = TypeVar('T1', bound='A')
T2 = TypeVar('T2', bound='B')  # E: Could not find name `B`
T3 = TypeVar('T3', 'A', int)
T4 = TypeVar('T4', 'B', int)  # E: Could not find name `B`
T5 = TypeVar('T5', default='A')
T6 = TypeVar('T6', default='B')  # E: Could not find name `B`

class A:
    pass
    "#,
);

testcase!(
    test_tvar_class_constraint,
    r#"
from typing import TypeVar
class A:
    pass
T1 = TypeVar('T1', int, A)
T2 = TypeVar('T2', int, B)  # E: Could not find name `B`
    "#,
);

testcase!(
    test_tvar_kwargs,
    r#"
from typing import TypeVar
T = TypeVar('T', **{'a': 'b'})  # E: Cannot pass unpacked keyword arguments to TypeVar
    "#,
);

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
    test_both_generic_and_implicit,
    r#"
from typing import Generic, Protocol, TypeVar, assert_type

T = TypeVar("T")
S = TypeVar("S")

class C(Generic[T], list[S]):  # E: Class `C` uses type variables not specified in `Generic` or `Protocol` base
    t: T

def f(c: C[int, str]):
    assert_type(c.t, int)
    assert_type(c[0], str)
    "#,
);

testcase!(
    test_default,
    r#"
from typing import Generic, TypeVar, assert_type
T1 = TypeVar('T1')
T2 = TypeVar('T2', default=int)
class C(Generic[T1, T2]):
    pass
def f9(c1: C[int, str], c2: C[str]):
    assert_type(c1, C[int, str])
    assert_type(c2, C[str, int])
    "#,
);

testcase!(
    test_bad_default_order,
    r#"
from typing import Generic, TypeVar
T1 = TypeVar('T1', default=int)
T2 = TypeVar('T2')
class C(Generic[T1, T2]):  # E: A type parameter without a default cannot follow one with a default
    pass
    "#,
);

testcase!(
    test_variance,
    r#"
from typing import Generic, TypeVar
T1 = TypeVar('T1', covariant=True)
T2 = TypeVar('T2', contravariant=True)
class C(Generic[T1, T2]):
    pass
class Parent:
    pass
class Child(Parent):
    pass
def f1(c: C[Parent, Child]):
    f2(c)  # E: EXPECTED
def f2(c: C[Child, Parent]):
    f1(c)
    "#,
);

// This test exercises an edge case where naively using type analysis on base classes
// can cause problems in the interaction of tparams validation and recursion.
testcase!(
    test_generic_with_reference_to_self_in_base,
    r#"
from typing import Generic, TypeVar, Any, assert_type

T = TypeVar("T")

class C(list[C[T]]):
    t: T

def f(c: C[int]):
    assert_type(c.t, int)
    assert_type(c[0], C[int])
    "#,
);

testcase!(
    test_redundant_generic_base,
    r#"
from typing import Generic
class C[T](Generic[T]):  # E: Redundant
    pass
    "#,
);
