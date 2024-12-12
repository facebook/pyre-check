/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_type_alias_simple,
    r#"
from typing import assert_type
type X = int
def f(x: X):
    assert_type(x, int)
    "#,
);

testcase!(
    test_type_alias_generic,
    r#"
from typing import assert_type
type X[T] = list[T]
def f(x: X[int]):
    assert_type(x, list[int])
    "#,
);

testcase!(
    test_type_alias_missing_quantifieds,
    r#"
from typing import TypeVar
T = TypeVar('T')
type X = list[T]  # E: Type parameters used in `X`
    "#,
);

testcase!(
    test_type_alias_unused_quantifieds,
    r#"
# Questionable code, but not an error
type X[T] = list
    "#,
);

testcase!(
    test_bad_type_alias,
    r#"
type X = 1  # E: Expected `X` to be a type alias, got Literal[1]
    "#,
);

testcase!(
    test_generic_alias_implicit,
    r#"
from typing import TypeVar, assert_type
T = TypeVar('T')
X = list[T]
def f(x: X[int]):
    assert_type(x, list[int])
    "#,
);

testcase!(
    test_generic_alias_explicit,
    r#"
from typing import TypeAlias, TypeVar, assert_type
T = TypeVar('T')
X: TypeAlias = list[T]
def f(x: X[int]):
    assert_type(x, list[int])
    "#,
);

testcase!(
    test_generic_alias_union,
    r#"
from typing import TypeVar, assert_type
T = TypeVar('T')
X = T | list[T]
def f(x: X[int]):
    assert_type(x, int | list[int])
    "#,
);

testcase!(
    test_generic_alias_callable,
    r#"
from typing import Callable, TypeVar, assert_type
T = TypeVar('T')
X1 = Callable[..., T]
X2 = Callable[[T], str]
def f(x1: X1[int], x2: X2[int]):
    assert_type(x1, Callable[..., int])
    assert_type(x2, Callable[[int], str])
    "#,
);

testcase!(
    test_generic_alias_annotated,
    r#"
from typing import Annotated, TypeVar, assert_type
T = TypeVar('T')
X = Annotated[T, 'the world is quiet here']
def f(x: X[int]):
    assert_type(x, int)
    "#,
);

testcase!(
    test_bad_annotated_alias,
    r#"
from typing import TypeAlias
X: TypeAlias = 1  # E: Expected `X` to be a type alias, got Literal[1]
    "#,
);

testcase!(
    test_attribute_access,
    r#"
from typing import TypeAlias
X1 = int
X2: TypeAlias = int
type X3 = int

X1.__add__  # ok
X2.__add__  # ok
X3.__add__  # E: Object of class `TypeAliasType` has no attribute `__add__`
    "#,
);

testcase!(
    test_forward_ref,
    r#"
from typing import TypeAlias, assert_type

X1 = "A"  # Just a normal alias to a str
X2: TypeAlias = "A"  # Forward ref
type X3 = "A"  # Forward ref

class A:
    pass

def f(x1: X1):  # E: untype, got Literal['A']
    pass

def g(x2: X2, x3: X3):
    assert_type(x2, A)
    assert_type(x3, A)
    "#,
);

testcase!(
    test_recursive_alias,
    r#"
from typing import TypeAlias, assert_type

Alias: TypeAlias = int | list["Alias"]

x: Alias = 1
y: Alias = [1]
z: Alias = [[1, 2]]
"#,
);

testcase!(
    test_container_variance,
    r#"
from typing import Iterable
type X1[T] = Iterable[T]
type X2[T] = list[T]
def f1(x: X1[int]):
    pass
def f2(x: X2[int]):
    pass
def g(x: list[bool]):
    f1(x)
    f2(x)  # E: EXPECTED
    "#,
);
