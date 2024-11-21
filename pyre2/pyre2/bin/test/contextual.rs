/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_context_annassign,
    r#"
class A: ...
class B(A): ...

xs: list[A] = [B()]
"#,
);

simple_test!(
    test_context_assign_annotated_binding,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
xs = [B()]
"#,
);

simple_test!(
    test_context_assign_member,
    r#"
class A: ...
class B(A): ...

class C:
    xs: list[A]

o = C()
o.xs = [B()]
"#,
);

simple_test!(
    test_context_class_field_init,
    r#"
class A: ...
class B(A): ...

class C:
    xs: list[A] = [B()]
    def __init__(self):
        self.xs = [B()]
"#,
);

simple_test!(
    test_context_return_annot,
    r#"
class A: ...
class B(A): ...

def f() -> list[A]:
    return [B()]
"#,
);

simple_test!(
    test_context_parameter,
    r#"
class A: ...
class B(A): ...

def posonly(xs: list[A], /): ...
posonly([B()])

def pos(xs: list[A]): ...
pos([B()])
pos(xs=[B()])

def kwonly(*, xs: list[A]): ...
kwonly(xs=[B()])

def vararg(*args: list[A]): ...
vararg([B()], [B()])

def kwarg(**kwargs: list[A]): ...
kwarg(xs=[B()], ys=[B()])
"#,
);

simple_test!(
    test_context_assign_unpacked_list,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
[*xs] = [B(), B()]
"#,
);

simple_test!(
    test_context_for,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
for xs in [[B()]]:
    pass
"#,
);

simple_test!(
    test_set_hint,
    r#"
from typing import Iterable, MutableSet

x: set[int] = {1}
x: set[int] = {'oops'}  # E: EXPECTED Literal['oops'] <: int
x: MutableSet[int] = {1}
x: MutableSet[int] = {'oops'}  # E: EXPECTED Literal['oops'] <: int
x: Iterable[int] = {1}
x: object = {1}
x: list[int] = {1}  # E: EXPECTED set[Unknown] <: list[int]
    "#,
);

simple_test!(
    test_dict_hint,
    r#"
from typing import Iterable, MutableMapping
x: dict[str, int] = {"a": 1}
x: dict[str, int] = {"a": "oops"}  # E: EXPECTED Literal['oops'] <: int
x: MutableMapping[str, int] = {"a": 1}
x: Iterable[str] = {"a": 1}
x: Iterable[int] = {"oops": 1}  # E: EXPECTED Literal['oops'] <: int
x: object = {"a": 1}
x: list[str] = {"a": 1}  # E: EXPECTED dict[Unknown, Unknown] <: list[str]
    "#,
);

// TODO: Unpacked assignment propagates wrong hint to RHS expression
simple_test!(
    test_context_assign_unpacked_tuple,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
(xs, _) = ([B()], None) # E: EXPECTED tuple[list[B], None] <: list[A]
"#,
);

// TODO: No context propagated to subscript assignment target
simple_test!(
    test_context_assign_subscript,
    r#"
class A: ...
class B(A): ...

xs: list[list[A]] = [[]]
xs[0] = [B()] # E: EXPECTED list[B] <: list[A]
"#,
);
