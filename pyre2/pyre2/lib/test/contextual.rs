/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_context_annassign,
    r#"
class A: ...
class B(A): ...

xs: list[A] = [B()]
"#,
);

testcase!(
    test_context_assign_annotated_binding,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
xs = [B()]
"#,
);

testcase!(
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

testcase!(
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

testcase!(
    test_context_return_annot,
    r#"
class A: ...
class B(A): ...

def f() -> list[A]:
    return [B()]
"#,
);

testcase!(
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

testcase!(
    test_context_assign_unpacked_list,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
[*xs] = [B(), B()]
"#,
);

testcase!(
    test_context_for,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
for xs in [[B()]]:
    pass
"#,
);

testcase!(
    test_set_hint,
    r#"
from typing import Iterable, MutableSet, Literal
x1: set[int] = {1}
x2: set[int] = {'oops'}  # E: EXPECTED set[str] <: set[int]
x3: set[Literal[1]] = {2}  # E: EXPECTED set[int] <: set[Literal[1]]
x4: MutableSet[int] = {1}
x5: MutableSet[int] = {'oops'}  # E: EXPECTED set[str] <: MutableSet[int]
x6: Iterable[int] = {1}
x7: object = {1}
x8: list[int] = {1}  # E: EXPECTED set[int] <: list[int]
    "#,
);

testcase!(
    test_dict_hint,
    r#"
from typing import Iterable, MutableMapping, Literal
x1: dict[str, int] = {"a": 1}
x2: dict[str, int] = {"a": "oops"}  # E: EXPECTED dict[str, str] <: dict[str, int]
x3: dict[str, Literal[1]] = {"a": 2} # E: EXPECTED dict[str, int] <: dict[str, Literal[1]]
x4: MutableMapping[str, int] = {"a": 1}
x5: Iterable[str] = {"a": 1}
x6: Iterable[int] = {"oops": 1}  # E: EXPECTED dict[str, int] <: Iterable[int]
x7: Iterable[Literal[4]] = {4: "a"}
x8: object = {"a": 1}
x9: list[str] = {"a": 1}  # E: EXPECTED dict[str, int] <: list[str]
    "#,
);

testcase!(
    test_context_list_comprehension,
    r#"
class A: ...
class B(A): ...
xs: list[A] = [B() for _ in [0]]
"#,
);

testcase!(
    test_context_set_comprehension,
    r#"
class A: ...
class B(A): ...
xs: set[A] = {B() for _ in [0]}
"#,
);

testcase!(
    test_context_dict_comprehension,
    r#"
class A: ...
class B(A): ...
class X: ...
class Y(X): ...
xs: dict[A, X] = {B(): Y() for _ in [0]}
"#,
);

testcase_with_bug!(
    "We should push context into generator expressions",
    test_context_generator_expr,
    r#"
from typing import Generator, Iterable
class A: ...
class B(A): ...
x0 = ([B()] for _ in [0])
x1a: Generator[list[A], None, None] = x0 # E: EXPECTED Generator[list[B], None, None] <: Generator[list[A], None, None]
x1b: Generator[list[A], None, None] = ([B()] for _ in [0])
x2a: Iterable[list[A]] = x0 # E: EXPECTED Generator[list[B], None, None] <: Iterable[list[A]]
x2b: Iterable[list[A]] = ([B()] for _ in [0])

# In theory, we should allow this, since the generator expression accepts _any_ send type,
# but both Mypy and Pyright assume that the send type is `None`.
x3: Generator[int, int, None] = (1 for _ in [1]) # E: Generator[Literal[1], None, None] <: Generator[int, int, None]

x4: Generator[int, None, int] = (1 for _ in [1]) # E: Generator[Literal[1], None, None] <: Generator[int, None, int]
"#,
);

testcase!(
    test_context_if_expr,
    r#"
class A: ...
class B(A): ...
def cond() -> bool: ...
xs: list[A] = [B()] if cond() else [B()]
"#,
);

// Still infer types for unreachable branches (and find errors in them),
// but don't propagate them to the result.
testcase!(
    test_context_if_expr_unreachable,
    r#"
class A: ...
class B(A): ...
def takes_int(x: int) -> None: ...
xs: list[A] = [B()] if True else takes_int("") # E: EXPECTED Literal[''] <: int
ys: list[A] = takes_int("") if False else [B()] # E: EXPECTED Literal[''] <: int
"#,
);

testcase!(
    test_context_yield,
    r#"
from typing import Generator, Iterator
class A: ...
class B(A): ...
def gen() -> Generator[list[A], None, None]:
    yield [B()]
def iter() -> Iterator[list[A]]:
    yield [B()]
"#,
);

testcase!(
    test_context_lambda_return,
    r#"
from typing import Callable
class A: ...
class B(A): ...
f: Callable[[], list[A]] = lambda: [B()]
"#,
);

// We want to contextually type lambda params even when there is an arity mismatch.
testcase!(
    test_context_lambda_arity,
    r#"
from typing import Callable
f: Callable[[int], None] = lambda x, y: None # E: EXPECTED (x: int, y: Unknown) -> None <: (int) -> None
g: Callable[[int, int], None] = lambda x: None # E: EXPECTED (x: int) -> None <: (int, int) -> None
"#,
);

// This case is tricky. The call to `f` uses `g` to determine the paramspec `P`
// We then use `P` to contextually type the lambda. Importantly, the lambda's params
// need to match, including stuff like parameter name.
testcase!(
    test_context_lambda_paramspec,
    r#"
from typing import Callable, reveal_type

def f[**P, R](f: Callable[P, R], g: Callable[P, R]) -> Callable[P, R]:
    ...

def g(x: int, y: str):
    pass

x1 = f(g, lambda x, y: None)
reveal_type(x1) # E: revealed type: (x: int, y: str) -> None

x2 = f(g, lambda x, z: None) # E: EXPECTED (x: int, z: Unknown) -> None <: (x: int, y: str) -> None
reveal_type(x2) # E: revealed type: (x: int, y: str) -> None
"#,
);

testcase_with_bug!(
    "TODO: We do not currently validate assignments in multi-target assigns. Depending how we fix it, contextual typing may not work",
    test_context_assign_unpacked_tuple,
    r#"
class A: ...
class B(A): ...

xs: list[A] = []
(xs, _) = ([B()], None)
"#,
);

testcase_with_bug!(
    "TODO: No context propagated to subscript assignment target",
    test_context_assign_subscript,
    r#"
class A: ...
class B(A): ...

xs: list[list[A]] = [[]]
xs[0] = [B()] # E: EXPECTED list[B] <: list[A]
"#,
);

testcase_with_bug!(
    "Context should be propagated to argument",
    test_generic_get_literal,
    r#"
from typing import assert_type, TypeVar, Literal

class Foo[T]:
    def __init__(self, x: T) -> None: ...
    def get(self) -> T: ...

# Should propagate the context to the argument 42
x: Foo[Literal[42]] = Foo(42)  # E: EXPECTED Foo[int] <: Foo[Literal[42]]
assert_type(x.get(), Literal[42])
"#,
);
