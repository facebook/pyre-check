/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Many of these tests come from <https://typing.readthedocs.io/en/latest/spec/generics.html#paramspec>.

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_callable_param_spec,
    r#"
from typing import Callable, ParamSpec
P = ParamSpec("P")
def test(f: Callable[P, None]) -> Callable[P, None]:
    def inner(*args: P.args, **kwargs: P.kwargs) -> None:
        f(*args, **kwargs)
    return inner
"#,
);

testcase!(
    test_param_spec_solve,
    r#"
from typing import Callable, ParamSpec, Concatenate, assert_type
def f1[**P](x: Callable[P, int]) -> Callable[P, int]: ...
def f2[**P](x: Callable[Concatenate[int, P], int]) -> Callable[P, int]: ...
def f3[**P](x: Callable[P, int]) -> Callable[Concatenate[int, P], int]: ...
def f4[**P](x: Callable[Concatenate[int, P], int]) -> Callable[Concatenate[str, P], int]: ...

def test(x1: Callable[[int, str], int], x2: Callable[..., int]):
    assert_type(f1(x1), Callable[[int, str], int])
    assert_type(f1(x2), Callable[..., int])

    assert_type(f2(x1), Callable[[str], int])
    assert_type(f2(x2), Callable[..., int])

    assert_type(f3(x1), Callable[[int, int, str], int])
    assert_type(f3(x2), Callable[Concatenate[int, ...], int])

    assert_type(f4(x1), Callable[[str, str], int])
    assert_type(f4(x2), Callable[Concatenate[str, ...], int])
"#,
);

testcase!(
    test_function_concatenate,
    r#"
from typing import Callable, ParamSpec, Concatenate
P = ParamSpec("P")
def f(t: Callable[Concatenate[int, P], int]):
    pass
"#,
);

testcase!(
    test_simple_paramspec,
    r#"
from typing import Callable, ParamSpec, assert_type, reveal_type

P = ParamSpec("P")

def changes_return_type_to_str(x: Callable[P, int]) -> Callable[P, str]: ...

def returns_int(a: str, b: bool) -> int: ...

f = changes_return_type_to_str(returns_int)
reveal_type(f)  # E: revealed type: (a: str, b: bool) -> str

f("A", True)
f(a="A", b=True)
f("A", "A")  # E: Literal['A'] <: bool
assert_type(f("A", True), str)
"#,
);

testcase_with_bug!(
    "ParamSpec types can only be used in argument positions or when a ParamSpec is expected by a generic",
    test_paramspec_in_right_place,
    r#"
from typing import Callable, Concatenate, ParamSpec

P = ParamSpec("P")

def foo(x: P) -> P: ...                           # Rejected
def foo(x: Concatenate[int, P]) -> int: ...       # Rejected
def foo(x: list[P]) -> None: ...                  # E: ParamSpec cannot be used for type parameter
def foo(x: Callable[[int, str], P]) -> None: ...  # Rejected
"#,
);

testcase!(
    test_paramspec_generic,
    r#"
from typing import Callable, Concatenate, ParamSpec, Generic, TypeVar

T = TypeVar("T")
P = ParamSpec("P")
P_2 = ParamSpec("P_2")

class X(Generic[T, P]):
  f: Callable[P, int]
  x: T

def f1(x: X[int, P_2]) -> str: ...                    # Accepted
def f2(x: X[int, Concatenate[int, P_2]]) -> str: ...  # Accepted
def f3(x: X[int, [int, bool]]) -> str: ...            # Accepted
def f4(x: X[int, ...]) -> str: ...                    # Accepted
def f5(x: X[int, int]) -> str: ...                    # E: Expected a valid ParamSpec expression

class X2[T, **P]:
  f: Callable[P, int]
  x: T

def f6(x: X2[int, [int, bool]]) -> str: ...           # Accepted
"#,
);

testcase!(
    test_paramspec_omit_brackets,
    r#"
from typing import Callable, Generic, ParamSpec

P = ParamSpec("P")

class Z(Generic[P]):
  f: Callable[P, int]

def f(x: Z[[int, str, bool]]) -> str: ...
def f(x: Z[int, str, bool]) -> str: ...
"#,
);

testcase!(
    test_paramspec_repeated,
    r#"
from typing import Callable, ParamSpec, reveal_type

P = ParamSpec("P")

def foo(x: Callable[P, int], y: Callable[P, int]) -> Callable[P, bool]: ...

def x_y(x: int, y: str) -> int: ...
def y_x(y: int, x: str) -> int: ...

reveal_type(foo(x_y, x_y)) # E: revealed type: (x: int, y: str) -> bool
               # Should return (x: int, y: str) -> bool
               # (a callable with two positional-or-keyword parameters)

foo(x_y, y_x) # E: EXPECTED (y: int, x: str) -> int <: (x: int, y: str) -> int
               # Could return (a: int, b: str, /) -> bool
               # (a callable with two positional-only parameters)
               # This works because both callables have types that are
               # behavioral subtypes of Callable[[int, str], int].
               # Could also fail, which is what we do.

def keyword_only_x(*, x: int) -> int: ...
def keyword_only_y(*, y: int) -> int: ...
foo(keyword_only_x, keyword_only_y) # Rejected # E: EXPECTED (*, y: int) -> int <: (*, x: int) -> int
"#,
);

testcase!(
    test_paramspec_constructor,
    r#"
from typing import TypeVar, Generic, Callable, ParamSpec, reveal_type

P = ParamSpec("P")
U = TypeVar("U")

class Y(Generic[U, P]):
  f: Callable[P, str]
  prop: U

  def __init__(self, f: Callable[P, str], prop: U) -> None:
    self.f = f
    self.prop = prop

def a(q: int) -> str: ...

reveal_type(Y(a, 1))   # E: revealed type: Y[int, (q: int)]
reveal_type(Y(a, 1).f) # E: revealed type: (q: int) -> str
"#,
);

// We have different formatting to what the spec suggests, but the same answers.
testcase!(
    test_simple_concatenate,
    r#"
from typing import Callable, Concatenate, ParamSpec, reveal_type

P = ParamSpec("P")

def bar(x: int, *args: bool) -> int: ...

def add(x: Callable[P, int]) -> Callable[Concatenate[str, P], bool]: ...

reveal_type(add(bar))       # Should return (a: str, /, x: int, *args: bool) -> bool # E: revealed type: (str, x: int, *bool) -> bool

def remove(x: Callable[Concatenate[int, P], int]) -> Callable[P, bool]: ...

reveal_type(remove(bar))    # Should return (*args: bool) -> bool # E: revealed type: (*bool) -> bool

def transform(
  x: Callable[Concatenate[int, P], int]
) -> Callable[Concatenate[str, P], bool]: ...

reveal_type(transform(bar)) # Should return (a: str, /, *args: bool) -> bool # E: revealed type: (str, *bool) -> bool
"#,
);

testcase_with_bug!(
    "P.args and P.kwargs should only work when P is in scope",
    test_paramspec_component_usage,
    r#"
from typing import Callable, ParamSpec

P = ParamSpec("P")

def puts_p_into_scope(f: Callable[P, int]) -> None:

  def inner(*args: P.args, **kwargs: P.kwargs) -> None:      # Accepted
    pass

  def mixed_up(*args: P.kwargs, **kwargs: P.args) -> None:   # Rejected
    pass

  def misplaced(x: P.args) -> None:                          # Rejected
    pass

def out_of_scope(*args: P.args, **kwargs: P.kwargs) -> None: # Rejected
  pass
"#,
);

testcase_with_bug!(
    "P.args and P.kwargs can't be used as annotations",
    test_paramspec_together,
    r#"
from typing import Callable, ParamSpec

P = ParamSpec("P")

def puts_p_into_scope(f: Callable[P, int]) -> None:

  stored_args: P.args                           # Rejected

  stored_kwargs: P.kwargs                       # Rejected

  def just_args(*args: P.args) -> None:         # E: ParamSpec *args and **kwargs must be used together
    pass

  def just_kwargs(**kwargs: P.kwargs) -> None:  # E: ParamSpec *args and **kwargs must be used together
    pass
"#,
);

testcase!(
    test_paramspec_decorator,
    r#"
from typing import Callable, ParamSpec, assert_type

P = ParamSpec("P")

def decorator(f: Callable[P, int]) -> Callable[P, None]:

  def foo(*args: P.args, **kwargs: P.kwargs) -> None:

    assert_type(f(*args, **kwargs), int)    # Accepted, should resolve to int

    f(*kwargs, **args)    # Rejected # E: Expected a `*args` and `**kwargs` for `ParamSpec`

    f(1, *args, **kwargs) # Rejected # E: Expected 0 positional arguments, got 1

  return foo              # Accepted
"#,
);

testcase!(
    test_paramspec_named_arguments_concatenate,
    r#"
from typing import Callable, ParamSpec

P = ParamSpec("P")

def outer(f: Callable[P, None]) -> Callable[P, None]:
  def foo(x: int, *args: P.args, **kwargs: P.kwargs) -> None:
    f(*args, **kwargs)

  def bar(*args: P.args, **kwargs: P.kwargs) -> None:
    foo(1, *args, **kwargs)   # Accepted
    foo(x=1, *args, **kwargs) # Rejected # E: Expected 1 more positional argument # E: Unexpected keyword argument `x`

  return bar
"#,
);

testcase!(
    test_paramspec_different_origins,
    r#"
from typing import Callable, ParamSpec

P1 = ParamSpec("P1")
P2 = ParamSpec("P2")

def foo(x: int, *args: P1.args, **kwargs: P2.kwargs) -> None: ...  # E: *args and **kwargs must come from the same ParamSpec
"#,
);

testcase_with_bug!(
    "Rejects everything",
    test_paramspec_twice,
    r#"
from typing import Callable, ParamSpec

P = ParamSpec("P")

def twice(f: Callable[P, int], *args: P.args, **kwargs: P.kwargs) -> int:
  return f(*args, **kwargs) + f(*args, **kwargs)

def a_int_b_str(a: int, b: str) -> int:
  return a

twice(a_int_b_str, 1, "A")       # Accepted # E: (a: int, b: str) -> int <: (*Unknown, **Unknown) -> int

twice(a_int_b_str, b="A", a=1)   # Accepted # E: (a: int, b: str) -> int <: (*Unknown, **Unknown) -> int

twice(a_int_b_str, "A", 1)       # Rejected # E: (a: int, b: str) -> int <: (*Unknown, **Unknown) -> int
"#,
);
