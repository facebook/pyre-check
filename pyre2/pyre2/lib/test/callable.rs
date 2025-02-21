/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_lambda,
    r#"
from typing import Callable, reveal_type
f1 = lambda x: 1
reveal_type(f1)  # E: revealed type: (x: Unknown) -> Literal[1]
f2 = lambda x: reveal_type(x)  # E: revealed type: Unknown
f3: Callable[[int], int] = lambda x: 1
reveal_type(f3)  # E: revealed type: (x: int) -> Literal[1]
f4: Callable[[int], None] = lambda x: reveal_type(x)  # E: revealed type: int
f5: Callable[[int], int] = lambda x: x
f6: Callable[[int], int] = lambda x: "foo"  # E: EXPECTED (x: int) -> Literal['foo'] <: (int) -> int
f7: Callable[[int, int], int] = lambda x: 1  # E: EXPECTED (x: int) -> Literal[1] <: (int, int) -> int
f8: Callable[[int], int] = lambda x: x + "foo" # E: EXPECTED Literal['foo'] <: int
"#,
);

testcase!(
    test_callable_ellipsis_upper_bound,
    r#"
from typing import Callable
def test(f: Callable[[int, str], None]) -> Callable[..., None]:
    return f
"#,
);

testcase!(
    test_callable_ellipsis_lower_bound,
    r#"
from typing import Callable
def test(f: Callable[..., None]) -> Callable[[int, str], None]:
    return f
"#,
);

testcase!(
    test_callable_unparameterized,
    r#"
from typing import Callable, assert_type, Any
def test(f: Callable):
    assert_type(f, Callable[..., Any])
"#,
);

testcase!(
    test_callable_annot_too_few_args,
    r#"
from typing import Callable
def test(f: Callable[[int], None]):
    f() # E: Expected 1 more positional argument
"#,
);

testcase!(
    test_callable_annot_too_many_args,
    r#"
from typing import Callable
def test(f: Callable[[], None]):
    f(
      1, # E: Expected 0 positional arguments
      2
    )
"#,
);

testcase!(
    test_callable_annot_keyword_args,
    r#"
from typing import Callable
def test(f: Callable[[], None]):
    f(
      x=1, # E: Unexpected keyword argument `x`
      y="hello" # E: Unexpected keyword argument `y`
    )
"#,
);

testcase!(
    test_callable_ellipsis_keyword_args,
    r#"
from typing import Callable
def test(f: Callable[..., None]):
    f(x=1, y="hello") # OK
"#,
);

testcase!(
    test_callable_annot_upper_bound,
    r#"
from typing import Callable
def test(f: Callable[[int, int], None]) -> None: ...

def f1(x: int, y: int) -> None: ...
test(f1) # OK

# Lower bound has too many args
def f2(x: int, y: int, z: int) -> None: ...
test(f2) # E: EXPECTED (x: int, y: int, z: int) -> None <: (int, int) -> None

# Lower bound has too few args
def f3(x: int) -> None: ...
test(f3) # E: EXPECTED (x: int) -> None <: (int, int) -> None

# Lower bound has wrong arg types
def f4(x: str, y: int) -> None: ...
test(f4) # E: EXPECTED (x: str, y: int) -> None <: (int, int) -> None

# Lower bound has variadic args of compatible type
def f5(*args: int) -> None: ...
test(f5) # OK

# Lower bound has variadic args of incompatible type
def f6(*args: str) -> None: ...
test(f6) # E: EXPECTED (*str) -> None <: (int, int) -> None

# Lower bound has extra kwargs of arbitrary type
class Arbitrary: pass
def f7(x: int, y: int, **kwargs: Arbitrary) -> None: ...
test(f7) # OK

# Lower bound has extra args with defaults
def f7(x: int, y: int, z: int = 0) -> None: ...
test(f7) # OK
"#,
);

testcase!(
    test_positional_param_keyword_arg,
    r#"
def test(x: int, y: str): ...
test(1, "hello") # OK
test(x=1, y="hello") # OK
test(y="hello", x=1) # OK
test(1, y="hello") # OK
test(1) # E: Missing argument `y`
test(1, "hello", x=2) # E: Multiple values for argument `x`
"#,
);

testcase!(
    test_positional_only_params,
    r#"
def test(x: int, y: str, /): ...
test(1, "hello") # OK
test(1) # E: Expected 1 more positional argument
test(1, y="hello") # E: Expected 1 more positional argument # E: Unexpected keyword argument `y`
test(1, "hello", 2) # E: Expected 2 positional arguments, got 3
"#,
);

testcase!(
    test_keyword_only_params,
    r#"
def test(*, x: int, y: str): ...
test(x=1, y="hello") # OK
test(1, "hello") # E: Expected 0 positional arguments, got 2 # E: Missing argument `x` # E: Missing argument `y`
test(x=1) # E: Missing argument `y`
test(y="hello") # E: Missing argument `x`
"#,
);

testcase!(
    test_varargs,
    r#"
def test(*args: int): ...
test(1, 2, "foo", 4) # E: EXPECTED Literal['foo'] <: int
"#,
);

testcase!(
    test_kwargs,
    r#"
def test(**kwargs: int): ...
test(x=1, y="foo", z=2) # E: EXPECTED Literal['foo'] <: int
"#,
);

testcase!(
    test_defaults,
    r#"
def test(x: int, y: int = 0, z: str = ""): ...
test() # E: Missing argument `x`
test(0, 1) # OK
test(0, 1, "foo") # OK
test(0, 1, "foo", 2) # E: Expected 3 positional arguments
"#,
);

testcase!(
    test_defaults_posonly,
    r#"
def test(x: int, y: int = 0, z: str = "", /): ...
test() # E: Expected 1 more positional argument
test(0, 1) # OK
test(0, 1, "foo") # OK
test(0, 1, "foo", 2) # E: Expected 3 positional arguments
"#,
);

testcase!(
    test_default_ellipsis,
    r#"
def stub(x: int = ...): ... # OK
def err(x: int = ...): pass # E: EXPECTED Ellipsis <: int
"#,
);

testcase!(
    test_splat_tuple,
    r#"
def test(x: int, y: int, z: int): ...
test(*(1, 2, 3)) # OK
test(*(1, 2)) # E: Missing argument `z`
test(*(1, 2, 3, 4)) # E: Expected 3 positional arguments, got 4
"#,
);

testcase!(
    test_splat_iterable,
    r#"
def test(x: int, y: int, z: int): ...
test(*[1, 2, 3]) # OK
test(*[1, 2]) # OK
test(*[1, 2, 3, 4]) # OK
test(*[1], 2) # E: Expected 3 positional arguments, got 4
test(1, 2, 3, *[4]) # OK
"#,
);

testcase!(
    test_splat_union,
    r#"
from typing import Iterable

def test(x: int, y: int, z: int): ...

def fixed_same_len_ok(xs: tuple[int, int, int] | tuple[int, int, int]):
    test(*xs) # OK

def fixed_same_len_type_err(xs: tuple[int, int, int] | tuple[int, int, str]):
    test(*xs) # E: EXPECTED int | str <: int

def fixed_same_len_too_few(xs: tuple[int, int] | tuple[int, int]):
    test(*xs) # E: Missing argument `z`

def fixed_diff_len(xs: tuple[int, int] | tuple[int, int, int]):
    test(*xs) # OK (treated as Iterable[int])

def mixed_same_type(xs: tuple[int, int] | Iterable[int]):
    test(*xs) # OK (treated as Iterable[int])

def mixed_type_err(xs: tuple[int, int] | Iterable[str]):
    test(*xs) # E: EXPECTED int | str <: int
"#,
);

// Normally, positional arguments can not come after keyword arguments. Splat args are an
// exception. However, splat args are still evaluated first, so they consume positional params
// before any keyword arguments.
// See https://github.com/python/cpython/issues/104007
testcase!(
    test_splat_keyword_first,
    r#"
def test(x: str, y: int, z: int): ...
test(x="", *(0, 1)) # E: EXPECTED Literal[0] <: str # E: Multiple values for argument `x` # E: Missing argument `z`
"#,
);

testcase!(
    test_splat_kwargs,
    r#"
def f(x: int, y: int, z: int): ...
def test(kwargs: dict[str, int]):
    f(**kwargs) # OK
    f(1, **kwargs) # OK
"#,
);

testcase!(
    test_splat_kwargs_mixed_with_keywords,
    r#"
def f(x: str, y: int, z: int): ...
def test(kwargs: dict[str, int]):
    f("foo", **kwargs) # OK
    f(x="foo", **kwargs) # OK
    f(**kwargs) # E: EXPECTED int <: str
"#,
);

testcase!(
    test_splat_kwargs_multi,
    r#"
def f(x: int, y: int, z: int): ...
def test(kwargs1: dict[str, int], kwargs2: dict[str, str]):
    f(**kwargs1, **kwargs2) # E: EXPECTED str <: int
"#,
);

testcase!(
    test_splat_kwargs_mapping,
    r#"
from typing import Mapping
def f(x: int, y: int, z: int): ...
def test(kwargs: Mapping[str, int]):
    f(**kwargs) # OK
"#,
);

testcase!(
    test_splat_kwargs_subclass,
    r#"
class Counter[T](dict[T, int]): ...
def f(**kwargs: int): ...
def test(c: Counter[str]):
    f(**c)
"#,
);

testcase!(
    test_splat_kwargs_wrong_key,
    r#"
def f(x: int): ...
def test(kwargs: dict[int, str]):
    f(**kwargs) # E: Expected argument after ** to have `str` keys, got: int # E: Missing argument `x`
"#,
);

testcase!(
    test_splat_kwargs_to_kwargs_param,
    r#"
def f(**kwargs: int): ...
def g(**kwargs: str): ...
def test(kwargs: dict[str, int]):
    f(**kwargs) # OK
    g(**kwargs) # E: EXPECTED int <: str
"#,
);

testcase!(
    test_callable_async,
    r#"
from typing import Any, Awaitable, Callable, Coroutine

async def f(x: int) -> int: ...
def test_corountine() -> Callable[[int], Coroutine[Any, Any, int]]:
    return f
def test_awaitable() -> Callable[[int], Awaitable[int]]:
    return f
def test_sync() -> Callable[[int], int]:
    return f  # E: (x: int) -> Coroutine[Unknown, Unknown, int] <: (int) -> int
"#,
);

testcase!(
    test_assignability_both_typed_dicts,
    r#"
from typing import TypedDict, Unpack, Protocol
class TD1(TypedDict):
    x: int
class TD2(TypedDict):
    x: int
    y: int
class P1(Protocol):
    def __call__(self, **kwargs: Unpack[TD1]): ...
class P2(Protocol):
    def __call__(self, **kwargs: Unpack[TD2]): ...
def test(accept_td1: P1, accept_td2: P2):
    a: P1 = accept_td2  # E: EXPECTED P2 <: P1
    b: P2 = accept_td1
"#,
);

testcase!(
    test_assignability_one_typed_dict,
    r#"
from typing import TypedDict, Unpack, NotRequired, Protocol
class TD(TypedDict):
    string: str
    number: NotRequired[int]
class P1(Protocol):
    def __call__(self, **kwargs: Unpack[TD]): ...
class P2(Protocol):
    def __call__(self, *, string: str, number: int = ...): ...
class P3(Protocol):
    def __call__(self, string: str, number: int = ...): ...
def test(accept_td: P1, kwonly_args: P2, regular_args: P3):
    a: P2 = accept_td
    b: P3 = accept_td  # E: EXPECTED P1 <: P3
    c: P1 = kwonly_args  # E: EXPECTED P2 <: P1
    d: P1 = regular_args   # E: EXPECTED P3 <: P1
"#,
);

testcase!(
    test_assignability_typed_dict_and_regular_kwargs,
    r#"
from typing import TypedDict, Unpack, NotRequired, Protocol
class TD(TypedDict):
    string: str
    number: NotRequired[int]
class P1(Protocol):
    def __call__(self, **kwargs): ...
class P2(Protocol):
    def __call__(self, **kwargs: Unpack[TD]): ...
class P3(Protocol):
    def __call__(self, **kwargs: int | str): ...
class P4(Protocol):
    def __call__(self, **kwargs: int): ...
def test(unannotated: P1, unpacked: P2, annotated: P3, annotated_wrong: P4):
    a: P2 = unannotated
    b: P2 = annotated
    c: P2 = annotated_wrong  # E: EXPECTED P4 <: P2
"#,
);

testcase!(
    test_assignability_typed_dict_wrong_kwarg,
    r#"
from typing import TypedDict, Protocol, Required, NotRequired, Unpack
class TD(TypedDict):
    v1: Required[int]
    v2: NotRequired[str]
    v3: Required[str]
def func1(**kwargs: Unpack[TD]) -> None: ...
class P1(Protocol):
    def __call__(self, *, v1: int, v2: int, v3: str) -> None:...
class P2(Protocol):
    def __call__(self, *, v1: int) -> None: ...
class P3(Protocol):
    def __call__(self, *, v1: int, v2: str, v4: str) -> None: ...
x: P1 = func1  # E: EXPECTED (**Unpack[TypedDict[TD]]) -> None <: P1
y: P2 = func1  # E: EXPECTED (**Unpack[TypedDict[TD]]) -> None <: P2
z: P3 = func1  # E: EXPECTED (**Unpack[TypedDict[TD]]) -> None <: P3
"#,
);

testcase!(
    test_function_vs_callable,
    r#"
from typing import assert_type, Callable
def f(x: int) -> int:
    return x
# This assertion (correctly) fails because x is a positional parameter rather than a positional-only one.
# This test verifies that we produce a sensible error message that shows the mismatch.
assert_type(f, Callable[[int], int])  # E: assert_type((x: int) -> int, (int) -> int) failed
    "#,
);
