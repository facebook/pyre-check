/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

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
      x=1, # E: Unexpected keyword argument 'x'
      y="hello" # E: Unexpected keyword argument 'y'
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
test(f2) # E: EXPECTED Callable[[int, int, int], None] <: Callable[[int, int], None]

# Lower bound has too few args
def f3(x: int) -> None: ...
test(f3) # E: EXPECTED Callable[[int], None] <: Callable[[int, int], None]

# Lower bound has wrong arg types
def f4(x: str, y: int) -> None: ...
test(f4) # E: EXPECTED Callable[[str, int], None] <: Callable[[int, int], None]

# Lower bound has variadic args of compatible type
def f5(*args: int) -> None: ...
test(f5) # OK

# Lower bound has variadic args of incompatible type
def f6(*args: str) -> None: ...
test(f6) # E: EXPECTED Callable[[Var[str]], None] <: Callable[[int, int], None]

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
test(1) # E: Missing argument 'y'
test(1, "hello", x=2) # E: Multiple values for argument 'x'
"#,
);

testcase!(
    test_positional_only_params,
    r#"
def test(x: int, y: str, /): ...
test(1, "hello") # OK
test(1) # E: Expected 1 more positional argument
test(1, y="hello") # E: Expected 1 more positional argument
test(1, "hello", 2) # E: Expected 2 positional arguments
"#,
);

testcase!(
    test_keyword_only_params,
    r#"
def test(*, x: int, y: str): ...
test(x=1, y="hello") # OK
test(1, "hello") # E: Expected 0 positional arguments
test(x=1) # E: Missing argument 'y'
test(y="hello") # E: Missing argument 'x'
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
test() # E: Missing argument 'x'
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
    test_callable_async,
    r#"
from typing import Any, Awaitable, Callable, Coroutine

async def f(x: int) -> int: ...
def test_corountine() -> Callable[[int], Coroutine[Any, Any, int]]:
    return f
def test_awaitable() -> Callable[[int], Awaitable[int]]:
    return f
def test_sync() -> Callable[[int], int]:
    return f  # E: Callable[[int], Coroutine[Unknown, Unknown, int]] <: Callable[[int], int]
"#,
);

// TODO
testcase_with_bug!(
    test_callable_param_spec,
    r#"
from typing import Callable, ParamSpec
P = ParamSpec("P")
def test(f: Callable[P, None]) -> Callable[P, None]:
    def inner(*args: P.args, **kwargs: P.kwargs) -> None:
        f(*args, **kwargs) # E: Answers::expr_infer wrong number of arguments to call
    return inner # E: EXPECTED Callable[[Var[Args[_]], KwArgs[Kwargs[_]]], None] <: Callable[ParamSpec(?_), None]
"#,
);
