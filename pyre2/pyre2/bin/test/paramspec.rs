/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

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
