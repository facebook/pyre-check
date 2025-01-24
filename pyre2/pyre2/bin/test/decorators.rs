/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_simple_function_decorator,
    r#"
from typing import assert_type, Callable, Any

def decorator(f: Callable[[int], int]) -> int: ...

@decorator
def decorated(x: int) -> int:
   return x

assert_type(decorated, int)
    "#,
);

testcase_with_bug!(
    "The logic tested here passes, but assert_type is running into a problem we need to debug.",
    test_identity_function_decorator,
    r#"
from typing import assert_type, Callable, Any

def decorator[T](f: T) -> T: ...

@decorator
def decorated(x: int) -> str:
   return f"{x}"

assert_type(decorated, Callable[[int], str])  # E: assert_type(Callable[[int], str], Callable[[int], str])
    "#,
);

testcase!(
    test_signature_modifying_function_decorator,
    r#"
from typing import assert_type, Callable, Any

def decorator[T, R](f: Callable[[T], R]) -> Callable[[T, T], R]: ...

@decorator
def decorated(x: int) -> str:
   return f"{x}"

assert_type(decorated, Callable[[int, int], str])
    "#,
);

testcase!(
    test_chaining_decorators,
    r#"
from typing import assert_type, Callable, Any

def decorator0[T, R](f: Callable[[T], R]) -> Callable[[T], set[R]]: ...

def decorator1[T, R](f: Callable[[T], R]) -> Callable[[T], list[R]]: ...

@decorator1
@decorator0
def decorated(x: int) -> str:
   return f"{x}"

assert_type(decorated, Callable[[int], list[set[str]]])
    "#,
);

testcase_with_bug!(
    "TODO: why don't the asserted types match?",
    test_callable_instance,
    r#"
from typing import assert_type, Callable
class im_callable:
    def __call__[T](self, arg: T, /) -> T: ...
@im_callable()
def f(x: int) -> int:
    return x
assert_type(f, Callable[[int], int])  # E: assert_type(Callable[[int], int], Callable[[int], int])
    "#,
);

// This test case does not directly use a decorator, but it verifies our
// handling of the `@final` decorator applied to `typing.TypeVar`, which can
// trigger recursion that breaks legacy type parameter handling if we are not
// careful.
testcase_with_bug!(
    "There is a recursion bug we need to fix with @final on TypeVar",
    test_that_final_decorator_on_type_var_works,
    r#"
from typing import MutableSequence
x: MutableSequence[int]  # E: Expected 0 type arguments for class `MutableSequence`, got 1.
    "#,
);
