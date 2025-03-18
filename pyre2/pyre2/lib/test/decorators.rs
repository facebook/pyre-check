/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

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

testcase!(
    test_identity_function_decorator,
    r#"
from typing import Any, Callable, reveal_type

def decorator[T](f: T) -> T: ...

@decorator
def decorated(x: int) -> str:
   return f"{x}"

reveal_type(decorated)  # E: revealed type: (x: int) -> str
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

testcase!(
    test_callable_instance,
    r#"
from typing import Callable, reveal_type
class im_callable:
    def __call__[T](self, arg: T, /) -> T: ...
@im_callable()
def f(x: int) -> int:
    return x
reveal_type(f)  # E: revealed type: (x: int) -> int
    "#,
);

// This test case does not directly use a decorator, but it verifies our
// handling of the `@final` decorator applied to `typing.TypeVar`, which can
// trigger recursion that breaks legacy type parameter handling if we are not
// careful.
testcase!(
    test_that_final_decorator_on_type_var_works,
    r#"
from typing import MutableSequence
x: MutableSequence[int]
    "#,
);

// A regression test for a bug where we were not correctly handling the anywhere
// type for a decorated function.
testcase!(
    test_decorator_general_type,
    r#"
from typing import assert_type, Callable

def decorator(f: Callable[[int], int]) -> int: ...

def anywhere():
    assert_type(decorated, int)

@decorator
def decorated(x: int) -> int:
   return x
    "#,
);

testcase!(
    test_classmethod_first_param,
    r#"
from typing import assert_type

class C:
    @classmethod
    def f(cls) -> int:
        return cls.g()

    @classmethod
    def g(cls) -> int:
        return 42

assert_type(C.f(), int)
assert_type(C.g(), int)
    "#,
);

testcase!(
    test_staticmethod_first_param,
    r#"
from typing import assert_type, Any

class C:
    @staticmethod
    def f(x):
        assert_type(x, Any)

    @staticmethod
    def g(x: int):
        return x

C.f(0)
assert_type(C.g(0), int)
    "#,
);

testcase!(
    test_final,
    r#"
from typing import final, reveal_type
@final
def f(x: int) -> int:
    return x
reveal_type(f)  # E: revealed type: (x: int) -> int
    "#,
);

testcase!(
    test_callable_class_as_decorator,
    r#"
import dataclasses
from typing import Callable, assert_type

@dataclasses.dataclass(frozen=True)
class decorator:
    metadata: int

    def __call__[TReturn, **TParams](
        self, func: Callable[TParams, TReturn]
    ) -> Callable[TParams, TReturn]:
        ...


class C:
    @decorator(42)
    def f(self, x: int) -> int:
        return x

assert_type(C().f(42), int)
    "#,
);

testcase!(
    test_decorate_to_any,
    r#"
from typing import Any, assert_type

def decorate(f) -> Any: ...

class C:
    @decorate
    def f(self, x: int): ...

# `f` is `Any` now, we should be able to call it with anything and get back `Any`.
assert_type(C().f("any", b"thing"), Any)
    "#,
);

testcase!(
    test_decorate_to_generic_callable,
    r#"
from typing import Any, Callable, TypeVar, assert_type
T = TypeVar('T')

def decorate(f) -> Callable[[Any, T], T]:
    return lambda _, x: x

class C:
    @decorate
    def f(self): ...

assert_type(C().f(0), int)
    "#,
);

testcase!(
    test_decorate_generic_function,
    r#"
from typing import assert_type

def decorate[T](f: T) -> T:
    return f

class C:
    @decorate
    def f[T](self, x: T) -> T:
        return x

assert_type(C().f(0), int)
    "#,
);
