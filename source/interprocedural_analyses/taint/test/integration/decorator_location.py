# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import __test_sink, __test_source
from functools import wraps
from typing import Awaitable, Callable


def with_logging(f: Callable[[int], None]) -> Callable[[int], None]:
    def some_helper(x: int) -> None:
        print(x)
        __test_sink(x)

    def inner(x: int) -> None:
        __test_sink(x)
        f(x)
        some_helper(x)

    return inner


def with_logging2(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        __test_sink(x)
        f(x)

    return inner


def skip_this_decorator(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        __test_sink(x)
        f(x)

    return inner


@with_logging
@with_logging2
def foo(x: int) -> None:
    __test_sink(x)


@skip_this_decorator
def bar(x: int) -> None:
    print(x)


@with_logging2
@skip_this_decorator
def baz(x: int) -> None:
    print(x)


def pass_local_variable_to_x(f: Callable) -> Callable:
    @wraps(f)
    def inner(request: str, *args, **kwargs) -> None:
        __test_sink(request)
        x = 42
        f(request, x, *args, **kwargs)

    return inner


@pass_local_variable_to_x
def handle_request(request: str, x: int, y: int) -> None:
    __test_sink(x)


class Foo:
    def return_source(self) -> int:
        return __test_source()


def identity(f: Callable) -> Callable:
    # The return type is wrongly written as `Callable`.
    @wraps(f)
    def inner(*args, **kwargs) -> Callable:
        return f(*args, **kwargs)

    return inner


@identity
def return_foo() -> Foo:
    return Foo()


def call_return_foo() -> None:
    foo = return_foo()
    __test_sink(foo.return_source())


def main() -> None:
    foo(__test_source())
    bar(__test_source())
    baz(__test_source())

    # No issue because this `x` is not passed to `handle_request`.
    handle_request("hello", __test_source(), 42)
    handle_request(__test_source(), 42, 42)
    call_return_foo()
