# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source
from functools import wraps
from typing import Awaitable, Callable


def with_logging(f: Callable[[int], None]) -> Callable[[int], None]:
    def some_helper(x: int) -> None:
        print(x)
        _test_sink(x)

    def inner(x: int) -> None:
        _test_sink(x)
        f(x)
        some_helper(x)

    return inner


def with_logging2(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        _test_sink(x)
        f(x)

    return inner


def skip_this_decorator(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        _test_sink(x)
        f(x)

    return inner


def ignore_this_decorator(f: Callable[[int], None]) -> Callable[[int], None]:
    return f


def ignore_this_decorator_factory(add: int):
    def decorator(f: Callable[[int], None]) -> Callable[[int], None]:
        def inner(x: int) -> None:
            f(x + add)

        return inner

    return decorator


class ignore_this_decorator_class:
    def __call__(self, f: Callable[[int], None]) -> Callable[[int], None]:
        return f


@with_logging
@with_logging2
def decorated_logging_logging2(x: int) -> None:
    _test_sink(x)


@skip_this_decorator
def decorated_skip_this_decorator(x: int) -> None:
    _test_sink(x)


@with_logging2
@skip_this_decorator
def decorated_logging2_skip_this_decorator(x: int) -> None:
    _test_sink(x)


@ignore_this_decorator
def decorated_ignore_this_decorator(x: int) -> None:
    _test_sink(x)


@ignore_this_decorator_factory(1)
def decorated_ignore_this_decorator_factory(x: int) -> None:
    _test_sink(x)


@ignore_this_decorator_class()
def decorated_ignore_this_decorator_class(x: int) -> None:
    _test_sink(x)


@ignore_this_decorator
@skip_this_decorator
def decorated_ignore_then_skip_decorator(x: int) -> None:
    _test_sink(x)


@with_logging
@ignore_this_decorator
def decorated_logging_ignore_this_decorator(x: int) -> None:
    _test_sink(x)


def pass_local_variable_to_x(f: Callable) -> Callable:
    @wraps(f)
    def inner(request: str, *args, **kwargs) -> None:
        _test_sink(request)
        x = 42
        f(request, x, *args, **kwargs)

    return inner


@pass_local_variable_to_x
def handle_request(request: str, x: int, y: int) -> None:
    _test_sink(x)


class Foo:
    def return_source(self) -> int:
        return _test_source()


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
    _test_sink(foo.return_source())


def main() -> None:
    # Properly finds all issues.
    decorated_logging_logging2(_test_source())
    # Does NOT find the issue (false negative).
    decorated_skip_this_decorator(_test_source())
    # Finds the issue to the decorator but not the inner function.
    decorated_logging2_skip_this_decorator(_test_source())
    # Properly finds the issue.
    decorated_ignore_this_decorator(_test_source())
    # Properly finds the issue.
    decorated_ignore_this_decorator_factory(_test_source())
    # Properly finds the issue.
    decorated_ignore_this_decorator_class(_test_source())
    # does NOT find the issue (false negative).
    decorated_ignore_then_skip_decorator(_test_source())
    # Properly finds all issues.
    decorated_logging_ignore_this_decorator(_test_source())

    # No issue because this `x` is not passed to `handle_request`.
    handle_request("hello", _test_source(), 42)
    handle_request(_test_source(), 42, 42)
    call_return_foo()
