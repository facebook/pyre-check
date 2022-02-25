# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source
from typing import Awaitable, Callable, TypeVar

from pyre_extensions import ParameterSpecification
from pyre_extensions.type_variable_operators import Concatenate

P = ParameterSpecification("P")


def with_logging(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        _test_sink(x)
        f(x)

    return inner


@with_logging
def foo(x: int) -> None:
    print(x)


def with_logging_no_sink(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        f(x)

    return inner


@with_logging_no_sink
def foo_with_sink(x: int) -> None:
    _test_sink(x)
    print(x)


async def with_logging_async(
    f: Callable[[str], Awaitable[None]]
) -> Callable[[str], Awaitable[None]]:
    async def inner(y: str) -> None:
        try:
            result = await f(y)
        except Exception:
            _test_sink(y)

    return inner


@with_logging_async
async def foo_async(x: str) -> None:
    print(x)


def with_logging_args_kwargs(f: Callable) -> Callable:
    def inner(*args, **kwargs) -> None:
        _test_sink(kwargs)
        f(*args, **kwargs)

    return inner


@with_logging_args_kwargs
def foo_args_kwargs(x: str) -> None:
    print(x)


def with_logging_args_kwargs_no_sink(f: Callable) -> Callable:
    def inner(*args, **kwargs) -> None:
        f(*args, **kwargs)

    return inner


@with_logging_args_kwargs_no_sink
def foo_args_kwargs_with_sink(x: str, y: int) -> None:
    _test_sink(y)


def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:
    def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

    return inner


def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:
    def inner(y: str) -> None:
        callable(y + _test_source())

    return inner


def fails_to_apply(f):
    return f


@fails_to_apply
@with_logging_source
@fails_to_apply
@with_logging_sink
@fails_to_apply
def foo_with_shady_decorators(z: str) -> None:
    print(z)


def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:
    def _inner_decorator(f: Callable) -> Callable:
        def inner(*args: object, **kwargs: object) -> None:
            print("Logging to:", logger_name)
            _test_sink(args)
            f(*args, **kwargs)

        return inner

    return _inner_decorator


@with_named_logger("foo_logger")
def foo_using_decorator_factory(x: str) -> None:
    print(x)


def with_logging_first_parameter(
    f: Callable[Concatenate[int, P], None]
) -> Callable[Concatenate[int, P], None]:
    def inner(first_parameter: int, *args: P.args, **kwargs: P.kwargs) -> None:
        if first_parameter != 42:
            _test_sink(first_parameter)
            return

        f(first_parameter, *args, **kwargs)

    return inner


@with_logging_first_parameter
def foo_log_first_parameter(x: int, y: str) -> None:
    print(x, y)


def with_logging_helper_functions(
    f: Callable[P, Awaitable[None]]
) -> Callable[P, Awaitable[None]]:
    async def inner(*args: P.args, **kwargs: P.kwargs) -> None:
        try:
            before(*args, **kwargs)
            await f(*args, **kwargs)
            after(*args, **kwargs)
        except Exception as exception:
            print(exception)

    def before(*args: object, **kwargs: object) -> None:
        print("before", args)

    def after(*args: object, **kwargs: object) -> None:
        print("after", kwargs)
        _test_sink(args)

    return inner


@with_logging_helper_functions
async def foo_with_helper_function(x: int, y: str) -> None:
    print(x, y)


T = TypeVar("T", bound="Foo")


class Foo:
    def sink_method(self, x: str) -> None:
        print(x)
        _test_sink(x)

    @with_logging_args_kwargs_no_sink
    def foo(self, x: str) -> None:
        self.sink_method(x)

    @with_logging_args_kwargs_no_sink
    @with_logging_args_kwargs
    @with_logging_args_kwargs_no_sink
    def bar(self, x: str) -> None:
        print(x)

    @with_logging_args_kwargs_no_sink
    def self_has_generic_type(self: T, other: T, x: str) -> None:
        other.bar(x)

    @classmethod
    @with_logging_args_kwargs_no_sink
    def some_class_method(cls, x: str) -> None:
        cls().sink_method(x)


async def main() -> None:
    foo(_test_source())
    foo_with_sink(_test_source())
    await foo_async(_test_source())

    foo_args_kwargs(_test_source())

    # No issue because the taint is on the second parameter.
    foo_args_kwargs_with_sink(_test_source(), 0)
    # Issue.
    foo_args_kwargs_with_sink("hello", _test_source())

    foo_with_shady_decorators("hello")

    foo_using_decorator_factory(_test_source())

    foo_log_first_parameter(_test_source(), "hello")

    foo_with_helper_function(_test_source(), "hello")

    Foo().foo(_test_source())

    Foo().bar(_test_source())

    Foo().self_has_generic_type(Foo(), _test_source())

    Foo.some_class_method(_test_source())
