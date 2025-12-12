# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from pysa import _test_sink, _test_source
from typing import Awaitable, Callable, TypeVar, ParamSpec, Concatenate

P = ParamSpec("P")


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


def with_logging_async(
    f: Callable[[str], Awaitable[None]],
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
    f: Callable[Concatenate[int, P], None],
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
    f: Callable[P, Awaitable[None]],
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


TFoo = TypeVar("TFoo", bound="Foo")


class Foo:
    def sink_method(self, x: str) -> None:
        print(x)
        _test_sink(x)

    @with_logging_args_kwargs_no_sink
    def foo(self, x: str) -> None:
        self.sink_method(x)

    @with_logging_args_kwargs_no_sink
    @with_logging_args_kwargs
    def bar(self, x: str) -> None:
        print(x)

    @with_logging_args_kwargs_no_sink
    def self_has_generic_type(self: TFoo, other: TFoo, x: str) -> None:
        other.bar(x=x)  # Sink is on the keyword argument

    @classmethod
    @with_logging_args_kwargs_no_sink
    def some_class_method(cls, x: str) -> None:
        cls().sink_method(x)


async def main() -> None:
    foo(_test_source())
    foo_with_sink(_test_source())
    await foo_async(_test_source())

    foo_args_kwargs(x=_test_source())  # Sink is on the keyword argument

    # No issue because the taint is on the second parameter.
    foo_args_kwargs_with_sink(_test_source(), 0)
    # Issue.
    foo_args_kwargs_with_sink("hello", _test_source())

    foo_with_shady_decorators("hello")

    foo_using_decorator_factory(_test_source())

    foo_log_first_parameter(_test_source(), "hello")

    foo_with_helper_function(_test_source(), "hello")

    Foo().foo(_test_source())  # Sink is on the 1st argument (False negative)
    Foo().foo(x=_test_source())  # Expect an issue
    Foo.foo(Foo(), _test_source())  # Expect an issue

    Foo().bar(x=_test_source())  # Sink is on the keyword argument

    # Sink is on the keyword argument
    Foo().self_has_generic_type(other=Foo(), x=_test_source())

    Foo.some_class_method(
        _test_source()
    )  # Sink is on the 1st argument (False negative)


def discard_second_parameter_inner(first_parameter: int) -> None:
    return


def discard_second_parameter_non_inlineable(
    f: Callable[[int, str], None],
) -> Callable[[int], None]:
    # Return a function not defined here, to prevent from inlining decorators
    return discard_second_parameter_inner


@discard_second_parameter_non_inlineable
def second_parameter_source_with_non_inlineable_decorator(arg1: int, arg2: str) -> None:
    _test_sink(arg2)  # Issue here


def trivial_decorator(f: Callable[P, None]) -> Callable[P, None]:
    def inner(*args: P.args, **kwargs: P.kwargs) -> None:
        f(*args, **kwargs)

    return inner


@trivial_decorator
def second_parameter_source_inlineable_decorator(arg1: int, arg2: str) -> None:
    _test_sink(arg2)  # Issue here


@trivial_decorator
def second_parameter_source_inlineable_decorator_with_inner(
    arg1: int, arg2: str
) -> None:
    def inner():
        _test_sink(arg2)

    inner()  # Issue here


@trivial_decorator
def sink_via_trivial_decorator(x: str) -> None:
    _test_sink(x)


def issue_via_trivial_decorator() -> None:
    sink_via_trivial_decorator(_test_source())


def _strip_first_parameter_(
    f: Callable[Concatenate[int, P], None],
) -> Callable[P, None]:
    def inner(*args: P.args, **kwargs: P.kwargs) -> None:
        f(0, *args, **kwargs)

    return inner


@_strip_first_parameter_
def decorated(self, into_sink) -> None:
    _test_sink(into_sink)


def using_decorated(into_decorated):
    decorated(into_decorated)


T = TypeVar("T")


def no_op_decorator(f: T) -> T:
    return f


@no_op_decorator
def sink_via_no_op_decorator(x: str) -> None:
    _test_sink(x)


def issue_via_no_op_decorator() -> None:
    sink_via_no_op_decorator(_test_source())


# pyre-ignore
def no_op_decorator_factory(flag: bool) -> Callable[[T], T]:
    def inner(f: T) -> T:
        f.__doc___ = "dummy doc"  # pyrefly: ignore[missing-attribute]
        return f

    return inner


@no_op_decorator_factory(True)
def sink_via_no_op_decorator_factory(x: str) -> None:
    _test_sink(x)


def issue_via_no_op_decorator_factory() -> None:
    sink_via_no_op_decorator_factory(_test_source())


# pyre-ignore
def conditional_no_op_decorator_factory(flag: bool) -> Callable[[T], T]:
    if flag:
        def inner_true(f: T) -> T:
            return f

        return inner_true
    else:
        def inner_false(f: T) -> T:
            return f

        return inner_false


@conditional_no_op_decorator_factory(False)
def sink_via_conditional_no_op_decorator_factory(x: str) -> None:
    _test_sink(x)


def issue_via_conditional_no_op_decorator_factory():
    sink_via_conditional_no_op_decorator_factory(_test_source())


def conditional_decorator_factory(flag: bool) -> Callable[[Callable[[str], None]], Callable[[str], None]]:
    if flag:
        def add_sink(f: Callable[[str], None]) -> Callable[[str], None]:
            def inner(x: str) -> None:
                _test_sink(x)
                f(x)

            return inner

        return add_sink
    else:
        def identity(f: Callable[[str], None]) -> Callable[[str], None]:
            return f

        return identity


@conditional_decorator_factory(True)
def sink_via_conditional_decorator_factory(x: str) -> None:
    print(x)


def issue_via_conditional_decorator_factory():
    sink_via_conditional_decorator_factory(_test_source())


class StringWrapper:
    def __init__(self, value: str) -> None:
        self.value = value

    @no_op_decorator
    def __add__(self, other: "StringWrapper") -> "StringWrapper":
        _test_sink(self)
        _test_sink(other)
        return StringWrapper(self.value + other.value)


def issue_str_wrapper():
    x = StringWrapper(_test_source())
    x + StringWrapper("")


def add_return_source(callable: Callable[[], str]) -> Callable[[], str]:
    def inner() -> str:
        if 1 > 2:
            return callable()
        else:
            return _test_source()

    return inner


@add_return_source
def return_source_via_decorator() -> str:
    return ""


def test_parameter_default_value(x: str = return_source_via_decorator()) -> None:
    _test_sink(x)
