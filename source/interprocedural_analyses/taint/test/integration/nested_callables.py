# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
import typing
from builtins import _test_sink, _test_source


def foo():
    def inner():
        x = _test_source()
        _test_sink(x)

    def inner_with_model():
        return _test_source()


def outer(x: int) -> None:
    def inner(x: int) -> None:
        _test_sink(x)

    return inner(x)


def call_outer() -> None:
    outer(_test_source())


def some_sink(x: int) -> None:
    _test_sink(x)


def outer_calling_other_function(x: int) -> None:
    def inner_calling_other_function(x: int) -> None:
        some_sink(x)

    inner_calling_other_function(x)


def parameter_function(
    add: typing.Optional[typing.Callable[[str, str], str]], x: str
) -> str:
    if add is None:

        def add(x: str, y: str) -> str:
            return x + y

    # pyre-ignore
    return add("/bin/bash", x)


def duplicate_function():
    foo()


def duplicate_function():
    foo()


g = None


def nested_global_function(x: str) -> str:
    global g

    def g(x: str, y: str) -> str:
        return x + y

    return g("/bin/bash", x)


def access_variables_in_outer_scope():
    x = _test_source()

    def inner():
        # TODO(T123114236): We should find an issue here
        _test_sink(x)
