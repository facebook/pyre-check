# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
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
