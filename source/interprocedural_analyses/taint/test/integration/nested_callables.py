# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source


def foo():
    def inner():
        x = __test_source()
        __test_sink(x)

    def inner_with_model():
        return __test_source()


def outer(x: int) -> None:
    def inner(x: int) -> None:
        __test_sink(x)

    return inner(x)


def call_outer() -> None:
    outer(__test_source())
