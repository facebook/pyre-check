# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import __test_sink, __test_source
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


@with_logging
@with_logging2
def foo(x: int) -> None:
    __test_sink(x)


def main() -> None:
    foo(__test_source())
