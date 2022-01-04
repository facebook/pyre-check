# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import Awaitable, Callable


def with_logging_with_helper(f: Callable[[int], None]) -> Callable[[int], None]:
    def some_helper(x: int) -> None:
        print(x)
        eval(x)

    def inner(x: int) -> None:
        eval(x)
        f(x)
        some_helper(x)

    return inner


def with_logging_without_helper(f: Callable[[int], None]) -> Callable[[int], None]:
    def inner(x: int) -> None:
        eval(x)
        f(x)

    return inner
