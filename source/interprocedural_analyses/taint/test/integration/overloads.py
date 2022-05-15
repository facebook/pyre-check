# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source
from typing import overload, Union


@overload
def f(x: int) -> None:
    pass


@overload
def f(x: str) -> None:
    pass


def f(x: Union[int, str]) -> None:
    call_me(x)


def call_me(x):
    _test_sink(x)


class A:
    def call_me(self, x):
        _test_sink(x)


@overload
def g(o: A) -> None:
    pass


@overload
def g(o: int) -> None:
    pass


def g(o):
    x = _test_source()
    if isinstance(o, A):
        o.call_me(x)  # Requires type refinement on `o`.
