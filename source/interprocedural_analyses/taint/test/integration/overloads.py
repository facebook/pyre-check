# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import Union, overload


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
