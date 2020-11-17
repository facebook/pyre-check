# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


def int_source() -> int:
    return __test_source()


def float_source() -> float:
    return __test_source()


def bool_source() -> bool:
    return __test_source()


def int_parameter(x, y: int):
    __test_sink(y)


def float_parameter(x, y: float):
    __test_sink(y)


def bool_parameter(x, y: bool):
    __test_sink(y)
