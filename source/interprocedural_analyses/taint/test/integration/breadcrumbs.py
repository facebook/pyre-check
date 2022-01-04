# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def int_source() -> int:
    return _test_source()


def float_source() -> float:
    return _test_source()


def bool_source() -> bool:
    return _test_source()


def int_parameter(x, y: int):
    _test_sink(y)


def float_parameter(x, y: float):
    _test_sink(y)


def bool_parameter(x, y: bool):
    _test_sink(y)
