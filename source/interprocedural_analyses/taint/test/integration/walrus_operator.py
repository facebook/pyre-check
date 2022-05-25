# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source


def walrus_operator_forward():
    (x := _test_source())
    (x := 1)
    (y := _test_source())
    return x, y


def walrus_operator_backward(x):
    (y := (y := x))
    (x := 1)
    return x, y
