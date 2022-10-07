# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def tito(arg):
    return arg


def transform_x(arg):
    pass


def transform_y(arg):
    pass


def transform_z(arg):
    pass


def transform_yz(arg):
    arg1 = tito(arg)
    arg2 = transform_y(arg1)
    arg3 = tito(arg2)
    arg4 = transform_z(arg3)
    return arg4


def nested_transform_x(arg):
    return transform_x(arg)


def double_nested_transform_x(arg):
    return nested_transform_x(arg)


def sequential_tito_forward():
    x0 = _test_source()
    x1 = nested_transform_x(x0)
    x2 = tito(x1)
    x3 = transform_yz(x2)
    return x3


def sequential_tito_backward(arg):
    arg1 = nested_transform_x(arg)
    arg2 = tito(arg1)
    arg3 = transform_yz(arg2)
    _test_sink(arg3)


def branch_tito_forward():
    x0 = _test_source()
    if 1 == 1:
        x1 = nested_transform_x(x0)
    else:
        x1 = transform_y(x0)
    return x1


def branch_tito_backward(arg):
    if 1 == 1:
        arg1 = nested_transform_x(arg)
    else:
        arg1 = transform_y(arg)
    _test_sink(arg1)


def loop_tito_forward():
    x0 = _test_source()
    while 1:
        x0 = transform_x(x0)
    return x0


def loop_tito_backward(arg):
    while 1:
        arg = transform_x(arg)
    _test_sink(arg)
