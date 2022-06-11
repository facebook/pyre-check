# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def forward():
    raise _test_sink(_test_source())


def backward(x):
    raise _test_sink(x)


def unreachable():
    x = _test_source()
    raise Exception()
    _test_sink(x)


def unreachable_through_function_call_sink():
    x = _test_source()
    no_sink(x)


def no_sink(x):
    raise Exception()
    _test_sink(x)


def no_source():
    raise Exception()
    return _test_source()


def unreachable_through_function_call_source():
    x = no_source()
    _test_sink(x)
