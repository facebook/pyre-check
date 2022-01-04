# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def test_from_1_to_0():
    x = 0
    change_arg0(x, _test_source())
    return x


def test_from_0_to_1():
    y = 0
    change_arg1(_test_source(), y)
    return y


def test_from_1_to_0_nested():
    x = {}
    change_arg0(x.foo, _test_source())
    return x.foo


def test_from_1_to_0_nested_distinct():
    x = {}
    change_arg0(x.foo, _test_source())
    return x.bar


def test_list_append():
    l = MyList()
    l.append(_test_source())
    return l


def wrapper_from_1_to_0(x, y):
    change_arg0(x, y)


def wrapper_from_0_to_1(x, y):
    change_arg1(x, y)


def to_sink_via_side_effect(y):
    x = {}
    change_arg0(x, y)
    _test_sink(x)


def dict_to_sink_via_side_effect(y):
    x = {}
    change_arg0(x["foo"], y)
    _test_sink(x)


# Mocks that have models


def change_arg0(arg0, arg1):
    ...


def change_arg1(arg0, arg1):
    ...


class MyList:
    def append(self, arg):
        pass
