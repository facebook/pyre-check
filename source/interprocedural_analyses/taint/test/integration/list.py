# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source


def create_zipped_source():
    x = [_test_source(), 1]
    y = [2, 3]
    return zip(x, y)


def zipped_source_access_path():
    # TODO(T134884591): False positive
    x = [_test_source(), 1]
    y = [2, 3]
    return list(zip(x, y))[1][0]


def create_zipped_source_with_all_tainted():
    x = [_test_source()]
    y = [_test_source()]
    z = [_test_source()]
    return zip(x, y, z)


def zipped_element_to_sink(x):
    l1 = [x]
    l2 = [1]

    for x, y in zip(l1, l2):
        _test_sink(x)


def zipped_element_not_flowing_to_sink(x):
    l1 = [x]
    l2 = [1]

    for x, y in zip(l1, l2):
        _test_sink(y)


class Woot:
    def taint_self(self, item):
        ...


def push_pop_no_taint():
    x = []
    x.append(_test_source())
    x.pop()
    return x


def push_pop_taint():
    x = []
    x.append(_test_source())
    x.append(1)
    x.pop()
    return x


def setitem():
    x = [""] * 10
    x[2] = _test_source()
    _test_sink(x[2])
    _test_sink(x[3])
