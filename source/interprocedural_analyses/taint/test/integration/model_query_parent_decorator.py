# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink


def d1():
    pass


def d2():
    pass


class TestC:
    pass


@d1
class TestC_1(TestC):
    def __init__(self, foo, bar, baz):
        _test_sink(foo)
        _test_sink(bar)
        _test_sink(baz)


@d2
class TestC_2(TestC):
    def __init__(self, foo, bar, baz):
        _test_sink(foo)
        _test_sink(bar)
        _test_sink(baz)


@d1
@d2
class TestC_3(TestC):
    def __init__(self, foo, bar, baz):
        _test_sink(foo)
        _test_sink(bar)
        _test_sink(baz)


def setup():
    TestC_1(0, 0, 0)
    TestC_1(0, 0, 0)
    TestC_1(0, 0, 0)
    TestC_2(0, 0, 0)
    TestC_2(0, 0, 0)
    TestC_2(0, 0, 0)
    TestC_3(0, 0, 0)
    TestC_3(0, 0, 0)
    TestC_3(0, 0, 0)
