# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


def bar():
    return __test_source()


def qux(arg):
    __test_sink(arg)


def bad(ok, arg):
    qux(arg)


def some_source():
    return bar()


def match_flows():
    x = some_source()
    bad(5, x)
