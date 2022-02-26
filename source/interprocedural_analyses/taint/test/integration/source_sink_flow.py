# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def bar():
    return _test_source()


def qux(arg):
    _test_sink(arg)


def bad(ok, arg):
    qux(arg)


def some_source():
    return bar()


def match_flows():
    x = some_source()
    bad(5, x)
