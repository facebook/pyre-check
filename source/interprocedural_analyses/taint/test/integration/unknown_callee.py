# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def test_issue(o):
    x = _test_source()
    y = o.method(x)
    _test_sink(y)


def test_collapse_source(o):
    x = {"a": _test_source()}
    y = o.method(x)
    _test_sink(y["b"])


def test_sink_collapse(arg, o):
    x = o.method(arg)
    _test_sink(x["a"])


def should_collapse_depth_zero(arg, o):
    return o.method(arg)


def test_collapse_depth():
    x = {"a": _test_source()}
    y = should_collapse_depth_zero(x, 0)
    _test_sink(y["b"])
