# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


def cross_repository_source(source_parameter):
    _test_sink(source_parameter)


def returns_crtex_source():
    pass


def test():
    s = returns_crtex_source()
    _test_sink(s)


def cross_repository_anchor_sink(sink_parameter):
    pass


def test_cross_repository_anchor():
    source = _test_source()
    cross_repository_anchor_sink(source)
