# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink, _test_source


def source_distance_one():
    return _test_source()


def source_distance_two():
    return source_distance_one()


def sink_distance_one(x):
    _test_sink(x)


def sink_distance_two(x):
    sink_distance_one(x)


def sink_distance_three(x):
    sink_distance_two(x)


def test_source_one_sink_one():
    sink_distance_one(source_distance_one())


def test_source_two_sink_one():
    # Should be ignored, source distance is > 1
    sink_distance_one(source_distance_two())


def test_source_one_sink_two():
    sink_distance_two(source_distance_one())


def test_source_two_sink_two():
    # Should be ignored, source distance is > 1
    sink_distance_two(source_distance_two())


def test_source_one_sink_three():
    # Should be ignored, sink distance is > 2
    sink_distance_three(source_distance_one())
