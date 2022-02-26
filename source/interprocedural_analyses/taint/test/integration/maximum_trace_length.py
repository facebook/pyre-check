# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source, _tito


def source_distance_zero():
    return _test_source()


def source_distance_one():
    return source_distance_zero()


def source_distance_two():
    # Ignored because too far.
    return source_distance_one()


def sink_distance_zero(x):
    _test_sink(x)


def sink_distance_one(x):
    sink_distance_zero(x)


def sink_distance_two(x):
    # Ignored because too far.
    sink_distance_one(x)


def issue_source_zero_sink_zero():
    sink_distance_zero(source_distance_zero())


def issue_source_one_sink_zero():
    sink_distance_zero(source_distance_one())


def issue_source_one_sink_one():
    sink_distance_one(source_distance_one())


def issue_source_two_sink_one():
    # Ignored because too far.
    sink_distance_one(source_distance_two())


def issue_source_one_sink_two():
    # Ignored because too far.
    sink_distance_two(source_distance_one())


def multi_sink(x):
    y = _tito(x, x.foo)
    sink_distance_one(y)
