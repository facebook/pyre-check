# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def first_source():
    pass


def second_source():
    pass


def multi_sink(first, second):
    pass


def first_source_distance_zero():
    return first_source()


def first_source_distance_one():
    return first_source_distance_zero()  # Should be dropped.


def second_source_distance_zero():
    return second_source()


def second_source_distance_one():
    return second_source_distance_zero()  # Should be dropped.


def test_first_source_zero_second_source_zero():
    multi_sink(first_source(), second_source())


def test_first_source_one_second_source_one():
    multi_sink(first_source_distance_zero(), second_source_distance_zero())


def test_first_source_two_second_source_two():
    # Should be dropped.
    multi_sink(first_source_distance_one(), second_source_distance_one())


def sink_distance_zero(x):
    multi_sink(first_source(), x)


def sink_distance_one(x):
    sink_distance_zero(x)  # Should be dropped.


def test_sink_distance_one(x):
    sink_distance_zero(second_source())


def test_sink_distance_two(x):
    sink_distance_one(second_source())  # Should be dropped.


def test_sink_distance_one_second_source_two(x):
    sink_distance_zero(second_source_distance_one())  # Should be dropped.
