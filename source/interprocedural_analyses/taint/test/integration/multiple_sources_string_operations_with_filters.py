# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def first_source() -> str:
    return ""


def first_source_distance_zero() -> str:
    return first_source()


def first_source_distance_one() -> str:
    return first_source_distance_zero()  # Should be dropped.


def second_source_distance_zero() -> str:
    return "SELECT "


def second_source_distance_one() -> str:
    return second_source_distance_zero()  # Should be dropped.


def test_first_source_zero_second_source_zero():
    "SELECT " + first_source()


def test_first_source_zero_second_source_one():
    second_source_distance_zero() + first_source()


def test_first_source_one_second_source_zero():
    "SELECT " + first_source_distance_zero()


def test_first_source_one_second_source_one():
    second_source_distance_zero() + first_source_distance_zero()


def test_first_source_two_second_source_two():
    second_source_distance_one() + first_source_distance_one()  # Should be dropped.


def sink_distance_zero(x):
    "SELECT " + x


def sink_distance_one(x):
    sink_distance_zero(x)  # Should be dropped.


def test_sink_zero_first_source_zero():
    sink_distance_zero(first_source())


def test_sink_one_first_source_zero():
    sink_distance_one(first_source())  # Should be dropped.
