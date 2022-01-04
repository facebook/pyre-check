# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Check that flows that can never match any rule are properly discarded.


def a_source():
    ...


def a_sink(x):
    ...


def b_sink(x):
    ...


def sanitize_a_source_tito(x):
    return x


def sanitize_a_sink_tito(x):
    return x


def sanitize_b_sink_tito(x):
    return x


def test_source_a_sanitize_a_kept():
    return sanitize_a_sink_tito(a_source())


def test_source_a_sanitize_a_b_discarded():
    return sanitize_b_sink_tito(sanitize_a_sink_tito(a_source()))


def test_sink_a_sanitize_a_discarded(x):
    a_sink(sanitize_a_source_tito(x))
