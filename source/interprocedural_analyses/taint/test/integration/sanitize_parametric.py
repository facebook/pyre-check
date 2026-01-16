# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import random


def a_source_x(): ...


def a_source_y(): ...


def a_sink_x(x): ...


def a_sink_y(x): ...


def sanitize_a_source_tito(x):
    return x


def sanitize_a_sink_tito(x):
    return x


def partial_issue_sources():
    if random.random() > 0.5:
        x = a_source_x()
        return sanitize_a_sink_tito(x)
    else:
        return a_source_y()


def partial_issue_sinks(x):
    if random.random() > 0.5:
        a_sink_x(x)
    else:
        y = sanitize_a_source_tito(x)
        a_sink_y(y)


def partial_issue_sanitize():
    # Sources: {NotSink[TestA]@TestA[X], TestA[Y]}
    # Sinks: {TestA[X], NotSource[testA]@TestA[Y]}
    x = partial_issue_sources()
    partial_issue_sinks(x)
