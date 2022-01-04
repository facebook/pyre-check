# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def add_feature_to_first(first, second):
    pass


def adds_and_taints():
    x = _test_source()
    add_feature_to_first(x, 0)
    return x


def propagate_add_feature(parameter):
    return add_feature_to_first(parameter, 0)


def add_via_value_of(first, second):
    pass


def test_add_via_value_of_second():
    x = _test_source()
    add_via_value_of(x, "second")
    return x


def dict_test_add_via_value_of_second():
    x = _test_source()
    add_via_value_of(x["foo"], "second")
    return x


def test_add_feature_to_sink(parameter):
    add_feature_to_first(parameter, "")
    _test_sink(parameter)


def test_add_feature_in_comprehension():
    sources = [_test_source()]
    v = [s for s in sources if add_feature_to_first(s, 0)]
    _test_sink(v[0])


def test_add_feature_to_sink_in_comprehension(parameter):
    x = [s for s in [1, 2, 3] if add_feature_to_first(parameter, 0)]
    _test_sink(parameter)
