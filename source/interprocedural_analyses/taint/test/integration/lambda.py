# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source


def test_map_lambda(i: int):
    elements = list(map(lambda x: x, [_test_source()]))
    _test_sink(elements[0])  # This is an issue.
    _test_sink(elements[i])  # This is an issue.

    elements = list(map(lambda x: x, [0, _test_source(), 0]))
    _test_sink(elements[i])  # This is an issue.
    _test_sink(elements[1])  # This is an issue.
    _test_sink(elements[0])  # This is an issue (false positive).

    elements = list(map(lambda x: {"a": x, "b": "safe"}, [_test_source()]))
    _test_sink(elements[i])  # This is an issue.
    _test_sink(elements[i]["a"])  # This is an issue.
    _test_sink(elements[i]["b"])  # This is an issue (false positive).

    elements = list(map(lambda x: x["a"], [{"a": _test_source(), "b": "safe"}]))
    _test_sink(elements[i])  # This is an issue.

    elements = list(map(lambda x: x["b"], [{"a": _test_source(), "b": "safe"}]))
    _test_sink(elements[i])  # This is an issue (false positive).

    elements = list(map(lambda x: _test_source(), ["safe"]))
    _test_sink(elements[i])  # This is an issue.

    # TODO(T137447924): This should be an issue, but it is NOT (false negative).
    elements = list(map(lambda x: _test_sink(x), [_test_source()]))


def test_filter_lambda(i: int):
    elements = list(filter(lambda x: x != 0, [_test_source()]))
    _test_sink(elements[0])  # This is an issue.
    _test_sink(elements[i])  # This is an issue.

    elements = list(filter(lambda x: x != 0, [0, _test_source(), 1]))
    _test_sink(elements[i])  # This is an issue.
    _test_sink(elements[0])  # This is an issue.
    _test_sink(elements[1])  # This is an issue (false positive).

    elements = list(filter(lambda x: x["a"], [{"a": _test_source(), "b": "safe"}]))
    _test_sink(elements[i])  # This is an issue.
    _test_sink(elements[i]["a"])  # This is an issue.
    _test_sink(elements[i]["b"])  # This is NOT an issue.

    # TODO(T137447924): This should be an issue, but it is NOT (false negative).
    elements = list(filter(lambda x: _test_sink(x), [_test_source()]))
