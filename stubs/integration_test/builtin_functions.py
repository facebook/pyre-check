# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from integration_test.taint import source, sink


def test_enumerate():
    elements = ["a", source(), "b"]
    for index, value in enumerate(elements):
        sink(index)  # This is NOT an issue.
        sink(value)  # This is an issue.

    elements = [{}, {}, {"a": source()}]
    for index, value in enumerate(elements):
        sink(index)  # This is NOT an issue.
        sink(value)  # This is an issue.
        sink(value["a"])  # This is an issue.
        sink(value["b"])  # This is NOT an issue.


def test_sorted(i: int):
    elements = ["a", source(), "b"]
    elements = sorted(elements)
    sink(elements[0])

    elements = [(0, "a"), (0, source()), (0, "b")]
    elements = sorted(elements)
    sink(elements[0][0])  # This is NOT an issue.
    sink(elements[0][1])  # This is an issue.
    sink(elements[i][1])  # This is an issue.

    d = {(0, 0): "a", (0, source()): "b"}
    elements = sorted(d)
    sink(elements[i][1])  # This is an issue.
    sink(elements[i][0])  # This is NOT an issue.


def test_reversed(i: int):
    elements = ["a", "b", source()]
    elements = reversed(elements)
    sink(elements[0])

    elements = [(0, "a"), (0, source())]
    elements = reversed(elements)
    sink(elements[0][0])  # This is NOT an issue.
    sink(elements[1][0])  # This is NOT an issue.
    sink(elements[i][0])  # This is NOT an issue.

    sink(elements[0][1])  # This is an issue.
    sink(elements[1][1])  # This is an issue (false positive).
    sink(elements[i][1])  # This is an issue.

    d = {(0, 0): "a", (0, source()): "b"}
    elements = reversed(d)
    sink(elements[i][1])  # This is an issue.
    sink(elements[i][0])  # This is NOT an issue.


def test_map_lambda(i: int):
    elements = list(map(lambda x: x, [source()]))
    sink(elements[0])  # This is an issue.
    sink(elements[i])  # This is an issue.

    elements = list(map(lambda x: x, [0, source(), 0]))
    sink(elements[i])  # This is an issue.
    sink(elements[1])  # This is an issue.
    sink(elements[0])  # This is an issue (false positive).

    elements = list(map(lambda x: {"a": x, "b": "safe"}, [source()]))
    sink(elements[i])  # This is an issue.
    sink(elements[i]["a"])  # This is an issue.
    sink(elements[i]["b"])  # This is an issue (false positive).

    elements = list(map(lambda x: x["a"], [{"a": source(), "b": "safe"}]))
    sink(elements[i])  # This is an issue.

    elements = list(map(lambda x: x["b"], [{"a": source(), "b": "safe"}]))
    sink(elements[i])  # This is an issue (false positive).

    elements = list(map(lambda x: source(), ["safe"]))
    sink(elements[i])  # This is an issue.

    # TODO(T137447924): This should be an issue, but it is NOT (false negative).
    elements = list(map(lambda x: sink(x), [source()]))


def test_filter_lambda(i: int):
    elements = list(filter(lambda x: x != 0, [source()]))
    sink(elements[0])  # This is an issue.
    sink(elements[i])  # This is an issue.

    elements = list(filter(lambda x: x != 0, [0, source(), 1]))
    sink(elements[i])  # This is an issue.
    sink(elements[0])  # This is an issue.
    sink(elements[1])  # This is an issue (false positive).

    elements = list(filter(lambda x: x["a"], [{"a": source(), "b": "safe"}]))
    sink(elements[i])  # This is an issue.
    sink(elements[i]["a"])  # This is an issue.
    sink(elements[i]["b"])  # This is NOT an issue.

    # TODO(T137447924): This should be an issue, but it is NOT (false negative).
    elements = list(filter(lambda x: sink(x), [source()]))

    elements = list(filter(lambda x: x, {source(): 0, "b": 1}))
    sink(elements[i])  # This is an issue.

    elements = list(filter(lambda x: x, {(0, source()): 0, "b": 1}))
    sink(elements[i])  # This is an issue.
    sink(elements[i][0])  # This is NOT an issue.
    sink(elements[i][1])  # This is an issue.


def test_next_iter():
    elements = [source()]
    sink(next(iter(elements)))  # This is an issue.

    elements = [0, source(), 2]
    i = iter(elements)
    sink(next(i))  # This should not be an issue, but it is (false positive).
    sink(next(i))  # This is an issue.
    sink(next(i))  # This should not be an issue, but it is (false positive).

    elements = [{"bad": source(), "good": "safe"}]
    element = next(iter(elements))
    sink(element["bad"])  # This is an issue.
    sink(element["good"])  # This is NOT an issue.

    d = {"a": source()}
    sink(next(iter(d)))  # This is NOT an issue.

    d = {source(): 0}
    sink(next(iter(d)))  # This is an issue.

    element = next(iter([]), source())
    sink(element)  # This is an issue.

    element = next(iter([]), {"bad": source(), "good": "safe"})
    sink(element)  # This is an issue.
    sink(element["bad"])  # This is an issue.
    sink(element["good"])  # This is NOT an issue.
