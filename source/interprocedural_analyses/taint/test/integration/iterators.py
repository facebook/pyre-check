# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Iterator


def test_next_iter():
    elements = [_test_source()]
    _test_sink(next(iter(elements)))  # This is an issue.

    elements = [0, _test_source(), 2]
    i = iter(elements)
    _test_sink(next(i))  # This is an issue (false positive).
    _test_sink(next(i))  # This is an issue.
    _test_sink(next(i))  # This is an issue (false positive).

    elements = [{"bad": _test_source(), "good": "safe"}]
    e = next(iter(elements))
    _test_sink(e["bad"])  # This is an issue.
    _test_sink(e["good"])  # This is NOT an issue.

    d = {"a": _test_source()}
    _test_sink(next(iter(d)))  # This is NOT an issue.

    d = {_test_source(): 0}
    _test_sink(next(iter(d)))  # This is an issue.

    e = next(iter([]), _test_source())
    _test_sink(e)  # This is an issue.

    e = next(iter([]), {"bad": _test_source(), "good": "safe"})
    _test_sink(e)  # This is an issue.
    _test_sink(e["bad"])  # This is an issue.
    _test_sink(e["good"])  # This is NOT an issue.


class CustomIter:
    def __iter__(self) -> Iterator[str]:
        return iter([_test_source()])


def test_custom_iter():
    # TODO(T137627339): False negative with custom `__iter__`
    _test_sink(next(iter(CustomIter())))


class CustomGetItem:
    def __getitem__(self, i: int) -> str:
        return _test_source()

    def __len__(self) -> int:
        return 10


def test_custom_getitem():
    # TODO(T137627339): False negative with custom `__getitem__`
    _test_sink(next(iter(CustomGetItem())))
