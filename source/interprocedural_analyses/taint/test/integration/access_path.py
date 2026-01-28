# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Dict, Optional

from pysa import _test_sink, _test_source


def indexes_are_strings() -> None:
    d = {}
    d[1] = _test_source()
    # pyrefly: ignore[bad-index]
    _test_sink(d["1"])  # False positive.


class SpecialDict:
    def __init__(self) -> None:
        self.attribute: Optional[str] = None
        self.dict: Dict[str, str] = {}

    def __getitem__(self, key: str) -> str:
        return self.dict.get(key, "")


def indexes_and_attributes():
    o = SpecialDict()
    o.attribute = _test_source()
    # False positive, attributes and indexes are mixed.
    _test_sink(o["attribute"])


def indexes_are_attributes_for___dict__():
    o = object()
    # pyrefly: ignore[missing-attribute]
    o.attribute = _test_source()
    _test_sink(o.__dict__["attribute"])


def positional_and_variadic1(x, /, y, *z):
    _test_sink(z)


def positional_and_variadic2(x, /, y, *z):
    _test_sink(y)


def issue_positional_and_variadic():
    positional_and_variadic1(1, 2, _test_source())
    positional_and_variadic2(1, 2, _test_source())
    positional_and_variadic2(1, _test_source())
    positional_and_variadic2(1, y=_test_source())


def sink_on_keyword_only(x, *, y):
    pass


# __x marked as a sink in .pysa
def sink_on_positional_only(__x, /, y):
    pass


# y marked as a sink in .pysa
def sink_on_non_positional_only(__x, /, y):
    pass
