# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Dict, Optional


def indexes_are_strings() -> None:
    d = {}
    d[1] = _test_source()
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
    o.attribute = _test_source()
    _test_sink(o.__dict__["attribute"])
