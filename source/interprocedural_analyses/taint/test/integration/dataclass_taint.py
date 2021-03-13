# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source
from dataclasses import dataclass
from typing import final


@dataclass
class DataClass:
    bad: int
    benign: str


def bad_is_tainted():
    context = DataClass(bad=__test_source(), benign=1)
    __test_sink(context)
    return context


def benign_is_untainted():
    context = DataClass(bad=__test_source(), benign=1)
    __test_sink(context.benign)
    return context


@dataclass
class DataClassWIthInit:
    bad: int

    def __init__(self, bad: int) -> None:
        self.bad = bad
        __test_sink(bad)


def issue_in_dataclass_constructor() -> None:
    DataClassWIthInit(bad=__test_source())


@dataclass
# pyre-ignore[13]: Uninitialized attributes.
class WeirdDataClass:
    # Declare the attributes because Pysa complains when specifying an
    # attribute as a sink.
    bad: int
    bad_sink: int

    def __init__(self, bad: int, another: int) -> None:
        object.__setattr__(self, "bad", bad)
        object.__setattr__(self, "bad_sink", another)


def test_weird_dataclass_taint() -> WeirdDataClass:
    return WeirdDataClass(bad=1, another=2)


@final
@dataclass(frozen=True)
class DataClassWithSource:
    tainted: int
    not_tainted: str


def test_dataclass_with_source(context: DataClassWithSource) -> None:
    __test_sink(context.tainted)
    __test_sink(context.not_tainted)


@final
@dataclass(frozen=True)
class DataClassWithOtherSource:
    tainted: int
    not_tainted: str


def test_dataclass_with_other_source(context: DataClassWithOtherSource) -> None:
    __test_sink(context.tainted)
    __test_sink(context.not_tainted)
