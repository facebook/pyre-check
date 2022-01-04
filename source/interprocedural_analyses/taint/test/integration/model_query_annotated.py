# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from builtins import _test_sink
from enum import Enum
from typing import Annotated, Optional


class Color(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3


class Test1_C:
    a: Annotated[Optional[float], Color.RED] = None
    b: Annotated[Optional[float], Color.BLUE] = None
    x: Annotated[Optional[float], "foo", Color.RED] = None
    y: Annotated[Optional[float], Color.BLUE, Color.RED] = None


def test1_alarm1(c: Test1_C) -> None:
    c.a = 1.01
    c.b = 1.01
    _test_sink(c.a)


def test1_alarm2(c: Test1_C) -> None:
    c.a = 1.01
    c.b = 1.01
    _test_sink(c.b)


def test1_alarm3(c: Test1_C) -> None:
    c.x = 1.01
    _test_sink(c.x)


def test1_alarm4(c: Test1_C) -> None:
    c.y = 1.01
    _test_sink(c.y)


class Test2_C:
    a: Annotated[Optional[float], Color.RED] = None
    b: Annotated[Optional[float], Color.BLUE] = None
    x: Annotated[Optional[float], Color.RED, "foo"] = None


def test2_alarm1(c: Test2_C) -> None:
    c.a = 1.01
    _test_sink(c.a)


def test2_alarm2(c: Test2_C) -> None:
    c.x = 1.01
    _test_sink(c.x)


def test2_noalarm1(c: Test2_C) -> None:
    c.b = 1.01
    _test_sink(c.b)


def test3_noalarm1(a: str, b: int) -> None:
    _test_sink(a)


def test3_noalarm2(a: str, b: int) -> None:
    _test_sink(b)


def test3_alarm1(
    a: Annotated[str, Color.RED], b: str, c: Annotated[str, Color.BLUE, "foo"]
) -> None:
    _test_sink(a)


def test3_alarm2(
    a: Annotated[str, Color.RED], b: str, c: Annotated[str, Color.BLUE, "foo"]
) -> None:
    _test_sink(c)


def test3_noalarm3(
    a: Annotated[str, Color.RED], b: str, c: Annotated[str, Color.BLUE, "foo"]
) -> None:
    _test_sink(b)
