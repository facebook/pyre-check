# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source
from typing import Annotated, Dict, List


def test1_f1(a, b, c):
    pass


def test1_alarm1():
    x: str = _test_source()
    test1_f1(x, "b", 0)


def test1_alarm2():
    x: Annotated[str, "foo"] = _test_source()
    test1_f1("a", x, 0)


def test1_alarm3():
    x: int = _test_source()
    test1_f1("a", "b", x)


def test1_noalarm1():
    test1_f1("a", "b", 0)


class Test2_C:
    def f1(self, a, b, c):
        pass

    def f2(self, a, b, c):
        pass


class Test2_T:
    pass


def test2_alarm1(c: Test2_C):
    x: str = _test_source()
    c.f1(x, "b", 0)


def test2_alarm2(c: Test2_C):
    x: Dict[str, int] = _test_source()
    c.f1("a", x, 0)


def test2_alarm3(c: Test2_C):
    x: Test2_T = _test_source()
    c.f1("a", "b", x)


def test2_alarm4(c: Test2_C):
    x: int = _test_source()
    c.f2(x, "b", 0)


def test2_alarm5(c: Test2_C):
    x: List[List[Test2_T]] = _test_source()
    c.f2("a", x, 0)


def test2_alarm6(c: Test2_C):
    x: float = _test_source()
    c.f2("a", "b", x)


def test2_noalarm1(c: Test2_C):
    c.f1([], {}, 0.0)


def test2_noalarm2(c: Test2_C):
    c.f2("a", 0, Test2_T())
