# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink
from typing import Annotated, List


class C:
    ...


def test1_alarm1(a: int, b: str, c: C, d):
    _test_sink(a)


def test1_alarm2(a: int, b: str, c: C, d):
    _test_sink(b)


def test1_alarm3(a: int, b: str, c: C, d):
    _test_sink(c)


def test1_alarm4(a: int, b: str, c: C, d):
    _test_sink(d)


def test2_noalarm1(foo_1, foo_2):
    _test_sink(foo_1)


def test2_noalarm2(foo_1, foo_2):
    _test_sink(foo_2)


def test3_noalarm1(a: int, b: str, c: C, d):
    _test_sink(a)


def test3_noalarm2(a: int, b: str, c: C, d):
    _test_sink(b)


def test3_alarm1(a: int, b: str, c: C, d):
    _test_sink(c)


def test3_alarm2(a: int, b: str, c: C, d):
    _test_sink(d)


def test4_alarm1(a: List[str], b: List[int], c: C, d):
    _test_sink(a)


def test4_noalarm1(a: List[str], b: List[int], c: C, d):
    _test_sink(b)


def test4_alarm2(a: int, b: str, c: C, d):
    _test_sink(c)


def test4_noalarm2(a: int, b: str, c: C, d):
    _test_sink(d)


class Test5:
    def test5_alarm1(self, x: List[str]):
        pass

    def test5_alarm2(self, x: List[int]):
        pass

    def test5_alarm3(self, x: C):
        pass

    def test5_alarm4(self, x: Annotated[str, "test"]):
        pass

    def test5_noalarm1(self, x: int):
        pass


def test6_alarm1(a, b, c, d):
    _test_sink(a)


def test6_noalarm1(a, b, c, d):
    _test_sink(b)


def test6_alarm2(a, b, c, d):
    _test_sink(c)


def test6_noalarm2(a, b, c, d):
    _test_sink(d)


class Test7:
    def test7_alarm1(self, x):
        pass

    def test7_noalarm1(self):
        pass


def test8_alarm1(a, b, c, d):
    _test_sink(a)


def test8_alarm2(a, b, c, d):
    _test_sink(b)


def test8_alarm3(a, b, c, d):
    _test_sink(c)


def test8_alarm4(a, b, c, d):
    _test_sink(d)


def test9_f(a, b):
    pass


def test10_f(a: Annotated[int, "foo"], b: str, c_foo, d: List[str]):
    pass
