# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from builtins import _test_source
from typing import Optional, Union


class Test1_C1:
    pass


class Test1_C2(Test1_C1):
    pass


class Test1_C3(Test1_C1):
    pass


class Test1_C4(Test1_C2):
    pass


class Test1_C5:
    pass


def test1_alarm1() -> Test1_C1:
    return _test_source()


def test1_alarm2() -> Test1_C2:
    return _test_source()


def test1_alarm3() -> Test1_C3:
    return _test_source()


def test1_alarm4() -> Test1_C4:
    return _test_source()


def test1_noalarm1() -> Test1_C5:
    return _test_source()


class Test2_C1:
    pass


class Test2_C2(Test2_C1):
    pass


class Test2_C3(Test2_C2):
    pass


class Test2_C4(Test2_C1):
    pass


class Test2:
    def test2_noalarm1(self) -> Test2_C1:
        return _test_source()

    def test2_noalarm2(self) -> Test2_C3:
        return _test_source()

    def test2_alarm1(self) -> Test2_C2:
        return _test_source()

    def test2_alarm2(self) -> Test2_C4:
        return _test_source()


class Test3_C1:
    pass


class Test3_C2(Test3_C1):
    pass


class Test3_C3(Test3_C1):
    pass


class Test3_C4(Test3_C2):
    pass


class Test3_C5:
    pass


def test3_alarm1() -> Optional[Test3_C1]:
    return _test_source()


def test3_alarm2() -> Optional[Test3_C2]:
    return _test_source()


def test3_alarm3() -> Optional[Test3_C3]:
    return _test_source()


def test3_alarm4() -> Optional[Test3_C4]:
    return _test_source()


def test3_noalarm1() -> Union[Test3_C5, Test3_C1]:
    return _test_source()
