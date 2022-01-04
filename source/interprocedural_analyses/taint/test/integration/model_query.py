# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from functools import lru_cache


def foo(x):
    return 0


def barfoo(x):
    return 0


class C:
    def foo(self, x):
        return 0

    def positional_method(self, x):
        return 0


def two_parameters(x, y):
    return 0


def three_parameters(x, y, z):
    return 0


def positional_a(x):
    return 0


def positional_b(y):
    return 0


class Base:
    def foo(self, x):
        return 0


class NotBase:
    def foo(self, x):
        return 0


class Child(Base):
    def bar(self, y):
        return 0


class GrandChild(Child):
    def baz(self, z):
        return 0


@lru_cache(maxsize=1)
def positional_decorated(x, y) -> int:
    ...


class AttributeTestBase:
    ...


class AttributeTestClass1(AttributeTestBase):
    attribute: ...

    def __init__(self):
        self.instance = None


class AttributeTestClass2(AttributeTestBase):
    attribute: ...

    def __init__(self):
        self.instance = None


class AttributeTestClass3:
    attribute: ...

    def __init__(self):
        self.instance = None


class AttributeTestClass4:
    attribute: ...

    def __init__(self):
        self.instance = None


class AttributeTestClass5:
    foo_attribute: ...

    def __init__(self):
        self.foo_instance = None


class AttributeTestClass6:
    foo_attribute: ...

    def __init__(self):
        self.foo_instance = None


class AttributeTestClass7:
    nomatch_attribute1: ...

    def __init__(self):
        self.nomatch_instance1 = None


class AttributeTestClass8(AttributeTestClass7):
    nomatch_attribute2: ...

    def __init__(self):
        self.nomatch_instance2 = None


def alarm_1(x: AttributeTestClass1):
    # should trigger SourceA -> Test
    _test_sink(x.attribute)


def alarm_2(x: AttributeTestClass1):
    # should trigger SourceA -> Test
    _test_sink(x.instance)


def alarm_3(x: AttributeTestClass2):
    # should trigger SourceA -> test
    _test_sink(x.attribute)


def alarm_4(x: AttributeTestClass2):
    # should trigger SourceA -> test
    _test_sink(x.instance)


def alarm_5(x: AttributeTestClass3, source):
    # should trigger Test -> SinkA and Test -> SinkB
    x.attribute = source


def alarm_6(x: AttributeTestClass3):
    # should trigger Test -> SinkA and Test -> SinkC
    x.instance = _test_source()


def alarm_7(x: AttributeTestClass4):
    # should trigger SourceC -> Test and SourceD -> Test
    return x.attribute


def alarm_8(x: AttributeTestClass4):
    # should trigger SourceC -> Test and SourceE -> Test
    _test_sink(x.instance)


def alarm_9(x: AttributeTestClass5):
    # should trigger SourceB -> Test
    _test_sink(x.foo_attribute)


def alarm_10(x: AttributeTestClass5):
    # should trigger SourceB -> Test
    _test_sink(x.foo_instance)


def alarm_11(x: AttributeTestClass6):
    # should trigger SourceB -> Test
    _test_sink(x.foo_attribute)


def alarm_12(x: AttributeTestClass6):
    # should trigger SourceB -> Test
    _test_sink(x.foo_instance)


def no_alarm_1(x: AttributeTestClass7):
    _test_sink(x.nomatch_attribute1)
    _test_sink(x.nomatch_instance1)


def no_alarm_2(x: AttributeTestClass8):
    _test_sink(x.nomatch_instance2)
    _test_sink(x.nomatch_instance2)


def function_test1_alarm1():
    return 0


def function_test1_alarm2():
    return 0


def function_test1_noalarm1():
    return 0


def function_test1_noalarm2():
    return 0


class ClassTest1:
    def method_test1_alarm1():
        return 0

    def method_test1_noalarm1():
        return 0


class NoAlarmClass:
    ...


class ClassTest2_Alarm1:
    def method1():
        return 0

    def method2():
        return 0


class ClassTest2_NoAlarm1(NoAlarmClass):
    def method1():
        return 0

    def method2():
        return 0


class ClassTest3_Alarm1:
    def method1():
        return 0

    def method2():
        return 0
