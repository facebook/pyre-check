# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


class Test1_C:
    attribute = ...

    def __init__(self):
        self.instance = ...


class Test1_C1(Test1_C):
    attribute = ...

    def __init__(self):
        self.instance = ...


class Test1_C2(Test1_C1):
    attribute = ...

    def __init__(self):
        self.instance = ...


class Test1_D:
    attribute = ...

    def __init__(self):
        self.instance = ...


class Test2_C:
    def foo(self, attribute):
        ...


class Test2_C1(Test2_C):
    def foo(self, attribute):
        ...


class Test2_C2(Test2_C1):
    def foo(self, attribute):
        ...


class Test2_D:
    def foo(self, attribute):
        ...


class UnrelatedClass:
    attribute = ...

    def __init__(self):
        self.instance = ...

    def foo(self, x):
        ...


def test1_alarm1(c: Test1_C1):
    __test_sink(c.attribute)


def test1_alarm2(c: Test1_C1):
    __test_sink(c.instance)


def test1_alarm3(c: Test1_C2):
    __test_sink(c.attribute)


def test1_alarm4(c: Test1_C2):
    __test_sink(c.instance)


def test1_alarm5(c: Test1_C):
    __test_sink(c.attribute)


def test1_alarm6(c: Test1_C):
    __test_sink(c.instance)


def test1_noalarm1(c: Test1_D):
    __test_sink(c.attribute)


def test1_noalarm2(c: Test1_D):
    __test_sink(c.instance)


def test2_alarm1(c: Test2_D):
    c.foo(__test_source())


def test2_noalarm1(c: Test2_C1):
    c.foo(__test_source())


def test2_noalarm2(c: Test2_C2):
    c.foo(__test_source())


def test2_noalarm3(c: Test2_C):
    c.foo(__test_source())


def misc_noalarm1(c: UnrelatedClass):
    __test_sink(c.attribute)


def misc_noalarm2(c: UnrelatedClass):
    __test_sink(c.instance)


def misc_noalarm3(c: UnrelatedClass):
    c.foo(__test_source())
