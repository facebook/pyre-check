# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import overload, Type


class Base:
    q: str = "q"
    r: str = "r"

    def __init__(self, arg):
        self.r = arg

    def methodA(self, arg):
        pass

    def methodB(self):
        pass

    @classmethod
    def classMethod(cls, arg):
        pass


class A(Base):
    q: str = "q"

    def __init__(self, arg):
        super(Base, self).__init__(arg)

    def methodA(self, arg):
        _test_sink(arg)


class B(Base):
    r: str = "r"

    def __init__(self, arg):
        super(Base, self).__init__(arg)

    def methodB(self):
        return _test_source()

    @classmethod
    def classMethod(cls, arg):
        _test_sink(arg)


class C(B):
    q: str = "q"

    def __init__(self, arg):
        super(B, self).__init__(arg)

    def methodA(self, arg):
        pass

    @classmethod
    def classMethod(cls, arg):
        pass


class D(C):
    def __init__(self, arg):
        super(C, self).__init__(arg)

    def methodA(self, arg):
        _test_sink(arg)

    def methodB(self):
        return _test_source()


def testBase(o: Base, cls: Type[Base]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticBase(o: Base):
    x = o.methodB()
    Base.classMethod(x)


def testMakeBase():
    o = Base()
    x = o.methodB()


def testA(o: A, cls: Type[A]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticA(o: A):
    x = o.methodB()
    A.classMethod(x)


def testMakeA():
    o = A()
    x = o.methodB()


def testB(o: B, cls: Type[B]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticB(o: B):
    x = o.methodB()
    B.classMethod(x)


def testMakeB():
    o = B()
    x = o.methodB()


def testC(o: C, cls: Type[C]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticC(o: C):
    x = o.methodB()
    C.classMethod(x)


def testMakeC():
    o = C()
    x = o.methodB()


def testD(o: D, cls: Type[D]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticD(o: D):
    x = o.methodB()
    D.classMethod(x)


def testMakeD():
    o = D()
    x = o.methodB()


def constructorTest(cls: Type[D]) -> D:
    return cls(_test_source())


class OverloadedOverride(D):
    @overload
    def methodA(self, arg: int) -> int:
        ...

    @overload
    def methodA(self, arg: str) -> str:
        ...

    def methodA(self, arg):
        return arg


class SkippedOverrides:
    def method(self, arg):
        return arg


class ExtendsSkipped(SkippedOverrides):
    def method(self, arg):
        _test_sink(arg)
