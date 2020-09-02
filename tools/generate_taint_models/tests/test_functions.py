# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe


def testA() -> None:
    pass


def testB(x) -> None:
    pass


def testC(x: int) -> None:
    pass


def testD(x: int, *args: int) -> None:
    pass


def testE(x: int, **kwargs: str) -> None:
    pass


class TestClass:
    def __init__(self):
        ...

    def methodA(self, x: int):
        ...

    def methodB(self, *args: str):
        ...


class TestChildClassA(TestClass):
    pass


class TestGrandChildClassA(TestChildClassA):
    def __init__(self, x: int):
        ...


class TestChildClassB(TestClass):
    def __init__(self, x: int):
        ...


all_functions = [
    testA,
    testB,
    testC,
    testD,
    testE,
    TestClass.methodA,
    TestClass.methodB,
]
