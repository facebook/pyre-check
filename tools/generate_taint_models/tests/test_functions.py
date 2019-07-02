# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def testA():
    pass


def testB(x):
    pass


def testC(x: int):
    pass


def testD(x: int, *args: int):
    pass


def testE(x: int, **kwargs: str):
    pass


class TestClass:
    def methodA(self, x: int):
        ...

    def methodB(self, *args: str):
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
