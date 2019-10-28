# Copyright (c) 2016-present, Facebook, Inc.
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
