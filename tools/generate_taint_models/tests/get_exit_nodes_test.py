# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_exit_nodes import ExitNodeGenerator


class GetExitNodesTest(unittest.TestCase):
    def test_compute_models(self):
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

        all_views = [
            testA,
            testB,
            testC,
            testD,
            testE,
            TestClass.methodA,
            TestClass.methodB,
        ]
        qualifier = f"{__name__}.GetExitNodesTest.test_compute_models"
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            list(ExitNodeGenerator([]).compute_models(all_views)),
            [
                f"def {qualifier}.TestClass.methodA(self, x) -> {sink}: ...",
                f"def {qualifier}.TestClass.methodB(self, *args) -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs) -> {sink}: ...",
            ],
        )
        self.assertEqual(
            list(
                ExitNodeGenerator([f"{qualifier}.TestClass.methodA"]).compute_models(
                    all_views
                )
            ),
            [
                f"def {qualifier}.TestClass.methodB(self, *args) -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs) -> {sink}: ...",
            ],
        )
