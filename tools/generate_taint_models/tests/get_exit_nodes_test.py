# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_exit_nodes import ExitNodeGenerator


class GetExitNodesTest(unittest.TestCase):
    def test_compute_models(self):
        def visit_all_views(callback: Callable[..., None]):
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

            callback(testA)
            callback(testB)
            callback(testC)
            callback(testD)
            callback(testE)
            callback(TestClass.methodA)
            callback(TestClass.methodB)

        qualifier = f"{__name__}.GetExitNodesTest.test_compute_models.visit_all_views"
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            list(ExitNodeGenerator([]).compute_models(visit_all_views)),
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
                    visit_all_views
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
