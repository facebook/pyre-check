# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_exit_nodes import ExitNodeGenerator
from ..model_generator import Configuration
from .test_functions import __name__ as qualifier, all_functions


class GetExitNodesTest(unittest.TestCase):
    def test_compute_models(self):
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            list(ExitNodeGenerator().compute_models(all_functions)),
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
        Configuration.whitelisted_views = [f"{qualifier}.TestClass.methodA"]
        self.assertEqual(
            list(ExitNodeGenerator().compute_models(all_functions)),
            [
                f"def {qualifier}.TestClass.methodB(self, *args) -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs) -> {sink}: ...",
            ],
        )
