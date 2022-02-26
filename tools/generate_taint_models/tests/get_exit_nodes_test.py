# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock

from ..get_exit_nodes import ExitNodeGenerator
from .test_functions import __name__ as qualifier, all_functions


class GetExitNodesTest(unittest.TestCase):
    def test_compute_models(self) -> None:
        self.maxDiff = None
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            [
                *map(
                    str,
                    ExitNodeGenerator(django_urls=MagicMock()).compute_models(
                        all_functions
                    ),
                )
            ],
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
            [
                *map(
                    str,
                    ExitNodeGenerator(
                        django_urls=MagicMock(),
                        whitelisted_views=[f"{qualifier}.TestClass.methodA"],
                    ).compute_models(all_functions),
                )
            ],
            [
                f"def {qualifier}.TestClass.methodB(self, *args) -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs) -> {sink}: ...",
            ],
        )
