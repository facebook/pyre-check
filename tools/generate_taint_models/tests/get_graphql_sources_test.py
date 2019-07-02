# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_graphql_sources import GraphQLSourceGenerator


class GetGraphQLSourcesTest(unittest.TestCase):
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
        qualifier = f"{__name__}.GetGraphQLSourcesTest.test_compute_models"
        source = "TaintSource[UserControlled]"
        self.assertEqual(
            list(GraphQLSourceGenerator().compute_models(all_views)),
            [
                f"def {qualifier}.TestClass.methodA(self, x): ...",
                f"def {qualifier}.TestClass.methodB(self, *args: {source}): ...",
                f"def {qualifier}.testA(): ...",
                f"def {qualifier}.testB(x): ...",
                f"def {qualifier}.testC(x): ...",
                f"def {qualifier}.testD(x, *args: {source}): ...",
                f"def {qualifier}.testE(x, **kwargs: {source}): ...",
            ],
        )
