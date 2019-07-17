# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import unittest
from typing import Callable

from ..get_graphql_sources import GraphQLSourceGenerator
from .test_functions import __name__ as qualifier, all_functions


class GetGraphQLSourcesTest(unittest.TestCase):
    def test_compute_models(self):
        source = "TaintSource[UserControlled]"
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            list(GraphQLSourceGenerator().compute_models(all_functions)),
            [
                f"def {qualifier}.TestClass.methodA(self, x) -> {sink}: ...",
                f"def {qualifier}.TestClass.methodB(self, *args: {source}) -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args: {source}) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs: {source}) -> {sink}: ...",
            ],
        )
