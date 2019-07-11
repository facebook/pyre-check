# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_REST_api_sources import RESTApiSourceGenerator
from ..model_generator import Configuration
from .test_functions import __name__ as qualifier, all_functions


class GetRESTApiSourcesTest(unittest.TestCase):
    def test_compute_models(self):
        source = "TaintSource[UserControlled]"
        self.assertEqual(
            list(RESTApiSourceGenerator().compute_models(all_functions)),
            [
                f"def {qualifier}.TestClass.methodA(self: {source}, x: {source}): ...",
                f"def {qualifier}.TestClass.methodB(self: {source}, *args: {source})"
                ": ...",
                f"def {qualifier}.testA(): ...",
                f"def {qualifier}.testB(x: {source}): ...",
                f"def {qualifier}.testC(x: {source}): ...",
                f"def {qualifier}.testD(x: {source}, *args: {source}): ...",
                f"def {qualifier}.testE(x: {source}, **kwargs: {source}): ...",
            ],
        )
        Configuration.whitelisted_classes = ["int"]
        Configuration.whitelisted_views = [f"{qualifier}.testA"]
        self.assertEqual(
            list(RESTApiSourceGenerator().compute_models(all_functions)),
            [
                f"def {qualifier}.TestClass.methodA(self: {source}, x): ...",
                f"def {qualifier}.TestClass.methodB(self: {source}, *args: {source})"
                ": ...",
                f"def {qualifier}.testB(x: {source}): ...",
                f"def {qualifier}.testC(x): ...",
                f"def {qualifier}.testD(x, *args): ...",
                f"def {qualifier}.testE(x, **kwargs: {source}): ...",
            ],
        )
