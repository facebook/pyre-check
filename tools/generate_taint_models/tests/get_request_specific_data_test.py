# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_request_specific_data import RequestSpecificDataGenerator
from .test_functions import __name__ as qualifier, all_functions


class GetRequestSpecificDataTest(unittest.TestCase):
    def test_compute_models(self):
        source = "TaintSource[RequestSpecificData]"
        self.assertEqual(
            list(RequestSpecificDataGenerator().compute_models(all_functions)),
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
