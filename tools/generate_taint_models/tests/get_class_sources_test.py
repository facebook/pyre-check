# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest

from ..get_class_sources import ClassSourceGenerator
from .test_functions import __name__ as qualifier, TestChildClassB, TestGrandChildClassA


class GetClassSourcesTest(unittest.TestCase):
    def test_gather_functions_to_model(self) -> None:
        self.assertEqual(
            set(
                ClassSourceGenerator(
                    classes_to_taint=[f"{qualifier}.TestClass"]
                ).gather_functions_to_model()
            ),
            {TestChildClassB.__init__, TestGrandChildClassA.__init__},
        )
