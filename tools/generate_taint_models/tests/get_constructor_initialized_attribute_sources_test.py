# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..get_constructor_initialized_attribute_sources import (
    ConstructorInitializedAttributeSourceGenerator,
)
from .test_functions import TestClass, __name__ as qualifier  # noqa


class ConstructorInitializedAttributeSourceGeneratorTest(unittest.TestCase):
    def test_end_to_end(self) -> None:
        self.assertEqual(
            set(
                map(
                    str,
                    ConstructorInitializedAttributeSourceGenerator(
                        classes_to_taint=[f"{qualifier}.TestClass"],
                        taint_annotation="Taint",
                    ).generate_models(),
                )
            ),
            {
                f"{qualifier}.TestGrandChildClassA.x: Taint = ...",
                f"{qualifier}.TestChildClassB.x: Taint = ...",
            },
        )
