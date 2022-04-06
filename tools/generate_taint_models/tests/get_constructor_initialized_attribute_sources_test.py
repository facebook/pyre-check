# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock

from ..get_constructor_initialized_attribute_sources import (
    ConstructorInitializedAttributeSourceGenerator,
)
from .test_functions import __name__ as qualifier, TestChildClassB, TestGrandChildClassA


class ConstructorInitializedAttributeSourceGeneratorTest(unittest.TestCase):
    def test_compute_models(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [
                {
                    "response": {
                        "attributes": [
                            {
                                "name": "__init__",
                                "annotation": f"BoundMethod[typing.Callable("
                                f"{qualifier}.TestGrandChildClassA.__init__)"
                                f"[[Named(self, {qualifier}.TestGrandChildClassA)"
                                f", Named(x, int)], typing.Any]"
                                f", {qualifier}.TestGrandChildClassA]",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "name": "x",
                                "annotation": "int",
                                "kind": "regular",
                                "final": False,
                            },
                        ]
                    }
                },
                {
                    "response": {
                        "attributes": [
                            {
                                "name": "__init__",
                                "annotation": f"BoundMethod[typing.Callable("
                                f"{qualifier}.TestChildClassB.__init__)"
                                f"[[Named(self, {qualifier}.TestChildClassB)"
                                f", Named(x, int)], typing.Any]"
                                f", {qualifier}.TestChildClassB]",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "name": "x",
                                "annotation": "int",
                                "kind": "regular",
                                "final": False,
                            },
                        ]
                    }
                },
            ]
        }

        self.assertEqual(
            set(
                map(
                    str,
                    ConstructorInitializedAttributeSourceGenerator(
                        classes_to_taint=[f"{qualifier}.TestClass"],
                        pyre_connection=pyre_connection,
                        taint_annotation="Taint",
                    ).compute_models(
                        [
                            TestGrandChildClassA.__init__,
                            TestChildClassB.__init__,
                        ]
                    ),
                )
            ),
            {
                f"{qualifier}.TestGrandChildClassA.x: Taint = ...",
                f"{qualifier}.TestChildClassB.x: Taint = ...",
            },
        )

    def test_gather_functions_to_model(self) -> None:
        self.assertEqual(
            set(
                ConstructorInitializedAttributeSourceGenerator(
                    classes_to_taint=[f"{qualifier}.TestClass"],
                    pyre_connection=MagicMock(),
                    taint_annotation="Taint",
                ).gather_functions_to_model()
            ),
            {TestGrandChildClassA.__init__, TestChildClassB.__init__},
        )

    def test_filter(self) -> None:
        self.assertEqual(
            set(
                ConstructorInitializedAttributeSourceGenerator(
                    classes_to_taint=[f"{qualifier}.TestClass"],
                    pyre_connection=MagicMock(),
                    filter_classes_by=(
                        lambda module: not module.__name__ == "TestChildClassB"
                    ),
                    taint_annotation="Taint",
                ).gather_functions_to_model()
            ),
            {TestGrandChildClassA.__init__},
        )
