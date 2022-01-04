# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest
from unittest.mock import MagicMock, patch

from ....api import query
from .. import subclass_generator


class SubclassGeneratorTest(unittest.TestCase):
    # pyre-fixme[56]: Argument `tools.pyre.api.query` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(query, "get_cached_class_hierarchy")
    def test_get_all_subclasses_from_pyre(
        self, get_cached_class_hierarchy_mock: MagicMock
    ) -> None:
        class_hierarchy = query.ClassHierarchy(
            {
                "GrandChild": ["WantedChild1"],
                "WantedChild1": ["WantedParent"],
                "WantedChild2": ["WantedParent"],
                "UnwantedChild1": ["UnwantedParent"],
                "UnwantedParent": ["object"],
                "WantedParent": ["object"],
                "object": [],
            }
        )
        get_cached_class_hierarchy_mock.return_value = class_hierarchy

        # Ensure we don't find 'GrandChild' when 'transitive' is defaulted to False
        self.assertEqual(
            # pyre-ignore[6]
            subclass_generator.get_all_subclasses_from_pyre(["WantedParent"], None),
            {"WantedParent": ["WantedChild1", "WantedChild2"]},
        )

        # Ensure we do find 'GrandChild' when 'transitive' is True
        self.assertEqual(
            subclass_generator.get_all_subclasses_from_pyre(
                ["WantedParent"],
                # pyre-ignore[6]
                None,
                True,
            ),
            {"WantedParent": ["GrandChild", "WantedChild1", "WantedChild2"]},
        )

    @patch.object(query, "defines")
    # pyre-fixme[56]: Argument
    #  `tools.pyre.tools.generate_taint_models.subclass_generator` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(subclass_generator, "get_all_subclasses_from_pyre")
    def test_get_all_subclass_defines_from_pyre(
        self, get_all_subclasses_from_pyre_mock: MagicMock, defines_mock: MagicMock
    ) -> None:
        subclasses_dict = {"WantedParent": ["WantedChild1", "WantedChild2"]}
        get_all_subclasses_from_pyre_mock.return_value = subclasses_dict

        subclass_defines = [
            query.Define(name="WantedChild1", parameters=[], return_annotation="None"),
            query.Define(name="WantedChild2", parameters=[], return_annotation="None"),
        ]

        defines_mock.return_value = subclass_defines

        self.assertEqual(
            subclass_generator.get_all_subclass_defines_from_pyre(
                ["WantedParent"],
                # pyre-ignore[6]
                None,
            ),
            {"WantedParent": subclass_defines},
        )
