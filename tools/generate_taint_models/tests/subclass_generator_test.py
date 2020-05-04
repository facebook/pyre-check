# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest
from unittest.mock import MagicMock, patch

from ....api import query
from .. import subclass_generator


class SubclassGeneratorTest(unittest.TestCase):
    @patch.object(query, "get_class_hierarchy")
    def test_get_all_subclasses_from_pyre(
        self, get_class_hierarchy_mock: MagicMock
    ) -> None:
        class_hierarchy = query.ClassHierarchy(
            {
                "WantedChild1": ["WantedParent"],
                "WantedChild2": ["WantedParent"],
                "UnwantedChild1": ["UnwantedParent"],
                "UnwantedParent": ["object"],
                "WantedParent": ["object"],
                "object": [],
            }
        )
        get_class_hierarchy_mock.return_value = class_hierarchy

        self.assertEqual(
            # pyre-ignore[6]
            subclass_generator.get_all_subclasses_from_pyre(["WantedParent"], None),
            {"WantedParent": ["WantedChild1", "WantedChild2"]},
        )

    @patch.object(query, "defines")
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
