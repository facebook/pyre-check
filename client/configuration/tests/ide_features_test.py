# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
from typing import Optional

import testslide

from ..configuration import PartialConfiguration, merge_partial_configurations
from ..exceptions import InvalidConfiguration
from ..ide_features import IdeFeatures


class IdeFeaturesTest(testslide.TestCase):
    def test_create_from_string(self) -> None:
        def assert_ide_features_equal(input: object, expected: object) -> None:
            self.assertEqual(
                PartialConfiguration.from_string(json.dumps(input)).ide_features,
                expected,
            )

        def assert_ide_features_raises(input: object) -> None:
            with self.assertRaises(InvalidConfiguration):
                PartialConfiguration.from_string(json.dumps(input))

        assert_ide_features_equal({}, None)
        assert_ide_features_equal({"ide_features": {}}, IdeFeatures())
        assert_ide_features_equal(
            {"ide_features": {"hover_enabled": True}}, IdeFeatures(hover_enabled=True)
        )
        assert_ide_features_equal(
            {"ide_features": {"hover_enabled": False}}, IdeFeatures(hover_enabled=False)
        )
        assert_ide_features_raises({"ide_features": {"hover_enabled": 42}})
        assert_ide_features_equal(
            {"ide_features": {"go_to_definition_enabled": True}},
            IdeFeatures(go_to_definition_enabled=True),
        )
        assert_ide_features_equal(
            {"ide_features": {"go_to_definition_enabled": False}},
            IdeFeatures(go_to_definition_enabled=False),
        )
        assert_ide_features_raises({"ide_features": {"go_to_definition_enabled": 42}})

    def test_merge(self) -> None:
        def assert_merged(
            base_ide_features: Optional[IdeFeatures],
            override_ide_features: Optional[IdeFeatures],
            expected: Optional[IdeFeatures],
        ) -> None:
            self.assertEqual(
                merge_partial_configurations(
                    base=PartialConfiguration(ide_features=base_ide_features),
                    override=PartialConfiguration(ide_features=override_ide_features),
                ).ide_features,
                expected,
            )

        assert_merged(None, None, None)
        assert_merged(
            IdeFeatures(hover_enabled=True), None, IdeFeatures(hover_enabled=True)
        )
        assert_merged(
            None, IdeFeatures(hover_enabled=True), IdeFeatures(hover_enabled=True)
        )
        assert_merged(
            IdeFeatures(hover_enabled=False),
            IdeFeatures(hover_enabled=True),
            IdeFeatures(hover_enabled=True),
        )
        assert_merged(
            IdeFeatures(hover_enabled=True),
            IdeFeatures(hover_enabled=False),
            IdeFeatures(hover_enabled=False),
        )
        assert_merged(
            IdeFeatures(go_to_definition_enabled=True),
            None,
            IdeFeatures(go_to_definition_enabled=True),
        )
        assert_merged(
            None,
            IdeFeatures(go_to_definition_enabled=True),
            IdeFeatures(go_to_definition_enabled=True),
        )
        assert_merged(
            IdeFeatures(go_to_definition_enabled=False),
            IdeFeatures(go_to_definition_enabled=True),
            IdeFeatures(go_to_definition_enabled=True),
        )
        assert_merged(
            IdeFeatures(go_to_definition_enabled=True),
            IdeFeatures(go_to_definition_enabled=False),
            IdeFeatures(go_to_definition_enabled=False),
        )
