# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Optional

import testslide

from ..exceptions import InvalidConfiguration
from ..ide_features import IdeFeatures


class IdeFeaturesTest(testslide.TestCase):
    def test_create_from_json(self) -> None:
        def assert_ide_features_equal(input: object, expected: object) -> None:
            self.assertEqual(
                IdeFeatures.create_from_json(input),
                expected,
            )

        def assert_ide_features_raises(input: object) -> None:
            with self.assertRaises(InvalidConfiguration):
                IdeFeatures.create_from_json(input)

        assert_ide_features_equal({}, IdeFeatures())
        assert_ide_features_equal(
            {"hover_enabled": True}, IdeFeatures(hover_enabled=True)
        )
        assert_ide_features_equal(
            {"hover_enabled": False}, IdeFeatures(hover_enabled=False)
        )
        assert_ide_features_raises({"hover_enabled": 42})

        assert_ide_features_equal(
            {"go_to_definition_enabled": True},
            IdeFeatures(go_to_definition_enabled=True),
        )
        assert_ide_features_equal(
            {"go_to_definition_enabled": False},
            IdeFeatures(go_to_definition_enabled=False),
        )
        assert_ide_features_raises({"go_to_definition_enabled": 42})

        assert_ide_features_equal(
            {"find_all_references_enabled": True},
            IdeFeatures(find_all_references_enabled=True),
        )
        assert_ide_features_equal(
            {"find_all_references_enabled": False},
            IdeFeatures(find_all_references_enabled=False),
        )
        assert_ide_features_raises({"find_all_references_enabled": 42})

        assert_ide_features_equal(
            {"expression_level_coverage_enabled": True},
            IdeFeatures(expression_level_coverage_enabled=True),
        )
        assert_ide_features_equal(
            {"expression_level_coverage_enabled": False},
            IdeFeatures(expression_level_coverage_enabled=False),
        )
        assert_ide_features_raises({"expression_level_coverage_enabled": 42})

        assert_ide_features_equal(
            {"consume_unsaved_changes_enabled": True},
            IdeFeatures(consume_unsaved_changes_enabled=True),
        )
        assert_ide_features_equal(
            {"consume_unsaved_changes_enabled": False},
            IdeFeatures(consume_unsaved_changes_enabled=False),
        )
        assert_ide_features_raises({"consume_unsaved_changes_enabled": 42})

    def test_merge(self) -> None:
        def assert_merged(
            base_ide_features: Optional[IdeFeatures],
            override_ide_features: Optional[IdeFeatures],
            expected: Optional[IdeFeatures],
        ) -> None:
            self.assertEqual(
                IdeFeatures.merge_optional(
                    base_ide_features,
                    override_ide_features,
                ),
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
        assert_merged(
            IdeFeatures(expression_level_coverage_enabled=True),
            None,
            IdeFeatures(expression_level_coverage_enabled=True),
        )
        assert_merged(
            None,
            IdeFeatures(expression_level_coverage_enabled=True),
            IdeFeatures(expression_level_coverage_enabled=True),
        )
        assert_merged(
            IdeFeatures(expression_level_coverage_enabled=False),
            IdeFeatures(expression_level_coverage_enabled=True),
            IdeFeatures(expression_level_coverage_enabled=True),
        )
        assert_merged(
            IdeFeatures(expression_level_coverage_enabled=True),
            IdeFeatures(expression_level_coverage_enabled=False),
            IdeFeatures(expression_level_coverage_enabled=False),
        )
        assert_merged(
            IdeFeatures(consume_unsaved_changes_enabled=True),
            None,
            IdeFeatures(consume_unsaved_changes_enabled=True),
        )
        assert_merged(
            None,
            IdeFeatures(consume_unsaved_changes_enabled=True),
            IdeFeatures(consume_unsaved_changes_enabled=True),
        )
        assert_merged(
            IdeFeatures(consume_unsaved_changes_enabled=False),
            IdeFeatures(consume_unsaved_changes_enabled=True),
            IdeFeatures(consume_unsaved_changes_enabled=True),
        )
        assert_merged(
            IdeFeatures(consume_unsaved_changes_enabled=True),
            IdeFeatures(consume_unsaved_changes_enabled=False),
            IdeFeatures(consume_unsaved_changes_enabled=False),
        )
