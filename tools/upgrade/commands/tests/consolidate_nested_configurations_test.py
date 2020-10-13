# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from ... import errors
from ...repository import Repository
from .. import consolidate_nested_configurations
from ..consolidate_nested_configurations import (
    ConsolidateNestedConfigurations,
    ErrorSuppressingCommand,
)


repository = Repository()


class ConsolidateNestedConfigurationsTest(unittest.TestCase):
    def test_gather_nested_configuration_mapping(self) -> None:
        arguments = MagicMock()
        configurations = []
        expected_mapping = {}
        mapping = ConsolidateNestedConfigurations.from_arguments(
            arguments, repository
        ).gather_nested_configuration_mapping(configurations)
        self.assertEqual(expected_mapping, mapping)

        configurations = [
            "a/.pyre_configuration.local",
            "b/.pyre_configuration.local",
            "a/b/.pyre_configuration.local",
            "aa/.pyre_configuration.local",
        ]
        expected_mapping = {
            "a/.pyre_configuration.local": ["a/b/.pyre_configuration.local"],
            "aa/.pyre_configuration.local": [],
            "b/.pyre_configuration.local": [],
        }
        mapping = ConsolidateNestedConfigurations.from_arguments(
            arguments, repository
        ).gather_nested_configuration_mapping(configurations)
        self.assertEqual(expected_mapping, mapping)

    @patch("builtins.open")
    @patch(f"{consolidate_nested_configurations.__name__}.Repository.remove_paths")
    @patch(f"{consolidate_nested_configurations.__name__}.Configuration.get_errors")
    @patch(
        f"{consolidate_nested_configurations.__name__}.Configuration.deduplicate_targets"
    )
    @patch(f"{consolidate_nested_configurations.__name__}.Configuration.add_strict")
    @patch.object(ErrorSuppressingCommand, "_apply_suppressions")
    def test_consolidate(
        self,
        apply_suppressions,
        add_strict,
        deduplicate_targets,
        get_errors,
        remove_paths,
        open_mock,
    ) -> None:
        arguments = MagicMock()
        get_errors.return_value = errors.Errors([])
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=json.dumps({"targets": ["//a/..."]})).return_value,
                mock_open(read_data=json.dumps({"targets": ["//b/..."]})).return_value,
                mock_open(read_data=json.dumps({"targets": ["//x/..."]})).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            ConsolidateNestedConfigurations.from_arguments(
                arguments, repository
            ).consolidate(
                Path("subdirectory/.pyre_configuration.local"),
                [
                    Path("subdirectory/a/.pyre_configuration.local"),
                    Path("subdirectory/b/.pyre_configuration.local"),
                ],
            )
            expected_configuration_contents = {
                "targets": ["//x/...", "//a/...", "//b/..."]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/a/.pyre_configuration.local"), "r"),
                    call(Path("subdirectory/b/.pyre_configuration.local"), "r"),
                    call(Path("subdirectory/.pyre_configuration.local"), "r"),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[3], indent=2, sort_keys=True
            )
        deduplicate_targets.assert_called_once()
        remove_paths.assert_called_once_with(
            [
                Path("subdirectory/a/.pyre_configuration.local"),
                Path("subdirectory/b/.pyre_configuration.local"),
            ]
        )

    @patch(f"{consolidate_nested_configurations.__name__}.Repository.submit_changes")
    @patch(f"{consolidate_nested_configurations.__name__}.find_files")
    @patch(
        f"{consolidate_nested_configurations.__name__}.ConsolidateNestedConfigurations.consolidate"
    )
    @patch(
        f"{consolidate_nested_configurations.__name__}.ConsolidateNestedConfigurations.gather_nested_configuration_mapping"
    )
    def test_run(self, gather, consolidate, find_files, submit_changes) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        arguments.lint = False
        arguments.no_commit = False

        # Skip if no configurations found
        find_files.return_value = []
        ConsolidateNestedConfigurations.from_arguments(arguments, repository).run()
        gather.assert_not_called()
        consolidate.assert_not_called()

        # Skip if only one configuration found
        find_files.return_value = ["subdirectory/.pyre_configuration.local"]
        ConsolidateNestedConfigurations.from_arguments(arguments, repository).run()
        gather.assert_not_called()
        consolidate.assert_not_called()

        # Consolidate with existing topmost configuration
        configurations = [
            "subdirectory/.pyre_configuration.local",
            "subdirectory/a/.pyre_configuration.local",
            "subdirectory/b/.pyre_configuration.local",
        ]
        find_files.return_value = configurations
        gather.return_value = {
            "subdirectory/.pyre_configuration.local": [
                "subdirectory/a/.pyre_configuration.local",
                "subdirectory/b/.pyre_configuration.local",
            ]
        }
        ConsolidateNestedConfigurations.from_arguments(arguments, repository).run()
        gather.assert_called_once_with(configurations)
        consolidate.assert_called_once_with(
            Path("subdirectory/.pyre_configuration.local"),
            [
                Path("subdirectory/a/.pyre_configuration.local"),
                Path("subdirectory/b/.pyre_configuration.local"),
            ],
        )

        # Consolidate with no existing topmost configuration
        gather.reset_mock()
        consolidate.reset_mock()
        configurations = [
            "subdirectory/a/.pyre_configuration.local",
            "subdirectory/b/.pyre_configuration.local",
        ]
        find_files.return_value = configurations
        gather.return_value = {
            "subdirectory/a/.pyre_configuration.local": [],
            "subdirectory/b/.pyre_configuration.local": [],
        }
        ConsolidateNestedConfigurations.from_arguments(arguments, repository).run()
        gather.assert_called_once_with(configurations)
        consolidate.assert_not_called()
