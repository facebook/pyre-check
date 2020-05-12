# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from ... import errors, upgrade
from ...repository import Repository
from .. import consolidate_nested_configurations
from ..consolidate_nested_configurations import ConsolidateNestedConfigurations


repository = Repository()


class ConsolidateNestedConfigurationsTest(unittest.TestCase):
    @patch("builtins.open")
    @patch(f"{consolidate_nested_configurations.__name__}.Repository.submit_changes")
    @patch(f"{consolidate_nested_configurations.__name__}.Repository.remove_paths")
    @patch(
        f"{consolidate_nested_configurations.__name__}.Configuration.find_local_configuration"
    )
    @patch(f"{consolidate_nested_configurations.__name__}.Configuration.get_errors")
    @patch(
        f"{consolidate_nested_configurations.__name__}.Configuration.deduplicate_targets"
    )
    @patch(f"{consolidate_nested_configurations.__name__}.Configuration.add_strict")
    @patch.object(upgrade.ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{consolidate_nested_configurations.__name__}.find_files")
    def test_consolidate_nested_configurations(
        self,
        find_files,
        suppress_errors,
        add_strict,
        deduplicate_targets,
        get_errors,
        find_local_configuration,
        remove_paths,
        submit_changes,
        open_mock,
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        arguments.lint = False
        arguments.no_commit = False
        find_local_configuration.return_value = Path(
            "subdirectory/.pyre_configuration.local"
        )

        # Skip if no configurations found
        find_files.return_value = []
        ConsolidateNestedConfigurations(arguments, repository).run()
        open_mock.assert_not_called()
        remove_paths.assert_not_called()

        # Skip if only one configuration found
        find_files.return_value = ["subdirectory/.pyre_configuration.local"]
        ConsolidateNestedConfigurations(arguments, repository).run()
        open_mock.assert_not_called()
        remove_paths.assert_not_called()

        # Consolidate with existing topmost configuration
        find_files.return_value = [
            "subdirectory/.pyre_configuration.local",
            "subdirectory/a/.pyre_configuration.local",
            "subdirectory/b/.pyre_configuration.local",
        ]
        get_errors.return_value = errors.Errors([])
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=json.dumps({"targets": ["//a/..."]})).return_value,
                mock_open(read_data=json.dumps({"targets": ["//b/..."]})).return_value,
                mock_open(read_data=json.dumps({"targets": ["//x/..."]})).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            ConsolidateNestedConfigurations(arguments, repository).run()
            expected_configuration_contents = {
                "targets": ["//x/...", "//a/...", "//b/..."]
            }
            open_mock.assert_has_calls(
                [
                    call("subdirectory/a/.pyre_configuration.local"),
                    call("subdirectory/b/.pyre_configuration.local"),
                    call("subdirectory/.pyre_configuration.local"),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[3], indent=2, sort_keys=True
            )
        deduplicate_targets.assert_called_once()
        remove_paths.assert_called_once_with(
            [
                "subdirectory/a/.pyre_configuration.local",
                "subdirectory/b/.pyre_configuration.local",
            ]
        )

        # Consolidate with no existing topmost configuration
        deduplicate_targets.reset_mock()
        remove_paths.reset_mock()
        find_files.return_value = [
            "subdirectory/a/.pyre_configuration.local",
            "subdirectory/b/.pyre_configuration.local",
        ]
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(
                    read_data=json.dumps({"targets": ["//a/..."], "strict": "true"})
                ).return_value,
                mock_open(
                    read_data=json.dumps({"targets": ["//b/..."], "strict": "true"})
                ).return_value,
                mock_open(read_data=json.dumps({})).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            ConsolidateNestedConfigurations(arguments, repository).run()
            open_mock.assert_has_calls(
                [
                    call("subdirectory/a/.pyre_configuration.local"),
                    call("subdirectory/b/.pyre_configuration.local"),
                ]
            )
            dump_mock.assert_not_called()
        remove_paths.assert_not_called()
