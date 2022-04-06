# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, mock_open, patch

from ...repository import Repository
from .. import expand_target_coverage
from ..expand_target_coverage import ErrorSuppressingCommand, ExpandTargetCoverage


repository = Repository()


class ExpandTargetCoverageTest(unittest.TestCase):
    @patch("builtins.open")
    @patch(f"{expand_target_coverage.__name__}.Repository.commit_changes")
    @patch(f"{expand_target_coverage.__name__}.Configuration.find_local_configuration")
    @patch(f"{expand_target_coverage.__name__}.Configuration.deduplicate_targets")
    @patch.object(ErrorSuppressingCommand, "_get_and_suppress_errors")
    @patch(f"{expand_target_coverage.__name__}.Repository.format")
    @patch(f"{expand_target_coverage.__name__}.find_files")
    def test_run_expand_target_coverage(
        self,
        find_files,
        repository_format,
        get_and_suppress_errors,
        deduplicate_targets,
        find_local_configuration,
        commit_changes,
        open_mock,
    ) -> None:
        arguments = MagicMock()
        arguments.local_configuration = "subdirectory"
        arguments.lint = False
        arguments.fixme_threshold = None
        arguments.no_commit = False
        find_local_configuration.return_value = Path(
            "subdirectory/.pyre_configuration.local"
        )

        # Skip if nested configurations found
        find_files.return_value = ["nested/.pyre_configuration.local"]
        ExpandTargetCoverage.from_arguments(arguments, repository).run()
        open_mock.assert_not_called()
        get_and_suppress_errors.assert_not_called()

        # Skip if target is already expanded
        find_files.return_value = []
        configuration_contents = json.dumps({"targets": ["//subdirectory/..."]})
        get_and_suppress_errors.reset_mock()
        deduplicate_targets.reset_mock()
        open_mock.reset_mock()
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
        ExpandTargetCoverage.from_arguments(arguments, repository).run()
        deduplicate_targets.assert_not_called()
        get_and_suppress_errors.assert_not_called()

        # Expand coverage and suppress errors
        get_and_suppress_errors.reset_mock()
        open_mock.reset_mock()
        configuration_contents = json.dumps({"targets": ["//existing:target"]})
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            ExpandTargetCoverage.from_arguments(arguments, repository).run()
            expected_configuration_contents = {
                "targets": ["//existing:target", "//subdirectory/..."]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration.local"), "r"),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
        deduplicate_targets.assert_called_once()
        get_and_suppress_errors.assert_called_once()

        # Expand coverage with no errors returned
        deduplicate_targets.reset_mock()
        get_and_suppress_errors.reset_mock()
        open_mock.reset_mock()
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            ExpandTargetCoverage.from_arguments(arguments, repository).run()
            expected_configuration_contents = {
                "targets": ["//existing:target", "//subdirectory/..."]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration.local"), "r"),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
        deduplicate_targets.assert_called_once()
        get_and_suppress_errors.assert_called_once()

        # Expand coverage hitting fixme threshold
        deduplicate_targets.reset_mock()
        get_and_suppress_errors.reset_mock()
        open_mock.reset_mock()
        arguments.fixme_threshold = 1
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            ExpandTargetCoverage.from_arguments(arguments, repository).run()
            expected_configuration_contents = {
                "targets": ["//existing:target", "//subdirectory/..."]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration.local"), "r"),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
        deduplicate_targets.assert_called_once()
        get_and_suppress_errors.assert_called_once()
