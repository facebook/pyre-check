# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from ... import errors, filesystem
from ...repository import Repository
from .. import expand_target_coverage
from ..expand_target_coverage import ErrorSuppressingCommand, ExpandTargetCoverage


repository = Repository()


class ExpandTargetCoverageTest(unittest.TestCase):
    @patch("builtins.open")
    @patch(f"{expand_target_coverage.__name__}.Repository.submit_changes")
    @patch(f"{expand_target_coverage.__name__}.Configuration.find_local_configuration")
    @patch(f"{expand_target_coverage.__name__}.Configuration.get_errors")
    @patch(f"{expand_target_coverage.__name__}.Configuration.deduplicate_targets")
    @patch(f"{expand_target_coverage.__name__}.add_local_mode")
    @patch.object(ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{expand_target_coverage.__name__}.Repository.format")
    @patch(f"{expand_target_coverage.__name__}.find_files")
    def test_run_expand_target_coverage(
        self,
        find_files,
        repository_format,
        suppress_errors,
        add_local_mode,
        deduplicate_targets,
        get_errors,
        find_local_configuration,
        submit_changes,
        open_mock,
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        arguments.lint = False
        arguments.fixme_threshold = None
        arguments.no_commit = False
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            },
            {
                "line": 3,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            },
        ]
        find_local_configuration.return_value = Path(
            "subdirectory/.pyre_configuration.local"
        )

        # Skip if nested configurations found
        find_files.return_value = ["nested/.pyre_configuration.local"]
        ExpandTargetCoverage.from_arguments(arguments, repository).run()
        open_mock.assert_not_called()
        suppress_errors.assert_not_called()

        # Expand coverage and suppress errors
        suppress_errors.reset_mock()
        open_mock.reset_mock()
        find_files.return_value = []
        get_errors.return_value = errors.Errors(pyre_errors)
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
        suppress_errors.assert_has_calls([call(errors.Errors(pyre_errors))])
        add_local_mode.assert_not_called()

        # Expand coverage with no errors returned
        deduplicate_targets.reset_mock()
        suppress_errors.reset_mock()
        open_mock.reset_mock()
        get_errors.return_value = errors.Errors([])
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
        suppress_errors.assert_not_called()
        add_local_mode.assert_not_called()

        # Expand coverage hitting fixme threshold
        deduplicate_targets.reset_mock()
        suppress_errors.reset_mock()
        open_mock.reset_mock()
        arguments.fixme_threshold = 1
        get_errors.return_value = errors.Errors(pyre_errors)
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
        suppress_errors.assert_not_called()
        add_local_mode.assert_called_once_with("local.py", filesystem.LocalMode.IGNORE)
