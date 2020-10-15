# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

from ... import upgrade
from ...repository import Repository
from ..command import ErrorSuppressingCommand
from ..fixme_single import Configuration, FixmeSingle


repository = Repository()


class FixmeSingleTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(Configuration, "find_project_configuration", return_value=Path("/"))
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "remove_version")
    @patch.object(Configuration, "get_errors")
    @patch.object(ErrorSuppressingCommand, "_apply_suppressions")
    @patch(f"{upgrade.__name__}.Repository.commit_changes")
    def test_run_fixme_single(
        self,
        commit_changes: MagicMock,
        apply_suppressions: MagicMock,
        get_errors: MagicMock,
        remove_version: MagicMock,
        configuration_write: MagicMock,
        find_configuration: MagicMock,
        subprocess: MagicMock,
    ) -> None:
        arguments = MagicMock()
        arguments.path = Path("/local")
        arguments.error_source = "generate"
        arguments.lint = False
        arguments.no_commit = False
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle.from_arguments(arguments, repository).run()
            apply_suppressions.assert_not_called()
            commit_changes.assert_called_once_with(
                commit=True, title="Update pyre version for local"
            )

        commit_changes.reset_mock()
        configuration_contents = '{"version": 123}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle.from_arguments(arguments, repository).run()
            apply_suppressions.assert_not_called()
            commit_changes.assert_called_once_with(
                commit=True, title="Update pyre version for local"
            )

        apply_suppressions.reset_mock()
        commit_changes.reset_mock()
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
            }
        ]
        get_errors.return_value = pyre_errors
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle.from_arguments(arguments, repository).run()
            apply_suppressions.assert_called_once_with(pyre_errors)
            commit_changes.assert_called_once_with(
                commit=True, title="Update pyre version for local"
            )
