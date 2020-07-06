# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

from ... import upgrade
from ...repository import Repository
from ..fixme_single import Configuration, ErrorSuppressingCommand, FixmeSingle


repository = Repository()


class FixmeSingleTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(Configuration, "find_project_configuration", return_value=Path("."))
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "remove_version")
    @patch.object(Configuration, "get_errors")
    @patch.object(ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{upgrade.__name__}.Repository.submit_changes")
    def test_run_fixme_single(
        self,
        submit_changes,
        suppress_errors,
        get_errors,
        remove_version,
        configuration_write,
        find_configuration,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = True
        arguments.path = Path("local")
        arguments.error_source = "generate"
        arguments.lint = False
        arguments.no_commit = False
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle.from_arguments(arguments, repository).run()
            suppress_errors.assert_not_called()
            submit_changes.assert_not_called()

        configuration_contents = '{"version": 123}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle.from_arguments(arguments, repository).run()
            suppress_errors.assert_not_called()
            submit_changes.assert_called_once_with(
                commit=True, submit=True, title="Update pyre version for local"
            )

        suppress_errors.reset_mock()
        submit_changes.reset_mock()
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
            suppress_errors.assert_called_once_with(pyre_errors)
            submit_changes.assert_called_once_with(
                commit=True, submit=True, title="Update pyre version for local"
            )
