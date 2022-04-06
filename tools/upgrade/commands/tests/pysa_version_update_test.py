# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, mock_open, patch

from ... import upgrade
from ...repository import Repository
from ..pysa_version_update import Configuration, PysaVersionUpdate


repository = Repository()


class UpdatePysaVersionTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch(f"{upgrade.__name__}.Repository.commit_changes")
    @patch.object(
        Configuration, "find_project_configuration", return_value=Path("/root")
    )
    @patch.object(Configuration, "set_pysa_version")
    @patch.object(Configuration, "write")
    @patch("builtins.open")
    def test_run_pysa_version_update(
        self,
        open_mock,
        configuration_write,
        configuration_set_pysa_version,
        find_project_configuration,
        commit_changes,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.hash = "new"
        arguments.no_commit = False
        with patch("json.dump"):
            mocks = [
                mock_open(read_data='{"pysa_version": "old"}').return_value,
            ]
            open_mock.side_effect = mocks

            PysaVersionUpdate.from_arguments(arguments, repository).run()
            configuration_set_pysa_version.assert_has_calls([call("new")])
            configuration_write.assert_has_calls([call()])
            commit_changes.assert_called_once_with(
                commit=True,
                title="Update global configuration pysa version",
                summary="Automatic upgrade to hash `new`",
                ignore_failures=True,
            )
