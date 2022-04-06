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
from ..global_version_update import Configuration, GlobalVersionUpdate


repository = Repository()


class UpdateGlobalVersionTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch(f"{upgrade.__name__}.Repository.commit_changes")
    @patch.object(
        Configuration, "find_project_configuration", return_value=Path("/root")
    )
    @patch.object(Configuration, "set_version")
    @patch.object(Configuration, "write")
    @patch.object(
        Configuration,
        "gather_local_configurations",
        return_value=[
            Configuration(
                Path("/root/a/.pyre_configuration.local"), {"use_buck_builder": False}
            ),
            Configuration(
                Path("/root/b/.pyre_configuration.local"), {"use_buck_builder": True}
            ),
        ],
    )
    @patch.object(upgrade.Fixme, "run")
    @patch("builtins.open")
    def test_run_global_version_update(
        self,
        open_mock,
        run_fixme,
        gather_local_configurations,
        configuration_write,
        configuration_set_version,
        find_project_configuration,
        commit_changes,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.hash = "abcd"
        arguments.paths = []
        arguments.no_commit = False
        with patch("json.dump"):
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data='{"use_buck_builder": false}').return_value,
                mock_open(read_data='{"use_buck_builder": true}').return_value,
            ]
            open_mock.side_effect = mocks

            GlobalVersionUpdate.from_arguments(arguments, repository).run()
            configuration_set_version.assert_has_calls(
                [call("abcd"), call("old"), call("old")]
            )
            configuration_write.assert_has_calls([call(), call(), call()])
            commit_changes.assert_called_once_with(
                commit=True,
                title="Update pyre global configuration version",
                summary="Automatic upgrade to hash `abcd`",
                ignore_failures=True,
            )

        # Paths passed from arguments will override the local configuration list
        # Therefore, we only read the first json configuration.
        subprocess.reset_mock()
        configuration_set_version.reset_mock()
        configuration_write.reset_mock()
        arguments.paths = [Path("foo/bar")]
        with patch("json.dump"):
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks

            GlobalVersionUpdate.from_arguments(arguments, repository).run()
            configuration_set_version.assert_has_calls([call("abcd"), call("old")])
            configuration_write.assert_has_calls([call(), call()])
            subprocess.assert_has_calls([])

        # Run fixme if global version has sources.
        subprocess.reset_mock()
        configuration_set_version.reset_mock()
        configuration_write.reset_mock()
        commit_changes.reset_mock()
        arguments.paths = []
        with patch("json.dump"):
            mocks = [
                mock_open(
                    read_data='{"version": "old", "source_directories": ["source"]}'
                ).return_value,
                mock_open(read_data='{"use_buck_builder": false}').return_value,
                mock_open(read_data='{"use_buck_builder": true}').return_value,
            ]
            open_mock.side_effect = mocks

            GlobalVersionUpdate.from_arguments(arguments, repository).run()
            configuration_set_version.assert_has_calls(
                [call("abcd"), call("old"), call("old")]
            )
            configuration_write.assert_has_calls([call(), call(), call()])
            run_fixme.assert_called_once()
            commit_changes.assert_called_once_with(
                commit=True,
                title="Update pyre global configuration version",
                summary="Automatic upgrade to hash `abcd`",
                ignore_failures=True,
            )
