# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ... import upgrade
from ...repository import Repository
from ..fix_configuration import Configuration, FixConfiguration


repository = Repository()


def _raise_on_bad_target(build_command, timeout) -> str:
    if "bad" in build_command[-1]:
        raise subprocess.CalledProcessError(cmd=build_command, returncode=1)
    elif "timeout" in build_command[-1]:
        raise subprocess.TimeoutExpired(cmd=build_command, timeout=30)
    else:
        return "stdout"


class FixmeConfigurationTest(unittest.TestCase):
    @patch("subprocess.check_output")
    @patch.object(Configuration, "get_errors")
    @patch(f"{upgrade.__name__}.Repository.commit_changes")
    @patch(f"{upgrade.__name__}.Repository.remove_paths")
    def test_run_fix_configuration(
        self, remove_paths, commit_changes, get_errors, check_output
    ) -> None:
        arguments = MagicMock()
        arguments.lint = False
        arguments.no_commit = False
        with tempfile.TemporaryDirectory() as root:
            arguments.path = Path(root)
            configuration_path = os.path.join(root, ".pyre_configuration.local")

            # Healthy configuration.
            with open(configuration_path, "w+") as configuration_file:
                json.dump({"targets": ["//my:target"]}, configuration_file)
                configuration_file.seek(0)
                FixConfiguration.from_arguments(arguments, repository).run()
                get_errors.assert_called_once()
                commit_changes.assert_called_once()

            # Remove bad targets.
            get_errors.reset_mock()
            commit_changes.reset_mock()
            check_output.side_effect = _raise_on_bad_target
            with open(configuration_path, "w+") as configuration_file:
                json.dump(
                    {
                        "targets": [
                            "//good:target",
                            "//timeout:target",
                            "//bad:target1",
                            "//bad:target2",
                        ]
                    },
                    configuration_file,
                )
                configuration_file.seek(0)
                FixConfiguration.from_arguments(arguments, repository).run()
                configuration_file.seek(0)
                self.assertEqual(
                    json.load(configuration_file),
                    {"targets": ["//good:target", "//timeout:target"]},
                )
                get_errors.assert_called_once()
                commit_changes.assert_called_once()

            # Remove configuration with only bad targets.
            get_errors.reset_mock()
            commit_changes.reset_mock()
            with open(configuration_path, "w+") as configuration_file:
                json.dump(
                    {"targets": ["//bad:target1", "//bad:target2"]}, configuration_file
                )
                configuration_file.seek(0)
                FixConfiguration.from_arguments(arguments, repository).run()
                remove_paths.assert_called_once_with([Path(configuration_path)])
                get_errors.assert_not_called()
                commit_changes.assert_called_once()

            # Consolidate nested configurations.
            remove_paths.reset_mock()
            get_errors.reset_mock()
            commit_changes.reset_mock()
            with open(configuration_path, "w+") as configuration_file:
                json.dump({"targets": ["//parent:target"]}, configuration_file)
                configuration_file.seek(0)
                subdirectory = os.path.join(root, "subdirectory")
                os.mkdir(subdirectory)
                nested_configuration_path = os.path.join(
                    subdirectory, ".pyre_configuration.local"
                )
                arguments.path = Path(subdirectory)
                with open(nested_configuration_path, "w+") as nested_configuration_file:
                    json.dump(
                        {"targets": ["//nested:target"]}, nested_configuration_file
                    )
                    nested_configuration_file.seek(0)
                    FixConfiguration.from_arguments(arguments, repository).run()
                    configuration_file.seek(0)
                    parent_contents = json.load(configuration_file)
                    self.assertEqual(
                        parent_contents,
                        {"targets": ["//parent:target", "//nested:target"]},
                    )
                    remove_paths.assert_called_once_with(
                        [Path(nested_configuration_path)]
                    )
                    get_errors.assert_called_once()
                    commit_changes.assert_called_once()

            # Skip consolidation on properly ignored nested configurations.
            remove_paths.reset_mock()
            get_errors.reset_mock()
            commit_changes.reset_mock()
            with open(configuration_path, "w+") as configuration_file:
                json.dump(
                    {
                        "targets": ["//parent:target"],
                        "ignore_all_errors": ["subdirectory"],
                    },
                    configuration_file,
                )
                configuration_file.seek(0)
                subdirectory = os.path.join(root, "subdirectory")
                nested_configuration_path = os.path.join(
                    subdirectory, ".pyre_configuration.local"
                )
                arguments.path = Path(subdirectory)
                with open(nested_configuration_path, "w+") as nested_configuration_file:
                    json.dump(
                        {"targets": ["//nested:target"]}, nested_configuration_file
                    )
                    nested_configuration_file.seek(0)
                    FixConfiguration.from_arguments(arguments, repository).run()
                    configuration_file.seek(0)
                    parent_contents = json.load(configuration_file)
                    self.assertEqual(
                        parent_contents,
                        {
                            "targets": ["//parent:target"],
                            "ignore_all_errors": ["subdirectory"],
                        },
                    )
                    remove_paths.assert_not_called()
                    get_errors.assert_called_once()
                    commit_changes.assert_called_once()
