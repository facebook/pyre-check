# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

import testslide

from .. import (
    command_arguments,
    commands,
    configuration,
    pyre,
    recently_used_configurations,
    statistics,
)
from ..exceptions import EnvironmentException
from ..pyre import _set_default_command
from .mocks import mock_incremental_command
from .setup import (
    ensure_directories_exists,
    switch_working_directory,
    write_configuration_file,
)


def _get_arguments() -> argparse.Namespace:
    arguments = argparse.Namespace()
    arguments.local_configuration = None
    arguments.version = False
    arguments.debug = False
    arguments.sequential = False
    arguments.strict = False
    arguments.additional_check = []
    arguments.show_error_traces = False
    arguments.output = "text"
    arguments.enable_profiling = False
    arguments.enable_memory_profiling = False
    arguments.noninteractive = True
    arguments.logging_sections = None
    arguments.log_identifier = None
    arguments.logger = None
    arguments.formatter = None
    arguments.targets = ["//foo"]
    arguments.use_buck_builder = False
    arguments.use_buck_source_database = True
    arguments.source_directories = []
    arguments.filter_directory = []
    arguments.buck_mode = None
    arguments.no_saved_state = False
    arguments.search_path = []
    arguments.binary = None
    arguments.buck_builder_binary = None
    arguments.exclude = []
    arguments.typeshed = None
    arguments.save_initial_state_to = None
    arguments.load_initial_state_from = None
    arguments.changed_files_path = None
    arguments.saved_state_project = None
    arguments.dot_pyre_directory = None
    arguments.features = None
    arguments.command = commands.Check.from_arguments
    return arguments


class PyreTest(unittest.TestCase):
    @patch.object(commands.Start, "run")
    @patch.object(commands.Persistent, "run_null_server")
    def test_persistent_integration(
        self, run_null_server: MagicMock, run_start: MagicMock
    ) -> None:
        run_start.side_effect = commands.ClientException
        self.assertEqual(pyre.main(["persistent"]), 2)
        run_null_server.assert_not_called()

        run_start.side_effect = EnvironmentException
        self.assertEqual(pyre.main(["persistent"]), 0)
        run_null_server.assert_has_calls([call(timeout=3600 * 12)])

    # pyre-fixme[56]: Argument `shutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "which", return_value=True)
    def test_set_default_command__watchman_exists(self, which: MagicMock) -> None:
        arguments = argparse.Namespace()
        _set_default_command(arguments)
        self.assertEqual(arguments.command, commands.Incremental.from_arguments)

    # pyre-fixme[56]: Argument `shutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "which", return_value=False)
    def test_set_default_command__no_watchman(self, which: MagicMock) -> None:
        arguments = argparse.Namespace()
        _set_default_command(arguments)
        self.assertEqual(arguments.command, commands.Check.from_arguments)

    # pyre-fixme[56]: Argument `tools.pyre.client.statistics` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(statistics, "log")
    def test_log_statistics(self, statistics_log: MagicMock) -> None:
        test_configuration = configuration.Configuration(
            project_root="irrelevant", dot_pyre_directory=Path(".pyre"), logger="logger"
        )
        command = mock_incremental_command(test_configuration)
        pyre._log_statistics(command, 0.0, "foo", "bar", 42)
        statistics_log.assert_called_once()

    # pyre-fixme[56]: Argument `tools.pyre.client.statistics` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(statistics, "log")
    def test_log_statistics__should_rerun(self, statistics_log: MagicMock) -> None:
        test_configuration = configuration.Configuration(
            project_root="irrelevant", dot_pyre_directory=Path(".pyre"), logger="logger"
        )
        command = mock_incremental_command(test_configuration)
        pyre._log_statistics(command, 0.0, "foo", "bar", 42, should_log=False)
        statistics_log.assert_not_called()


class CreateConfigurationWithRetryTest(testslide.TestCase):
    def test_create_configuration_with_retry__no_retry(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, [".pyre", "local"])
            write_configuration_file(root_path, {})

            with switch_working_directory(root_path):
                try:
                    pyre._create_configuration_with_retry(
                        command_arguments.CommandArguments(
                            local_configuration=None,
                            source_directories=["."],
                            dot_pyre_directory=Path(".pyre"),
                        ),
                        base_directory=Path(root),
                    )
                except configuration.InvalidConfiguration:
                    self.fail("Unexpected InvalicConfiguration failure!")

    def test_create_configuration_with_retry__no_recent_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, [".pyre", "local"])
            write_configuration_file(root_path, {})

            with switch_working_directory(root_path):
                with self.assertRaises(configuration.InvalidConfiguration):
                    pyre._create_configuration_with_retry(
                        command_arguments.CommandArguments(
                            local_configuration=None,
                            source_directories=[],
                            dot_pyre_directory=Path(".pyre"),
                        ),
                        base_directory=Path(root),
                    )

    def test_create_configuration_with_retry__success(self) -> None:
        self.mock_callable(
            recently_used_configurations, "prompt_user_for_local_root"
        ).to_return_value("local").and_assert_called_once()

        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, [".pyre", "local"])
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path, {"source_directories": ["."]}, relative="local"
            )
            recently_used_configurations.Cache(root_path / ".pyre").put("local")

            with switch_working_directory(root_path):
                test_configuration = pyre._create_configuration_with_retry(
                    command_arguments.CommandArguments(
                        local_configuration=None,
                        source_directories=[],
                        dot_pyre_directory=Path(".pyre"),
                    ),
                    base_directory=Path(root),
                )
                self.assertEqual(
                    test_configuration.local_root, str(root_path / "local")
                )

    def test_create_configuration_with_retry__fail(self) -> None:
        self.mock_callable(
            recently_used_configurations, "prompt_user_for_local_root"
        ).to_return_value("local2").and_assert_called_once()

        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, [".pyre", "local", "local2"])
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path, {"source_directories": ["."]}, relative="local"
            )
            recently_used_configurations.Cache(root_path / ".pyre").put("local2")

            with switch_working_directory(root_path):
                with self.assertRaises(configuration.InvalidConfiguration):
                    pyre._create_configuration_with_retry(
                        command_arguments.CommandArguments(
                            local_configuration=None,
                            source_directories=[],
                            dot_pyre_directory=Path(".pyre"),
                        ),
                        base_directory=Path(root),
                    )

    def test_create_configuration_with_retry__invalid_user_input(self) -> None:
        self.mock_callable(
            recently_used_configurations, "prompt_user_for_local_root"
        ).to_return_value(None).and_assert_called_once()

        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, [".pyre", "local"])
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path, {"source_directories": ["."]}, relative="local"
            )
            recently_used_configurations.Cache(root_path / ".pyre").put("local")

            with switch_working_directory(root_path):
                with self.assertRaises(configuration.InvalidConfiguration):
                    pyre._create_configuration_with_retry(
                        command_arguments.CommandArguments(
                            local_configuration=None,
                            source_directories=[],
                            dot_pyre_directory=Path(".pyre"),
                        ),
                        base_directory=Path(root),
                    )
