# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import fcntl
import unittest
from unittest.mock import MagicMock, call, mock_open, patch

from .. import monitor  # noqa
from ... import commands  # noqa
from ...filesystem import AnalysisDirectory, acquire_lock  # noqa
from .command_test import mock_arguments, mock_configuration


class StartTest(unittest.TestCase):
    @patch("fcntl.lockf")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(monitor.Monitor, "daemonize")
    def test_start(self, _daemonize, get_directories_to_analyze, lock_file) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        configuration.number_of_workers = 5

        analysis_directory = AnalysisDirectory(".")
        # Check start without watchman.
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client:
            arguments.no_watchman = True
            command = commands.Start(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)

        analysis_directory = AnalysisDirectory(".")

        # This magic is necessary to test, because the inner call to ping a server is
        # always non-blocking.
        def pass_when_blocking(file_descriptor, command):
            if not pass_when_blocking.failed and (command & fcntl.LOCK_NB):
                pass_when_blocking.failed = True
                raise OSError(errno.EAGAIN, "Only accepting blocking calls.")

        pass_when_blocking.failed = False

        lock_file.side_effect = pass_when_blocking
        # EAGAINs get caught.
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client:
            arguments.no_watchman = True
            command = commands.Start(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
        lock_file.side_effect = None

        def raise_mount_error(fileno, command):
            raise OSError(errno.ENOTCONN)

        lock_file.side_effect = raise_mount_error
        # Check that the command errors on OS errors other than EAGAIN.
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client:
            arguments.no_watchman = True
            command = commands.Start(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            with self.assertRaises(OSError):
                command.run()
            call_client.assert_not_called()
        lock_file.side_effect = None

        # Shared analysis directories are prepared when starting.
        shared_analysis_directory = MagicMock()
        shared_analysis_directory.get_root = lambda: "."
        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(shared_analysis_directory, "prepare") as prepare:
            arguments = mock_arguments(no_watchman=True)
            configuration = mock_configuration(version_hash="hash")
            command = commands.Start(
                arguments, configuration, shared_analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
            prepare.assert_called_once_with()

    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_start_flags(self, get_directories_to_analyze):
        # Check start with watchman.
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )

        arguments = mock_arguments(no_watchman=True, terminal=True)
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-terminal",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )

        # Check filter directories.
        arguments = mock_arguments(no_watchman=True)
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        with patch.object(command, "_get_directories_to_analyze") as get_directories:
            get_directories.return_value = {"a", "b"}
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-filter-directories",
                    "a;b",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )

        # Check save-initial-state-to.
        arguments = mock_arguments(save_initial_state_to="/tmp")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-save-initial-state-to",
                "/tmp",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )

        # Check load-initial-state-from.
        arguments = mock_arguments(
            load_initial_state_from="/tmp/pyre_shared_memory",
            changed_files_path="/tmp/changed_files",
        )
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-load-state-from",
                "/tmp/pyre_shared_memory",
                "-changed-files-path",
                "/tmp/changed_files",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )
        # Both changed-files-path and load-initial-state-from must be not-None.
        arguments = mock_arguments(changed_files_path="/tmp/changed_files")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )
        # Check load-initial-state-from.
        arguments = mock_arguments(changed_files_path="/tmp/changed_files")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )
        # Check --saved-state-project.
        arguments = mock_arguments(saved_state_project="pyre/saved_state")
        command = commands.Start(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-saved-state-project",
                "pyre/saved_state",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )
