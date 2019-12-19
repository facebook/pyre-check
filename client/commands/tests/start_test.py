# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import errno
import fcntl
import os
import unittest
from unittest.mock import MagicMock, Mock, mock_open, patch

from ... import commands, configuration_monitor, project_files_monitor  # noqa
from ...analysis_directory import AnalysisDirectory
from ...commands import start  # noqa
from ...filesystem import acquire_lock  # noqa
from ..command import __name__ as client_name
from ..start import __name__ as start_name
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(commands.start.__name__)


@patch("{}.get_enabled_features".format(start_name), return_value={})
class StartTest(unittest.TestCase):
    @patch("{}.find_project_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    @patch("fcntl.lockf")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(configuration_monitor.ConfigurationMonitor, "daemonize")
    def test_start(
        self,
        _daemonize,
        get_directories_to_analyze,
        lock_file,
        find_local_root,
        find_project_root,
        get_enabled_features,
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        configuration.number_of_workers = 5

        analysis_directory = AnalysisDirectory(".")
        # Check start without watchman.
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            arguments.no_watchman = True
            command = commands.Start(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
            Monitor.assert_not_called()

        analysis_directory = AnalysisDirectory(".")
        # Check start with watchman.
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            arguments.no_watchman = False
            command = commands.Start(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
            Monitor.assert_called_once_with(configuration, ".", analysis_directory)
            Monitor.return_value.daemonize.assert_called_once_with()

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
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            arguments.no_watchman = True
            command = commands.Start(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
            Monitor.assert_not_called()

        lock_file.side_effect = None

        def raise_mount_error(fileno, command):
            raise OSError(errno.ENOTCONN)

        lock_file.side_effect = raise_mount_error
        # Check that the command errors on OS errors other than EAGAIN.
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            arguments.no_watchman = True
            command = commands.Start(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            with self.assertRaises(OSError):
                command.run()
            call_client.assert_not_called()
            Monitor.assert_not_called()

        lock_file.side_effect = None

        # Shared analysis directories are prepared when starting.
        shared_analysis_directory = MagicMock()
        shared_analysis_directory.get_root = lambda: "."
        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            shared_analysis_directory, "prepare"
        ) as prepare, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            arguments = mock_arguments(no_watchman=True)
            configuration = mock_configuration(version_hash="hash")
            command = commands.Start(
                arguments, original_directory, configuration, shared_analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
            prepare.assert_called_once_with()
            Monitor.assert_not_called()

    @patch("{}.find_project_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_start_flags(
        self,
        get_directories_to_analyze,
        find_local_root,
        find_project_root,
        get_enabled_features,
    ) -> None:
        # Check start with watchman.
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        arguments = mock_arguments(no_watchman=True, terminal=True)
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-terminal",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        # Check filter directories.
        arguments = mock_arguments(no_watchman=True)
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        with patch.object(command, "_get_directories_to_analyze") as get_directories:
            get_directories.return_value = {"a", "b"}
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-filter-directories",
                    "a;b",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

        # Check configuration-file-hash.
        arguments = mock_arguments(no_watchman=True)
        configuration = mock_configuration(version_hash="hash", file_hash="ABCD")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        with patch.object(command, "_get_directories_to_analyze") as get_directories:
            get_directories.return_value = {"a", "b"}
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-filter-directories",
                    "a;b",
                    "-configuration-file-hash",
                    "ABCD",
                    "-workers",
                    "5",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

        # Check save-initial-state-to.
        arguments = mock_arguments(save_initial_state_to="/tmp")
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-save-initial-state-to",
                "/tmp",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        # Check load-initial-state-from.
        arguments = mock_arguments(
            load_initial_state_from="/tmp/pyre_shared_memory",
            changed_files_path="/tmp/changed_files",
        )
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-load-state-from",
                "/tmp/pyre_shared_memory",
                "-changed-files-path",
                "/tmp/changed_files",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments(load_initial_state_from="/tmp/pyre_shared_memory")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-load-state-from",
                "/tmp/pyre_shared_memory",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments(changed_files_path="/tmp/changed_files")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        # Check --saved-state-project.
        arguments = mock_arguments(saved_state_project="pyre/saved_state")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-saved-state-project",
                "pyre/saved_state",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        # Check --no-saved-state.
        arguments = mock_arguments(
            saved_state_project="pyre/saved_state", no_saved_state=True
        )
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        arguments = mock_arguments(no_saved_state=True)
        arguments.load_initial_state_from = "/do/not/load"
        arguments.save_initial_state_to = "/do/not/save"
        arguments.changed_files_path = "/do/not/change"
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments(store_type_check_resolution=True)
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-store-type-check-resolution",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        arguments = mock_arguments(saved_state_project="pyre/saved_state")
        configuration = mock_configuration()
        configuration.local_configuration_root = os.path.join(
            os.getcwd(), "first/second"
        )
        configuration.version_hash = "hash"
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-saved-state-project",
                "pyre/saved_state",
                "-saved-state-metadata",
                "first$second",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        configuration.ignore_all_errors = ["/absolute/a", "/root/b"]
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-ignore-all-errors",
                "/absolute/a;/root/b",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments()
        arguments.incremental_style = commands.IncrementalStyle.FINE_GRAINED
        configuration = mock_configuration(version_hash="hash")
        configuration.ignore_all_errors = ["/absolute/a", "/root/b"]
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-ignore-all-errors",
                "/absolute/a;/root/b",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
                "-new-incremental-check",
            ],
        )
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        configuration.autocomplete = True
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
                "-autocomplete",
            ],
        )
        arguments = mock_arguments()
        arguments.enable_profiling = True
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-profiling-output",
                ".pyre/profiling.log",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments()
        arguments.enable_memory_profiling = True
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-memory-profiling-output",
                ".pyre/profiling.log",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
        arguments = mock_arguments()
        arguments.enable_profiling = True
        arguments.enable_memory_profiling = True
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-profiling-output",
                ".pyre/profiling.log",
                "-memory-profiling-output",
                ".pyre/profiling.log",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        # Incremental style override tests
        arguments = mock_arguments()
        arguments.incremental_style = None
        configuration = mock_configuration(version_hash="hash")
        get_enabled_features.return_value = {"enable_fine_grained_incremental": False}
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        arguments = mock_arguments()
        arguments.incremental_style = None
        configuration = mock_configuration(version_hash="hash")
        get_enabled_features.return_value = {"irrelevant_feature": True}
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )

        arguments = mock_arguments()
        arguments.incremental_style = None
        configuration = mock_configuration(version_hash="hash")
        get_enabled_features.return_value = {"enable_fine_grained_incremental": True}
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
                "-new-incremental-check",
            ],
        )

        # Command line override takes precedence
        arguments = mock_arguments()
        arguments.incremental_style = commands.IncrementalStyle.SHALLOW
        get_enabled_features.return_value = {"enable_fine_grained_incremental": True}
        configuration = mock_configuration(version_hash="hash")
        command = commands.Start(
            arguments, original_directory, configuration, AnalysisDirectory(".")
        )
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
                "-workers",
                "5",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2,path3",
            ],
        )
