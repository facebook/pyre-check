# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import errno
import fcntl
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, mock_open, patch

from ... import (
    commands,
    configuration_monitor,
    filesystem,
    find_directories,
    project_files_monitor,
)
from ...analysis_directory import AnalysisDirectory
from ..command import ExitCode
from ..start import Start
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(commands.start.__name__)


class StartTest(unittest.TestCase):
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("fcntl.lockf")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(configuration_monitor.ConfigurationMonitor, "daemonize")
    def test_start(
        self,
        _daemonize,
        get_directories_to_analyze,
        lock_file,
        find_global_and_local_root,
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        configuration.number_of_workers = 5

        # Check start without watchman.
        analysis_directory = AnalysisDirectory(".")
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=False,
                incremental_style=commands.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
                analysis_directory=analysis_directory,
            ).run()
            call_client.assert_called_once_with(command=Start.NAME)
            Monitor.assert_not_called()

        # Check start with watchman.
        analysis_directory = AnalysisDirectory(".")
        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor:
            Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=True,
                incremental_style=commands.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
                analysis_directory=analysis_directory,
            ).run()
            call_client.assert_called_once_with(command=Start.NAME)
            Monitor.assert_called_once_with(configuration, "/root", analysis_directory)
            Monitor.return_value.daemonize.assert_called_once_with()

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
            Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=False,
                incremental_style=commands.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
                analysis_directory=analysis_directory,
            ).run()
            call_client.assert_called_once_with(command=Start.NAME)
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
            with self.assertRaises(OSError):
                Start(
                    arguments,
                    original_directory,
                    terminal=False,
                    store_type_check_resolution=False,
                    use_watchman=False,
                    incremental_style=commands.IncrementalStyle.FINE_GRAINED,
                    configuration=configuration,
                    analysis_directory=analysis_directory,
                ).run()
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
            arguments = mock_arguments()
            configuration = mock_configuration(version_hash="hash")
            Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=False,
                incremental_style=commands.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
                analysis_directory=shared_analysis_directory,
            ).run()
            call_client.assert_called_once_with(command=Start.NAME)
            prepare.assert_called_once_with()
            Monitor.assert_not_called()

        # Exit code is success when server already exists.
        mock_object = Mock()
        mock_object.__enter__ = Mock(return_value=(Mock(), None))
        mock_object.__exit__ = Mock(return_value=None)

        def raise_os_error(lock, blocking):
            if lock.endswith("server.lock"):
                raise OSError
            return mock_object

        with patch("builtins.open", mock_open()), patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            project_files_monitor, "ProjectFilesMonitor"
        ) as Monitor, patch.object(
            filesystem, "acquire_lock"
        ) as acquire_lock:
            acquire_lock.side_effect = raise_os_error
            command = Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=False,
                incremental_style=commands.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
                analysis_directory=analysis_directory,
            )
            command.run()
            self.assertEqual(command._exit_code, ExitCode.SUCCESS)

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_start_flags(
        self, get_directories_to_analyze, find_global_and_local_root
    ) -> None:
        flags = [
            "-logging-sections",
            "environment,-progress",
            "-project-root",
            "/root",
            "-log-directory",
            ".pyre",
            "-workers",
            "5",
            "-expected-binary-version",
            "hash",
            "-search-path",
            "path3",
            "-new-incremental-check",
        ]

        # Check start with watchman.
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(command._flags(), flags)

        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        command = Start(
            arguments,
            original_directory,
            terminal=True,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(set(command._flags()), {*flags, "-terminal"})

        # Check filter directories.
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        with patch.object(command, "_get_directories_to_analyze") as get_directories:
            get_directories.return_value = {"a", "b"}
            self.assertEqual(
                set(command._flags()), {*flags, "-filter-directories", "a;b"}
            )

        # Check configuration-file-hash.
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash", file_hash="ABCD")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        with patch.object(command, "_get_directories_to_analyze") as get_directories:
            get_directories.return_value = {"a", "b"}
            self.assertEqual(
                set(command._flags()),
                {
                    *flags,
                    "-filter-directories",
                    "a;b",
                    "-configuration-file-hash",
                    "ABCD",
                },
            )

        # Check save-initial-state-to.
        arguments = mock_arguments(save_initial_state_to="/tmp")
        configuration = mock_configuration(version_hash="hash")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()), {*flags, "-save-initial-state-to", "/tmp"}
        )

        # Test saved state options.
        arguments = mock_arguments(
            load_initial_state_from="/tmp/pyre_shared_memory",
            changed_files_path="/tmp/changed_files",
        )
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()),
            {
                *flags,
                "-load-state-from",
                "/tmp/pyre_shared_memory",
                "-changed-files-path",
                "/tmp/changed_files",
            },
        )

        arguments = mock_arguments(load_initial_state_from="/tmp/pyre_shared_memory")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()),
            {*flags, "-load-state-from", "/tmp/pyre_shared_memory"},
        )

        arguments = mock_arguments(saved_state_project="pyre/saved_state")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()), {*flags, "-saved-state-project", "pyre/saved_state"}
        )

        arguments = mock_arguments(
            no_saved_state=True,
            load_initial_state_from="/do/not/load",
            save_initial_state_to="/do/not/save",
            changed_files_path="/do/not/change",
        )
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(set(command._flags()), {*flags})

        arguments = mock_arguments(saved_state_project="pyre/saved_state")
        configuration = mock_configuration()
        configuration.relative_local_root = "first/second"
        configuration.version_hash = "hash"
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()),
            {
                *flags,
                "-saved-state-project",
                "pyre/saved_state",
                "-saved-state-metadata",
                "first$second",
            },
        )

        # Store type check resolution.
        arguments = mock_arguments()
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=True,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()), {*flags, "-store-type-check-resolution"}
        )

        # Test autocomplete.
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        configuration.autocomplete = True
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(set(command._flags()), {*flags, "-autocomplete"})

        # Test profiling options.
        arguments = mock_arguments(enable_profiling=True)
        configuration = mock_configuration(version_hash="hash")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()), {*flags, "-profiling-output", ".pyre/profiling.log"}
        )

        arguments = mock_arguments(enable_memory_profiling=True)
        configuration = mock_configuration(version_hash="hash")
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()),
            {*flags, "-memory-profiling-output", ".pyre/profiling.log"},
        )

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_start_flags__ignore_all_errors(
        self,
        get_directories_to_analyze: MagicMock,
        find_global_and_local_root: MagicMock,
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration(version_hash="hash")
        configuration.get_existent_ignore_all_errors_paths.return_value = [
            "/absolute/a",
            "/root/b",
        ]
        flags = [
            "-logging-sections",
            "environment,-progress",
            "-project-root",
            "/root",
            "-log-directory",
            ".pyre",
            "-workers",
            "5",
            "-expected-binary-version",
            "hash",
            "-search-path",
            "path3",
            "-new-incremental-check",
        ]
        command = Start(
            arguments,
            original_directory,
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=True,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
        )
        self.assertEqual(
            set(command._flags()), {*flags, "-ignore-all-errors", "/absolute/a;/root/b"}
        )

    @patch.object(configuration_monitor.ConfigurationMonitor, "daemonize")
    def test_start_configuration_monitor_watchman_enabled(
        self, daemonize: MagicMock
    ) -> None:
        start_command = Start(
            mock_arguments(),
            "/original/directory",
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=True,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory("/root"),
        )
        start_command._start_configuration_monitor()
        daemonize.assert_called_once()

    @patch.object(configuration_monitor.ConfigurationMonitor, "daemonize")
    def test_start_configuration_monitor_watchman_disabled(
        self, daemonize: MagicMock
    ) -> None:
        start_command = Start(
            mock_arguments(),
            "/original/directory",
            terminal=False,
            store_type_check_resolution=False,
            use_watchman=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory("/root"),
        )
        start_command._start_configuration_monitor()
        daemonize.assert_not_called()
