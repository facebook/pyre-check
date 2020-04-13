# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from contextlib import contextmanager
from pathlib import Path
from typing import Generator, Optional
from unittest.mock import MagicMock, call, patch

from .. import configuration_monitor, watchman
from ..analysis_directory import AnalysisDirectory
from ..commands import stop
from ..tests.mocks import mock_arguments, mock_configuration


class MonitorTest(unittest.TestCase):
    @patch("os.fork")
    @patch("os.close")
    @patch("sys.exit")
    @patch.object(configuration_monitor.ConfigurationMonitor, "_run")
    def test_daemonize(
        self, run: MagicMock, _exit: MagicMock, _close: MagicMock, fork: MagicMock
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory("/tmp")
        project_root = "/"
        local_configuration_root = None
        # Ensure that run() only gets called from the child.
        fork.return_value = 1
        original_directory = "/"
        configuration_monitor.ConfigurationMonitor(
            arguments,
            configuration,
            analysis_directory,
            project_root,
            original_directory,
            local_configuration_root,
        ).daemonize()
        run.assert_not_called()
        fork.assert_has_calls([call()])

        fork.return_value = 0
        configuration_monitor.ConfigurationMonitor(
            arguments,
            configuration,
            analysis_directory,
            project_root,
            original_directory,
            local_configuration_root,
        ).daemonize()
        fork.assert_has_calls([call(), call()])
        run.assert_has_calls([call()])
        _exit.assert_has_calls([call(0)])

        run.side_effect = OSError
        configuration_monitor.ConfigurationMonitor(
            arguments,
            configuration,
            analysis_directory,
            project_root,
            original_directory,
            local_configuration_root,
        ).daemonize()
        _exit.assert_has_calls([call(0), call(1)])

    @patch("os.makedirs")
    @patch.object(watchman, "acquire_lock")
    def test_run(self, _lock: MagicMock, _makedirs: MagicMock) -> None:
        @contextmanager
        def yield_once(
            path: str, blocking: bool
        ) -> Generator[Optional[int], None, None]:
            yield

        _lock.side_effect = yield_once
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory("/tmp")
        project_root = "/"
        original_directory = "/"
        local_configuration_root = None
        try:
            import pywatchman

            with patch.object(pywatchman, "client") as pywatchman_client:
                pywatchman_client.side_effect = Exception
                with self.assertRaises(Exception):
                    with patch("builtins.open"):
                        configuration_monitor.ConfigurationMonitor(
                            arguments,
                            configuration,
                            analysis_directory,
                            project_root,
                            original_directory,
                            local_configuration_root,
                        )._run()
        except ImportError:
            pass

    @patch.object(stop, "Stop")
    def test_handle_response(self, stop_command: MagicMock) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory("/tmp")
        project_root = "/"
        original_directory = "/"
        local_configuration_root = None
        monitor_instance = configuration_monitor.ConfigurationMonitor(
            arguments,
            configuration,
            analysis_directory,
            project_root,
            original_directory,
            local_configuration_root,
        )

        monitor_instance._project_root_path = Path("/ROOT/a/b")
        monitor_instance._handle_response(
            {"files": ["a/b/.pyre_configuration"], "root": "/ROOT"}
        )
        stop_command.assert_called_once()

        # Don't consider a .pyre_configuration that is not at the project root.
        stop_command.reset_mock()
        monitor_instance._project_root_path = Path("/ROOT/a/b")
        monitor_instance._handle_response(
            {"files": ["a/b/c/d/.pyre_configuration"], "root": "/ROOT"}
        )
        stop_command.assert_not_called()

        stop_command.reset_mock()
        monitor_instance._local_configuration_root = "/ROOT/a/b/c"
        monitor_instance._handle_response(
            {"files": ["a/b/c/.pyre_configuration.local"], "root": "/ROOT"}
        )
        stop_command.assert_called_once()

        stop_command.reset_mock()
        monitor_instance._local_configuration_root = "/ROOT/a/b/c/foo"
        monitor_instance._handle_response(
            {"files": ["a/b/c/.pyre_configuration.local"], "root": "/ROOT"}
        )
        stop_command.assert_not_called()

        stop_command.reset_mock()
        monitor_instance._local_configuration_root = "/ROOT/a"
        monitor_instance._handle_response(
            {
                "files": [
                    "a/b/.pyre_configuration.local",
                    "c/d/.pyre_configuration.local",
                    "c/a/b/c/.pyre_configuration.local",
                ],
                "root": "/ROOT",
            }
        )
        stop_command.assert_not_called()

    @patch.object(
        configuration_monitor.ConfigurationMonitor, "__init__", return_value=None
    )
    @patch.object(watchman.Subscriber, "_watchman_client")
    def test_subscriptions(self, watchman_client: MagicMock, init: MagicMock) -> None:
        watchman_client.query.return_value = {"roots": ["/a", "/b"]}
        monitor = configuration_monitor.ConfigurationMonitor(
            MagicMock(), MagicMock(), MagicMock(), MagicMock(), MagicMock(), MagicMock()
        )
        monitor._project_root_path = Path("/b/project_two")
        actual = monitor._subscriptions
        self.assertEqual(len(actual), 1)
        self.assertEqual(actual[0].root, "/b")
        self.assertEqual(actual[0].name, "pyre_monitor_b")

        monitor._project_root_path = Path("/a")
        actual = monitor._subscriptions
        self.assertEqual(len(actual), 1)
        self.assertEqual(actual[0].root, "/a")
        self.assertEqual(actual[0].name, "pyre_monitor_a")

        monitor._project_root_path = Path("/c/")
        actual = monitor._subscriptions
        self.assertEqual(len(actual), 0)
