# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os  # noqa
import sys  # noqa
import unittest
from contextlib import contextmanager
from unittest.mock import call, patch

from .. import configuration_monitor, watchman_subscriber
from ..commands import stop
from ..commands.tests.command_test import mock_arguments, mock_configuration
from ..filesystem import AnalysisDirectory


class MonitorTest(unittest.TestCase):
    @patch("os.fork")
    @patch("os.close")
    @patch("sys.exit")
    @patch.object(configuration_monitor.ConfigurationMonitor, "_run")
    def test_daemonize(self, run, _exit, _close, fork) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory("/tmp")
        # Ensure that run() only gets called from the child.
        fork.return_value = 1
        configuration_monitor.ConfigurationMonitor(
            arguments, configuration, analysis_directory
        ).daemonize()
        run.assert_not_called()
        fork.assert_has_calls([call()])

        fork.return_value = 0
        configuration_monitor.ConfigurationMonitor(
            arguments, configuration, analysis_directory
        ).daemonize()
        fork.assert_has_calls([call(), call()])
        run.assert_has_calls([call()])
        _exit.assert_has_calls([call(0)])

        run.side_effect = OSError
        configuration_monitor.ConfigurationMonitor(
            arguments, configuration, analysis_directory
        ).daemonize()
        _exit.assert_has_calls([call(0), call(1)])

    @patch("os.makedirs")
    @patch.object(watchman_subscriber, "acquire_lock")
    def test_run(self, _lock, _makedirs) -> None:
        @contextmanager
        def yield_once(path, blocking):
            yield

        _lock.side_effect = yield_once
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory("/tmp")
        try:
            import pywatchman  # noqa

            with patch.object(pywatchman, "client") as pywatchman_client:
                pywatchman_client.side_effect = Exception
                with self.assertRaises(Exception):
                    with patch("builtins.open"):
                        configuration_monitor.ConfigurationMonitor(
                            arguments, configuration, analysis_directory
                        )._run()
        except ImportError:
            pass

    def test_handle_response(self) -> None:
        arguments = mock_arguments()
        arguments.local_configuration = "/ROOT/a/b/c"
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory("/tmp")
        monitor_instance = configuration_monitor.ConfigurationMonitor(
            arguments, configuration, analysis_directory
        )

        with patch.object(stop, "Stop") as stop_command:
            monitor_instance._handle_response(
                {"files": ["a/b/c/.pyre_configuration.local"], "root": "/ROOT"}
            )
            stop_command.assert_called_once_with(
                arguments, configuration, analysis_directory
            )

        with patch.object(stop, "Stop") as stop_command:
            monitor_instance._handle_response(
                {
                    "files": [
                        "a/b/.pyre_configuration.local",
                        "c/d/.pyre_configuration.local",
                        "c/a/b/c/.pyre_configuration.local",
                        "a/.pyre_configuration.local",
                    ],
                    "root": "/ROOT",
                }
            )
            stop_command.assert_not_called()

        with patch.object(stop, "Stop") as stop_command:
            monitor_instance._handle_response(
                {"files": [".pyre_configuration"], "root": "/ROOT"}
            )
            stop_command.assert_called_once_with(
                arguments, configuration, analysis_directory
            )

        # Changes to any other .pyre_configuration files within root directory
        # should not trigger server stop.
        with patch.object(stop, "Stop") as stop_command:
            monitor_instance._handle_response(
                {"files": ["a/b/.pyre_configuration"], "root": "/ROOT"}
            )
            stop_command.assert_not_called()
