# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import sys  # noqa
import unittest
from contextlib import contextmanager
from unittest.mock import call, patch

from ..commands import monitor  # noqa
from .infer_test import mock_arguments, mock_configuration


class MonitorTest(unittest.TestCase):
    @patch("os.fork")
    @patch("os.close")
    @patch("sys.exit")
    @patch.object(monitor.Monitor, "_run")
    def test_daemonize(self, run, _exit, _close, fork) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        # Ensure that run() only gets called from the child.
        fork.return_value = 1
        monitor.Monitor(arguments, configuration, "/tmp").daemonize()
        run.assert_not_called()
        fork.assert_has_calls([call()])

        fork.return_value = 0
        monitor.Monitor(arguments, configuration, "/tmp").daemonize()
        fork.assert_has_calls([call(), call()])
        run.assert_has_calls([call()])
        _exit.assert_has_calls([call(0)])

        run.side_effect = OSError
        monitor.Monitor(arguments, configuration, "/tmp").daemonize()
        _exit.assert_has_calls([call(0), call(1)])

    @patch("os.makedirs")
    @patch.object(monitor, "acquire_lock")
    def test_run(self, _lock, _makedirs) -> None:
        @contextmanager
        def yield_once(path, blocking):
            yield

        _lock.side_effect = yield_once
        arguments = mock_arguments()
        configuration = mock_configuration()
        try:
            import pywatchman  # noqa

            with patch.object(pywatchman, "client") as pywatchman_client:
                pywatchman_client.side_effect = Exception
                with self.assertRaises(Exception):
                    with patch("builtins.open"):
                        monitor.Monitor(arguments, configuration, "/tmp")._run()
        except ImportError:
            pass
