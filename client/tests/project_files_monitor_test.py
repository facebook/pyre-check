# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import unittest
from unittest.mock import MagicMock, patch

from .. import project_files_monitor
from ..project_files_monitor import ProjectFilesMonitor


class ProjectFilesMonitorTest(unittest.TestCase):
    @patch.object(sys, "exit")
    @patch.object(project_files_monitor, "find_root")
    def test_subscriptions(self, find_root, sys_exit):
        find_root.return_value = "/ROOT"

        arguments = MagicMock()
        configuration = MagicMock()
        analysis_directory = MagicMock()
        analysis_directory.get_root.return_value = "/ROOT"

        # no additional extensions
        configuration.extensions = []
        monitor = ProjectFilesMonitor(arguments, configuration, analysis_directory)
        self.assertEqual(len(monitor._subscriptions), 1)
        subscription = monitor._subscriptions[0]
        self.assertEqual(subscription.root, "/ROOT")
        self.assertEqual(subscription.name, "pyre_file_change_subscription")
        self.assertEqual(subscription.subscription["fields"], ["name"])
        self.assertEqual(
            subscription.subscription["expression"][0:3],
            ["allof", ["type", "f"], ["not", "empty"]],
        )
        self.assertCountEqual(
            subscription.subscription["expression"][3],
            ["anyof", ["suffix", "py"], ["suffix", "pyi"]],
        )

        # additional extensions
        configuration.extensions = ["thrift", "whl"]
        monitor = ProjectFilesMonitor(arguments, configuration, analysis_directory)
        self.assertEqual(len(monitor._subscriptions), 1)
        subscription = monitor._subscriptions[0]
        self.assertEqual(subscription.root, "/ROOT")
        self.assertEqual(subscription.name, "pyre_file_change_subscription")
        self.assertEqual(subscription.subscription["fields"], ["name"])
        self.assertEqual(
            subscription.subscription["expression"][0:3],
            ["allof", ["type", "f"], ["not", "empty"]],
        )
        self.assertCountEqual(
            subscription.subscription["expression"][3],
            [
                "anyof",
                ["suffix", "py"],
                ["suffix", "pyi"],
                ["suffix", "thrift"],
                ["suffix", "whl"],
            ],
        )

        # no buck root -> terminate
        find_root.return_value = None
        monitor = ProjectFilesMonitor(arguments, configuration, analysis_directory)
        monitor._subscriptions
        sys_exit.assert_called_once_with(0)
