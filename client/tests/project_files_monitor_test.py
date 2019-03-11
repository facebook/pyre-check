# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os.path
import sys
import unittest
from unittest.mock import MagicMock, patch

from .. import project_files_monitor
from ..project_files_monitor import ProjectFilesMonitor, ProjectFilesMonitorException


class ProjectFilesMonitorTest(unittest.TestCase):
    @patch.object(project_files_monitor, "find_root")
    def test_subscriptions(self, find_root):
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

        # no watchman root -> terminate
        find_root.return_value = None
        self.assertRaises(
            ProjectFilesMonitorException,
            ProjectFilesMonitor,
            arguments,
            configuration,
            analysis_directory,
        )

    @patch.object(project_files_monitor, "find_paths_with_extensions")
    @patch.object(
        os.path,
        "realpath",
        side_effect=lambda path: path.replace("ANALYSIS_ROOT", "LOCAL_ROOT"),
    )
    def test_calculate_symbolic_links(self, realpath, find_paths_with_extensions):
        find_paths_with_extensions.return_value = [
            "ANALYSIS_ROOT/a.py",
            "ANALYSIS_ROOT/b.thrift",
            "ANALYSIS_ROOT/subX/d.pyi",
            "ANALYSIS_ROOT/subX/e.py",
            "ANALYSIS_ROOT/subY/subZ/g.pyi",
        ]

        self.assertDictEqual(
            ProjectFilesMonitor._calculate_symbolic_links(
                "ANALYSIS_ROOT", ["py", "pyi", "thrift"]
            ),
            {
                "LOCAL_ROOT/a.py": "ANALYSIS_ROOT/a.py",
                "LOCAL_ROOT/b.thrift": "ANALYSIS_ROOT/b.thrift",
                "LOCAL_ROOT/subX/d.pyi": "ANALYSIS_ROOT/subX/d.pyi",
                "LOCAL_ROOT/subX/e.py": "ANALYSIS_ROOT/subX/e.py",
                "LOCAL_ROOT/subY/subZ/g.pyi": "ANALYSIS_ROOT/subY/subZ/g.pyi",
            },
        )
