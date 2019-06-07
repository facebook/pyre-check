# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import builtins  # noqa
import json
import os
import subprocess
import unittest
from unittest.mock import MagicMock, call, mock_open, patch

from ... import commands  # noqa
from ...error import Error  # noqa
from ...filesystem import AnalysisDirectory, SharedAnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class ReportingTest(unittest.TestCase):
    @patch.object(os.path, "realpath", side_effect=lambda path: path)
    @patch.object(os.path, "isdir", side_effect=lambda path: True)
    def test_get_errors(self, isdir, realpath) -> None:
        arguments = mock_arguments()
        arguments.original_directory = "/test"  # called from
        arguments.current_directory = "/"  # project root
        arguments.local_configuration = None
        configuration = mock_configuration()
        result = MagicMock()

        json_errors = {
            "errors": [
                {
                    "line": 1,
                    "column": 2,
                    "path": "test/path.py",
                    "code": 3,
                    "name": "Error",
                    "description": "description",
                    "inference": "inference",
                }
            ]
        }

        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("/test/f/g")
        )
        with patch.object(json, "loads", return_value=json_errors):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        arguments.targets = ["//f/g:target"]
        configuration.targets = []
        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("/test/f/g")
        )
        with patch.object(json, "loads", return_value=json_errors):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        arguments.original_directory = "/f/g/target"
        arguments.targets = ["//f/g:target"]
        configuration.targets = []
        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("/test/h/i")
        )
        with patch.object(json, "loads", return_value=json_errors):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        # Called from root with local configuration command line argument
        arguments.original_directory = "/"  # called from
        arguments.current_directory = "/"  # project root
        arguments.local_configuration = "/test"
        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("/shared")
        )
        with patch.object(json, "loads", return_value=json_errors):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        return

        # Test wildcard in do not check
        arguments.original_directory = "/"  # called from
        arguments.current_directory = "/"  # project root
        arguments.local_configuration = None
        configuration.ignore_all_errors = ["*/b"]
        handler = commands.Reporting(arguments, configuration, AnalysisDirectory("/a"))
        json_errors["errors"][0]["path"] = "b/c.py"
        with patch.object(json, "loads", return_value=json_errors):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertTrue(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

    @patch.object(subprocess, "run")
    def test_get_directories_to_analyze(self, run) -> None:
        arguments = mock_arguments()
        arguments.current_directory = "base"
        arguments.source_directories = ["base"]
        configuration = mock_configuration()
        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("base")
        )
        run.return_value = subprocess.CompletedProcess(
            args=[],
            returncode=0,
            stdout="\n".join(
                [
                    "external/a/.pyre_configuration.local",
                    "external/b/c/.pyre_configuration.local",
                ]
            ).encode("utf-8"),
        )
        with patch("builtins.open", mock_open(read_data='{"push_blocking": false}')):
            self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        with patch("builtins.open", mock_open(read_data='{"push_blocking": true}')):
            self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        with patch("builtins.open", mock_open(read_data='{"continuous": true}')):
            self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        configuration.local_configuration = "a/b/.pyre_configuration.local"
        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("base")
        )
        self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        configuration.local_configuration = "a/b/.pyre_configuration.local"
        arguments.source_directories = None
        handler = commands.Reporting(
            arguments, configuration, AnalysisDirectory("base", filter_paths=["a/b"])
        )
        self.assertEqual(handler._get_directories_to_analyze(), {"a/b"})

        # With no local configuration, no filter paths, and a shared analysis
        # directory, fall back on the pyre root (current directory).
        configuration.local_configuration = None
        handler = commands.Reporting(
            arguments,
            configuration,
            SharedAnalysisDirectory([], ["//target/name"], filter_paths=[]),
        )
        with patch.object(os, "getcwd", return_value="source_directory"):
            self.assertEqual(
                handler._get_directories_to_analyze(), {"source_directory"}
            )
