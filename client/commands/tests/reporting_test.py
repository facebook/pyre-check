# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import copy
import json
import os
import subprocess
import unittest
from typing import Any, Dict
from unittest.mock import MagicMock, mock_open, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory, SharedAnalysisDirectory
from ..command import ClientException, __name__ as client_name
from .command_test import mock_arguments, mock_configuration


class ReportingTest(unittest.TestCase):
    @patch.object(os.path, "realpath", side_effect=lambda path: path)
    @patch.object(os.path, "isdir", side_effect=lambda path: True)
    @patch.object(os.path, "exists", side_effect=lambda path: True)
    @patch("{}.find_project_root".format(client_name), return_value="/")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    @patch("os.chdir")
    def test_get_errors(
        self, chdir, find_local_root, find_project_root, exists, isdir, realpath
    ) -> None:
        original_directory = "/test"
        arguments = mock_arguments()
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
            arguments, original_directory, configuration, AnalysisDirectory("/test/f/g")
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        arguments = mock_arguments(targets=["//f/g:target"])
        configuration.targets = []
        handler = commands.Reporting(
            arguments, original_directory, configuration, AnalysisDirectory("/test/f/g")
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        original_directory = "/f/g/target"
        arguments = mock_arguments(targets=["//f/g:target"])
        configuration.targets = []
        handler = commands.Reporting(
            arguments, original_directory, configuration, AnalysisDirectory("/test/h/i")
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        # Called from root with local configuration command line argument
        original_directory = "/project"  # called from
        find_project_root.return_value = "/project"  # project root
        find_local_root.return_value = "/project/test"  # local configuration
        handler = commands.Reporting(
            arguments, original_directory, configuration, AnalysisDirectory("/project")
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

        # Test overlapping analysis directory and error path
        original_directory = "/project"  # called from
        find_project_root.return_value = "/project"  # project root
        find_local_root.return_value = "/project/test"  # local configuration
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory("/project/test"),
        )
        json_errors = copy.deepcopy(json_errors)
        json_errors["errors"][0]["path"] = "/project/test/path.py"
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertFalse(error.external_to_global_root)
            self.assertEqual(error.path, "test/path.py")

        return

        # Test wildcard in do not check
        original_directory = "/"  # called from
        find_project_root.return_value = "/"  # project root
        find_local_root.return_value = None
        configuration.ignore_all_errors = ["*/b"]
        handler = commands.Reporting(
            arguments, original_directory, configuration, AnalysisDirectory("/a")
        )
        json_errors["errors"][0]["path"] = "b/c.py"
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertTrue(error.ignore_error)
            self.assertFalse(error.external_to_global_root)

    @patch.object(json, "loads")
    def test_load_errors_from_json(self, loads: MagicMock) -> None:
        error_list = [{"one": 1}, {"two": 2}]
        loads.return_value = {"errors": error_list}
        actual = commands.Reporting._load_errors_from_json("<some json string>")
        self.assertEqual(actual, error_list)

    @patch.object(json, "loads")
    def test_load_errors_from_json_unexpected_format(self, loads: MagicMock) -> None:
        error_list = [{"one": 1}, {"two": 2}]
        loads.return_value = {"response": {"errors": error_list}}
        actual = commands.Reporting._load_errors_from_json("<some json string>")
        self.assertEqual(actual, [])

    @patch.object(json, "loads")
    def test_load_errors_from_json_list(self, loads: MagicMock) -> None:
        error_list = [{"one": 1}, {"two": 2}]
        loads.return_value = error_list
        # It expects a dictionary, not a list.
        actual = commands.Reporting._load_errors_from_json("<some json string>")
        self.assertEqual(actual, [])

    @patch.object(json, "loads")
    def test_load_errors_from_json_decode_error(self, loads: MagicMock) -> None:
        def raise_error(input: str) -> Dict[str, Any]:
            raise json.JSONDecodeError("foo", "foo", 0)

        loads.side_effect = raise_error
        with self.assertRaises(ClientException):
            commands.Reporting._load_errors_from_json("<some json string>")

    @patch.object(subprocess, "run")
    @patch("os.chdir")
    @patch("{}.find_project_root".format(client_name), return_value="/")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    def test_get_directories_to_analyze(
        self, find_local_root, find_project_root, chdir, run
    ) -> None:
        original_directory = "/"
        find_project_root.return_value = "base"
        arguments = mock_arguments(source_directories=["base"])
        configuration = mock_configuration()
        handler = commands.Reporting(
            arguments, original_directory, configuration, AnalysisDirectory("base")
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
        with patch("builtins.open", mock_open(read_data="{}")):
            self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        configuration.local_configuration = "a/b/.pyre_configuration.local"
        handler = commands.Reporting(
            arguments, original_directory, configuration, AnalysisDirectory("base")
        )
        self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        configuration.local_configuration = "a/b/.pyre_configuration.local"
        arguments = mock_arguments(source_directories=[])
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory("base", filter_paths={"a/b"}),
        )
        self.assertEqual(handler._get_directories_to_analyze(), {"a/b"})

        # With no local configuration, no filter paths, and a shared analysis
        # directory, fall back on the pyre root (current directory).
        configuration.local_configuration = None
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            SharedAnalysisDirectory([], ["//target/name"], filter_paths=set()),
        )
        with patch.object(os, "getcwd", return_value="source_directory"):
            self.assertEqual(
                handler._get_directories_to_analyze(), {"source_directory"}
            )
