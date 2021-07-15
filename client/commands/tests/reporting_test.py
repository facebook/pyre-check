# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import copy
import json
import os
import subprocess
import unittest
from pathlib import Path
from typing import Any, Dict
from unittest.mock import MagicMock, mock_open, patch

from ... import commands, find_directories, configuration as configuration_module
from ...analysis_directory import AnalysisDirectory, SharedAnalysisDirectory
from ..command import ClientException
from .command_test import mock_arguments, mock_configuration


class ReportingTest(unittest.TestCase):
    @patch.object(os.path, "realpath", side_effect=lambda path: path)
    @patch.object(os.path, "isdir", side_effect=lambda path: True)
    @patch.object(os.path, "exists", side_effect=lambda path: True)
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path("/root")),
    )
    def test_get_errors(
        self, find_global_and_local_root, exists, isdir, realpath
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
                    "stop_line": 3,
                    "stop_column": 4,
                    "path": "test/path.py",
                    "code": 3,
                    "name": "Error",
                    "description": "description",
                }
            ]
        }

        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("/root/f/g")
            ),
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)

        arguments = mock_arguments(targets=["//f/g:target"])
        configuration.targets = []
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("/root/f/g")
            ),
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)

        original_directory = "/f/g/target"
        arguments = mock_arguments(targets=["//f/g:target"])
        configuration.targets = []
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("/root/h/i")
            ),
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)

        # Called from root with local configuration command line argument
        original_directory = "/root"  # called from
        find_global_and_local_root.return_value = find_directories.FoundRoot(
            Path("/root"), Path("/root/test")
        )  # project root
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(configuration_module.SimpleSearchPathElement("/root")),
        )
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)

        # Test overlapping analysis directory and error path
        original_directory = "/root"  # called from
        find_global_and_local_root.return_value = find_directories.FoundRoot(
            Path("/root"), Path("/root/test")
        )  # project root
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("/root/test")
            ),
        )
        json_errors = copy.deepcopy(json_errors)
        json_errors["errors"][0]["path"] = "/root/test/path.py"
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertFalse(error.ignore_error)
            self.assertEqual(error.error.path, "test/path.py")

        return

        # Test wildcard in do not check
        original_directory = "/"  # called from
        find_global_and_local_root.return_value = find_directories.FoundRoot(Path("/"))
        configuration.ignore_all_errors = ["*/b"]
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(configuration_module.SimpleSearchPathElement("/a")),
        )
        json_errors["errors"][0]["path"] = "b/c.py"
        with patch.object(json, "loads", return_value=copy.deepcopy(json_errors)):
            errors = handler._get_errors(result)
            self.assertEqual(len(errors), 1)
            [error] = errors
            self.assertTrue(error.ignore_error)

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
    @patch("{}.find_global_and_local_root".format(find_directories.__name__))
    def test_get_directories_to_analyze(self, find_global_and_local_root, run) -> None:
        original_directory = "/"
        find_global_and_local_root.return_value = find_directories.FoundRoot(
            Path("base")
        )
        arguments = mock_arguments(source_directories=["base"])
        configuration = mock_configuration()
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(configuration_module.SimpleSearchPathElement("base")),
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
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(configuration_module.SimpleSearchPathElement("base")),
        )
        self.assertEqual(handler._get_directories_to_analyze(), {"base"})

        configuration.local_configuration = "a/b/.pyre_configuration.local"
        arguments = mock_arguments(source_directories=[])
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("base"),
                filter_paths={"a/b"},
            ),
        )
        self.assertEqual(handler._get_directories_to_analyze(), {"a/b"})

        # With no local configuration, no filter paths, and a shared analysis
        # directory, fall back on the pyre root (current directory).
        configuration.local_configuration = None
        handler = commands.Reporting(
            arguments,
            original_directory,
            configuration,
            SharedAnalysisDirectory(
                [],
                ["//target/name"],
                project_root="source_directory",
                filter_paths=set(),
            ),
        )
        with patch.object(os, "getcwd", return_value="source_directory"):
            self.assertEqual(
                handler._get_directories_to_analyze(), {"source_directory"}
            )
