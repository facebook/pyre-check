# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import textwrap
import unittest
from pathlib import Path
from typing import Any, Dict
from unittest.mock import MagicMock, patch

from libcst import Module, parse_module

from ... import configuration as configuration_module
from ...analysis_directory import AnalysisDirectory
from ..statistics import Statistics, _find_paths, parse_path_to_module
from .command_test import mock_arguments, mock_configuration


def mock_absolute(self: Path) -> Path:
    return Path("example/path/client/" + self.name)


class StatisticsTest(unittest.TestCase):
    @patch.object(Path, "absolute", mock_absolute)
    def test_find_paths(self) -> None:
        self.assertEqual(
            _find_paths("example/path/client", set()), [Path("example/path/client")]
        )

        self.assertEqual(
            _find_paths("example/path/client/.pyre_configuration.local", set()),
            [Path("example/path/client")],
        )

        # Sorting the lists because the set argument leads to non-deterministic
        # ordering.
        self.assertEqual(
            sorted(
                _find_paths(
                    "example/path/client/.pyre_configuration.local", {"a.py", "b.py"}
                )
            ),
            sorted(
                [Path("example/path/client/a.py"), Path("example/path/client/b.py")]
            ),
        )

    @patch.object(Statistics, "_find_paths", return_value=[])
    @patch.object(Statistics, "_log_to_scuba")
    def test_log_results(self, log: MagicMock, _find_paths: MagicMock) -> None:
        arguments = mock_arguments(local_configuration="example/path/client")
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        original_directory = "/original/directory"

        Statistics(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            filter_paths=["a.py", "b.py"],
            log_results=False,
            aggregate=False,
        )._run()
        log.assert_not_called()

        Statistics(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            filter_paths=["a.py", "b.py"],
            log_results=True,
            aggregate=False,
        )._run()
        log.assert_called()

    def test_parse_module(self) -> None:
        invalid_python = """
            def foo -> int:
                pass
        """
        valid_python = """
        def foo() -> int:
            pass
        """
        path = MagicMock()
        path.read_text = MagicMock(
            return_value=textwrap.dedent(invalid_python.rstrip())
        )
        self.assertEqual(parse_path_to_module(path), None)

        path.read_text = MagicMock(return_value=textwrap.dedent(valid_python.rstrip()))
        self.assertIsNotNone(parse_path_to_module(path))

    @patch.object(Path, "read_text", side_effect=FileNotFoundError)
    def test_parse_module__file_not_found(self, read_text: MagicMock) -> None:
        self.assertIsNone(parse_path_to_module(Path("foo.txt")))


class AggregateStatisticsTest(unittest.TestCase):
    @staticmethod
    def format_files(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_aggregate_counts(
        self, statistics: Statistics, sources: Dict[str, str], expected: Dict[str, Any]
    ) -> None:
        source_modules = {}
        for path, source in sources.items():
            source_modules[path] = self.format_files(source)
        raw_data = statistics._collect_statistics(source_modules)
        self.assertEqual(statistics._aggregate_data(raw_data), expected)

    def test_aggregate_counts(self) -> None:
        arguments = mock_arguments(local_configuration="example/path/client")
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        original_directory = "/original/directory"
        statistics = Statistics(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            filter_paths=["a.py", "b.py"],
            log_results=False,
            aggregate=True,
        )

        self.assert_aggregate_counts(
            statistics,
            {
                "a.py": """
            # pyre-unsafe

            def foo():
                return 1
            """
            },
            {
                "annotations": {
                    "return_count": 1,
                    "annotated_return_count": 0,
                    "globals_count": 0,
                    "annotated_globals_count": 0,
                    "parameter_count": 0,
                    "annotated_parameter_count": 0,
                    "attribute_count": 0,
                    "annotated_attribute_count": 0,
                    "partially_annotated_function_count": 0,
                    "fully_annotated_function_count": 0,
                    "line_count": 6,
                },
                "fixmes": 0,
                "ignores": 0,
                "strict": 0,
                "unsafe": 1,
            },
        )

        self.assert_aggregate_counts(
            statistics,
            {
                "a.py": """
            # pyre-unsafe

            def foo():
                return 1
            """,
                "b.py": """
            # pyre-strict

            def foo(x: int) -> int:
                return 1
            """,
            },
            {
                "annotations": {
                    "return_count": 2,
                    "annotated_return_count": 1,
                    "globals_count": 0,
                    "annotated_globals_count": 0,
                    "parameter_count": 1,
                    "annotated_parameter_count": 1,
                    "attribute_count": 0,
                    "annotated_attribute_count": 0,
                    "partially_annotated_function_count": 0,
                    "fully_annotated_function_count": 1,
                    "line_count": 12,
                },
                "fixmes": 0,
                "ignores": 0,
                "strict": 1,
                "unsafe": 1,
            },
        )
