# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import textwrap
import unittest
from pathlib import Path
from typing import Dict
from unittest.mock import MagicMock, patch

from libcst import Module, parse_module

from ...analysis_directory import AnalysisDirectory
from ...statistics_collectors import (
    AnnotationCountCollector,
    FixmeCountCollector,
    IgnoreCountCollector,
    StrictCountCollector,
)
from ..statistics import Statistics, _find_paths, parse_path_to_module
from .command_test import mock_arguments, mock_configuration


class StatisticsTest(unittest.TestCase):
    def test_find_targets(self) -> None:
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

    # pyre-fixme[56]: Argument `[]` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(Statistics, "_find_paths", return_value=[])
    @patch.object(Statistics, "_log_to_scuba")
    def test_log_results(self, log: MagicMock, _find_paths: MagicMock) -> None:
        arguments = mock_arguments(local_configuration="example/path/client")
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")
        original_directory = "/original/directory"

        Statistics(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            filter_paths=["a.py", "b.py"],
            log_results=False,
        )._run()
        log.assert_not_called()

        Statistics(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            filter_paths=["a.py", "b.py"],
            log_results=True,
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


class AnnotationCountCollectorTest(unittest.TestCase):
    @staticmethod
    def format_files(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_counts(self, source: str, expected: Dict[str, int]) -> None:
        source_module = self.format_files(source)
        collector = AnnotationCountCollector()
        source_module.visit(collector)
        self.assertEqual(collector.build_json(), expected)

    def test_annotate_functions(self) -> None:
        self.assert_counts(
            """
            def foo(x) -> int:
                pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            def bar(x: int, y):
                pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            a = foo()
            b: int = bar()
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 1,
                "annotated_parameter_count": 0,
                "return_count": 0,
                "globals_count": 2,
                "parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            class A:
                a: int = 100
                b = ""
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 0,
                "globals_count": 0,
                "parameter_count": 0,
                "attribute_count": 2,
                "annotated_attribute_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
            },
        )

        # For now, don't count annotations inside of functions
        self.assert_counts(
            """
            def foo():
                a: int = 100
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            def foo():
                def bar(x: int) -> int:
                    pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 2,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
            },
        )

        self.assert_counts(
            """
            class A:
                def bar(self, x: int):
                    pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 2,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            class A:
                def bar(this, x: int) -> None:
                    pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 2,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
            },
        )

        self.assert_counts(
            """
            class A:
                @classmethod
                def bar(cls, x: int):
                    pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 2,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            def bar(self, x: int):
                pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            class A:
                @staticmethod
                def bar(self, x: int) -> None:
                    pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )

        self.assert_counts(
            """
            def foo(x):
                def bar(x):
                    return x
                return bar

            class A:
                @foo(42)
                def baz(self): ...
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 3,
                "globals_count": 0,
                "parameter_count": 3,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
            },
        )


class FixmeCountCollectorTest(unittest.TestCase):
    @staticmethod
    def format_files(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_counts(self, source: str, expected: Dict[str, int]) -> None:
        source_module = self.format_files(source)
        collector = FixmeCountCollector()
        source_module.visit(collector)
        self.assertEqual(collector.build_json(), expected)

    def test_annotate_functions(self) -> None:
        self.assert_counts("# pyre-fixme[2]: Example Error Message", {"2": 1})
        self.assert_counts(
            "# pyre-fixme[3]: Example Error Message \n\n\n # pyre-fixme[34]: Example",
            {"3": 1, "34": 1},
        )
        self.assert_counts(
            "# pyre-fixme[2]: Example Error Message\n\n\n# pyre-fixme[2]: message",
            {"2": 2},
        )


class IgnoreCountCollectorTest(unittest.TestCase):
    @staticmethod
    def format_files(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_counts(self, source: str, expected: Dict[str, int]) -> None:
        source_module = self.format_files(source)
        collector = IgnoreCountCollector()
        source_module.visit(collector)
        self.assertEqual(collector.build_json(), expected)

    def test_annotate_functions(self) -> None:
        self.assert_counts("# pyre-ignore[2]: Example Error Message", {"2": 1})
        self.assert_counts(
            "# pyre-ignore[3]: Example Error Message \n\n\n # pyre-ignore[34]: Example",
            {"3": 1, "34": 1},
        )
        self.assert_counts(
            "# pyre-ignore[2]: Example Error Message\n\n\n# pyre-ignore[2]: message",
            {"2": 2},
        )


class StrictCountCollectorTest(unittest.TestCase):
    @staticmethod
    def format_files(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_counts(
        self, source: str, expected: Dict[str, int], default_strict: bool
    ) -> None:
        source_module = self.format_files(source)
        collector = StrictCountCollector(default_strict)
        source_module.visit(collector)
        self.assertEqual(collector.build_json(), expected)

    def test_strict_files(self) -> None:
        self.assert_counts(
            """
            # pyre-unsafe

            def foo():
                return 1
            """,
            {"strict_count": 0, "unsafe_count": 1},
            True,
        )
        self.assert_counts(
            """
            # pyre-strict
            def foo():
                return 1
            """,
            {"strict_count": 1, "unsafe_count": 0},
            False,
        )
        self.assert_counts(
            """
            def foo():
                return 1
            """,
            {"strict_count": 0, "unsafe_count": 1},
            False,
        )
        self.assert_counts(
            """
            def foo():
                return 1
            """,
            {"strict_count": 1, "unsafe_count": 0},
            True,
        )
