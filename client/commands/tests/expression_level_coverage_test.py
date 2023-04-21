# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path
from typing import Iterable, List

import testslide

from ... import command_arguments, configuration, frontend_configuration

from ...language_server import connections, protocol as lsp
from ...tests import setup
from .. import commands, daemon_query, expression_level_coverage


class ExpressionLevelTest(testslide.TestCase):
    def test_make_expression_level_coverage_response(self) -> None:
        self.assertEqual(
            expression_level_coverage._make_expression_level_coverage_response(
                daemon_query.Response(
                    {
                        "response": [
                            [
                                "CoverageAtPath",
                                {
                                    "path": "test.py",
                                    "total_expressions": 7,
                                    "coverage_gaps": [
                                        {
                                            "location": {
                                                "start": {"line": 11, "column": 16},
                                                "stop": {"line": 11, "column": 17},
                                            },
                                            "function_name": None,
                                            "type_": "typing.Any",
                                            "reason": ["TypeIsAny message."],
                                        },
                                    ],
                                },
                            ],
                        ]
                    }
                ).payload,
            ),
            expression_level_coverage.ExpressionLevelCoverageResponse(
                response=[
                    expression_level_coverage.CoverageAtPathResponse(
                        CoverageAtPath=expression_level_coverage.CoverageAtPath(
                            path="test.py",
                            total_expressions=7,
                            coverage_gaps=[
                                expression_level_coverage.CoverageGap(
                                    location=expression_level_coverage.Location(
                                        start=expression_level_coverage.Pair(
                                            line=11, column=16
                                        ),
                                        stop=expression_level_coverage.Pair(
                                            line=11, column=17
                                        ),
                                    ),
                                    function_name=None,
                                    type_="typing.Any",
                                    reason=["TypeIsAny message."],
                                )
                            ],
                        )
                    )
                ],
            ),
        )
        self.assertEqual(
            expression_level_coverage._make_expression_level_coverage_response(
                daemon_query.Response(
                    {
                        "response": [
                            [
                                "ErrorAtPath",
                                {
                                    "path": "test.py",
                                    "error": "Not able to get lookups in: `test.py` (file not found)",
                                },
                            ],
                        ]
                    }
                ).payload,
            ),
            expression_level_coverage.ExpressionLevelCoverageResponse(
                response=[
                    expression_level_coverage.ErrorAtPathResponse(
                        expression_level_coverage.ErrorAtPath(
                            path="test.py",
                            error="Not able to get lookups in: `test.py` (file not found)",
                        )
                    )
                ],
            ),
        )
        with self.assertRaises(expression_level_coverage.ErrorParsingFailure):
            expression_level_coverage._make_expression_level_coverage_response(
                "garbage input"
            )

    def test_calculate_percent_covered(self) -> None:
        self.assertEqual(
            expression_level_coverage._calculate_percent_covered(0, 0), 100.0
        )
        self.assertEqual(
            expression_level_coverage._calculate_percent_covered(3, 7), 57.14
        )

    def test_get_total_and_uncovered_expressions(self) -> None:
        coverage = expression_level_coverage.CoverageAtPath(
            path="test.py",
            total_expressions=7,
            coverage_gaps=[
                expression_level_coverage.CoverageGap(
                    location=expression_level_coverage.Location(
                        start=expression_level_coverage.Pair(line=11, column=16),
                        stop=expression_level_coverage.Pair(line=11, column=17),
                    ),
                    function_name=None,
                    type_="typing.Any",
                    reason=["TypeIsAny message."],
                )
            ],
        )
        self.assertEqual(
            expression_level_coverage._get_total_and_uncovered_expressions(coverage),
            (7, 1),
        )

    def test_get_percent_covered_per_path(self) -> None:
        def assert_get_percent_covered_per_path(
            response: expression_level_coverage.CoverageAtPathResponse, expected: float
        ) -> None:
            self.assertEqual(
                expression_level_coverage.get_percent_covered_per_path(response),
                expected,
            )

        assert_get_percent_covered_per_path(
            expression_level_coverage.CoverageAtPathResponse(
                CoverageAtPath=expression_level_coverage.CoverageAtPath(
                    path="test.py",
                    total_expressions=0,
                    coverage_gaps=[],
                )
            ),
            100.0,
        )

        assert_get_percent_covered_per_path(
            expression_level_coverage.CoverageAtPathResponse(
                CoverageAtPath=expression_level_coverage.CoverageAtPath(
                    path="test.py",
                    total_expressions=5,
                    coverage_gaps=[
                        expression_level_coverage.CoverageGap(
                            location=expression_level_coverage.Location(
                                start=expression_level_coverage.Pair(
                                    line=11, column=16
                                ),
                                stop=expression_level_coverage.Pair(line=11, column=17),
                            ),
                            function_name=None,
                            type_="typing.Any",
                            reason=["TypeIsAny message."],
                        )
                    ],
                )
            ),
            80.0,
        )

    def test_summary_expression_level_coverage(self) -> None:
        def assert_summary_expression_level_coverage(
            response: object, expected: str
        ) -> None:
            self.assertEqual(
                expression_level_coverage.summary_expression_level(response),
                expected,
            )

        assert_summary_expression_level_coverage(
            daemon_query.Response({"response": []}).payload,
            "Overall: 100.0% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            daemon_query.Response(
                {
                    "response": [
                        [
                            "CoverageAtPath",
                            {
                                "path": "test.py",
                                "total_expressions": 0,
                                "coverage_gaps": [],
                            },
                        ],
                    ]
                }
            ).payload,
            "test.py: 100.0% expressions are covered\nOverall: 100.0% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            daemon_query.Response(
                {
                    "response": [
                        [
                            "CoverageAtPath",
                            {
                                "path": "test.py",
                                "total_expressions": 7,
                                "coverage_gaps": [
                                    {
                                        "location": {
                                            "start": {"line": 11, "column": 16},
                                            "stop": {"line": 11, "column": 17},
                                        },
                                        "function_name": None,
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny message."],
                                    },
                                    {
                                        "location": {
                                            "start": {"line": 12, "column": 11},
                                            "stop": {"line": 12, "column": 12},
                                        },
                                        "function_name": None,
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny message."],
                                    },
                                ],
                            },
                        ],
                    ]
                }
            ).payload,
            "test.py: 71.43% expressions are covered\nOverall: 71.43% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            daemon_query.Response(
                {
                    "response": [
                        [
                            "CoverageAtPath",
                            {
                                "path": "library.py",
                                "total_expressions": 4,
                                "coverage_gaps": [],
                            },
                        ],
                        [
                            "CoverageAtPath",
                            {
                                "path": "test.py",
                                "total_expressions": 7,
                                "coverage_gaps": [
                                    {
                                        "location": {
                                            "start": {"line": 11, "column": 16},
                                            "stop": {"line": 11, "column": 17},
                                        },
                                        "function_name": None,
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny message."],
                                    },
                                    {
                                        "location": {
                                            "start": {"line": 12, "column": 11},
                                            "stop": {"line": 12, "column": 12},
                                        },
                                        "function_name": None,
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny message."],
                                    },
                                ],
                            },
                        ],
                    ]
                }
            ).payload,
            "library.py: 100.0% expressions are covered\ntest.py: 71.43% expressions are covered\nOverall: 81.82% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            daemon_query.Response(
                {
                    "response": [
                        [
                            "ErrorAtPath",
                            {
                                "path": "fake.py",
                                "error": "Not able to get lookups in: `fake.py` (file not found)",
                            },
                        ],
                        [
                            "CoverageAtPath",
                            {
                                "path": "test.py",
                                "total_expressions": 7,
                                "coverage_gaps": [
                                    {
                                        "location": {
                                            "start": {"line": 11, "column": 16},
                                            "stop": {"line": 11, "column": 17},
                                        },
                                        "function_name": None,
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny message."],
                                    },
                                    {
                                        "location": {
                                            "start": {"line": 12, "column": 11},
                                            "stop": {"line": 12, "column": 12},
                                        },
                                        "function_name": None,
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny message."],
                                    },
                                ],
                            },
                        ],
                    ]
                }
            ).payload,
            "test.py: 71.43% expressions are covered\nOverall: 71.43% expressions are covered",
        )

    def test_CoveragePaths(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            global_root = Path(root).resolve()
            setup.ensure_directories_exists(
                global_root,
                [".pyre", "allows", "blocks", "search", "local/src/subpackage"],
            )
            setup.write_configuration_file(
                global_root,
                {
                    "only_check_paths": ["allows", "nonexistent"],
                    "ignore_all_errors": ["blocks", "nonexistent"],
                    "exclude": ["exclude"],
                    "extensions": [".ext", "invalid_extension"],
                    "workers": 42,
                    "search_path": ["search", "nonexistent"],
                    "strict": True,
                },
            )
            setup.write_configuration_file(
                global_root, {"source_directories": ["src"]}, relative="local"
            )
            local_root = global_root / "local"
            Path(local_root / "src/a.py").touch()
            Path(local_root / "src/b.py").touch()
            Path(local_root / "src/subpackage/c.py").touch()
            temp_configuration: configuration.Configuration = (
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        dot_pyre_directory=global_root / ".pyre",
                    ),
                    global_root,
                )
            )

            with setup.switch_working_directory(local_root):

                def assert_backend_paths(
                    raw_paths: Iterable[str],
                    expected: List[str],
                ) -> None:
                    self.assertEqual(
                        sorted(
                            expression_level_coverage.CoveragePaths.from_raw_path_arguments(
                                raw_paths=raw_paths,
                                configuration=frontend_configuration.OpenSource(
                                    temp_configuration
                                ),
                            ).get_paths_for_backend()
                        ),
                        sorted(expected),
                    )

                assert_backend_paths(
                    raw_paths=[],
                    expected=[
                        str(local_root / "src/a.py"),
                        str(local_root / "src/b.py"),
                        str(local_root / "src/subpackage/c.py"),
                    ],
                )
                assert_backend_paths(
                    raw_paths=["@arguments.txt", "@/absolute/arguments.txt"],
                    expected=[
                        "@" + str(local_root / "arguments.txt"),
                        "@/absolute/arguments.txt",
                    ],
                )
                assert_backend_paths(
                    raw_paths=[
                        str(local_root / "src/a.py"),  # absolute path
                        "src/b.py",  # relative path
                    ],
                    expected=[
                        str(local_root / "src/a.py"),
                        str(local_root / "src/b.py"),
                    ],
                )
                assert_backend_paths(
                    raw_paths=[
                        "src/subpackage",  # directory as a path
                    ],
                    expected=[
                        str(local_root / "src/subpackage/c.py"),
                    ],
                )

    def test_backend_exception(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(
                root_path, [".pyre", "allows", "blocks", "search", "local/src"]
            )
            setup.write_configuration_file(
                root_path,
                {
                    "only_check_paths": ["allows", "nonexistent"],
                    "ignore_all_errors": ["blocks", "nonexistent"],
                    "exclude": ["exclude"],
                    "extensions": [".ext", "invalid_extension"],
                    "workers": 42,
                    "search_path": ["search", "nonexistent"],
                    "strict": True,
                },
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["src"]}, relative="local"
            )

            check_configuration = frontend_configuration.OpenSource(
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        dot_pyre_directory=root_path / ".pyre",
                    ),
                    root_path,
                )
            )

            self.mock_callable(daemon_query, "execute_query").to_raise(
                connections.ConnectionFailure
            )
            self.assertEqual(
                expression_level_coverage.run(
                    configuration=check_configuration,
                    paths=[],
                    print_summary=False,
                ),
                commands.ExitCode.SERVER_NOT_FOUND,
            )

    def test_location_to_range(self) -> None:
        def assert_location_to_range(
            response: expression_level_coverage.Location, expected: lsp.LspRange
        ) -> None:
            self.assertEqual(
                expression_level_coverage.location_to_range(response),
                expected,
            )

        assert_location_to_range(
            expression_level_coverage.Location(
                start=expression_level_coverage.Pair(line=1, column=1),
                stop=expression_level_coverage.Pair(line=1, column=7),
            ),
            lsp.LspRange(
                start=lsp.LspPosition(line=0, character=1),
                end=lsp.LspPosition(line=0, character=7),
            ),
        )

    def test_make_diagnostic_for_coverage_gap(self) -> None:
        def assert_make_diagnostic_for_coverage_gap(
            response: expression_level_coverage.CoverageGap, expected: lsp.Diagnostic
        ) -> None:
            self.assertEqual(
                expression_level_coverage.make_diagnostic_for_coverage_gap(response),
                expected,
            )

        assert_make_diagnostic_for_coverage_gap(
            expression_level_coverage.CoverageGap(
                location=expression_level_coverage.Location(
                    start=expression_level_coverage.Pair(line=1, column=1),
                    stop=expression_level_coverage.Pair(line=1, column=7),
                ),
                function_name=None,
                type_="typing.Any",
                reason=["TypeIsAny message."],
            ),
            lsp.Diagnostic(
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=0, character=1),
                    end=lsp.LspPosition(line=0, character=7),
                ),
                message="TypeIsAny message.",
            ),
        )

    def test_get_uncovered_expression_diagnostics(self) -> None:
        def assert_get_uncovered_expression_diagnostics(
            response: expression_level_coverage.ExpressionLevelCoverageResponse,
            expected: List[lsp.Diagnostic],
        ) -> None:
            self.assertEqual(
                expression_level_coverage.get_uncovered_expression_diagnostics(
                    response
                ),
                expected,
            )

        assert_get_uncovered_expression_diagnostics(
            expression_level_coverage.ExpressionLevelCoverageResponse(
                response=[
                    expression_level_coverage.ErrorAtPathResponse(
                        ErrorAtPath=expression_level_coverage.ErrorAtPath(
                            path="test.py",
                            error="error",
                        )
                    )
                ],
            ),
            [],
        )

        assert_get_uncovered_expression_diagnostics(
            expression_level_coverage.ExpressionLevelCoverageResponse(
                response=[
                    expression_level_coverage.CoverageAtPathResponse(
                        CoverageAtPath=expression_level_coverage.CoverageAtPath(
                            path="test.py",
                            total_expressions=7,
                            coverage_gaps=[
                                expression_level_coverage.CoverageGap(
                                    location=expression_level_coverage.Location(
                                        start=expression_level_coverage.Pair(
                                            line=1, column=1
                                        ),
                                        stop=expression_level_coverage.Pair(
                                            line=1, column=7
                                        ),
                                    ),
                                    function_name=None,
                                    type_="typing.Any",
                                    reason=["TypeIsAny message."],
                                )
                            ],
                        )
                    )
                ],
            ),
            [
                lsp.Diagnostic(
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=0, character=1),
                        end=lsp.LspPosition(line=0, character=7),
                    ),
                    message="TypeIsAny message.",
                )
            ],
        )
        assert_get_uncovered_expression_diagnostics(
            expression_level_coverage.ExpressionLevelCoverageResponse(
                response=[
                    expression_level_coverage.CoverageAtPathResponse(
                        CoverageAtPath=expression_level_coverage.CoverageAtPath(
                            path="test.py",
                            total_expressions=7,
                            coverage_gaps=[
                                expression_level_coverage.CoverageGap(
                                    location=expression_level_coverage.Location(
                                        start=expression_level_coverage.Pair(
                                            line=1, column=1
                                        ),
                                        stop=expression_level_coverage.Pair(
                                            line=1, column=7
                                        ),
                                    ),
                                    function_name=None,
                                    type_="typing.List[typing.Any]",
                                    reason=["ContainerParameterIsAny message."],
                                ),
                                expression_level_coverage.CoverageGap(
                                    location=expression_level_coverage.Location(
                                        start=expression_level_coverage.Pair(
                                            line=2, column=4
                                        ),
                                        stop=expression_level_coverage.Pair(
                                            line=2, column=7
                                        ),
                                    ),
                                    function_name=None,
                                    type_="typing.Callable(foo.foo)[[Named(x, unknown)], None]",
                                    reason=["CallableParameterIsUnknownOrAny message."],
                                ),
                            ],
                        )
                    )
                ],
            ),
            [
                lsp.Diagnostic(
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=0, character=1),
                        end=lsp.LspPosition(line=0, character=7),
                    ),
                    message="ContainerParameterIsAny message.",
                ),
                lsp.Diagnostic(
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=1, character=4),
                        end=lsp.LspPosition(line=1, character=7),
                    ),
                    message="CallableParameterIsUnknownOrAny message.",
                ),
            ],
        )
