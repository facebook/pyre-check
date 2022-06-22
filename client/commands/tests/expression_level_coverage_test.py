# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from .. import expression_level_coverage

from ..query import Response


class ExpressionLevelTest(testslide.TestCase):
    def test_get_expression_level_coverage_response(self) -> None:
        self.assertEqual(
            expression_level_coverage._get_expression_level_coverage_response(
                Response(
                    {
                        "response": [
                            {
                                "path": "test.py",
                                "total_expressions": 7,
                                "coverage_gaps": [
                                    {
                                        "location": {
                                            "start": {"line": 11, "column": 16},
                                            "stop": {"line": 11, "column": 17},
                                        },
                                        "type_": "typing.Any",
                                        "reason": ["TypeIsAny"],
                                    },
                                ],
                            }
                        ]
                    }
                ).payload,
            ),
            [
                expression_level_coverage.ExpressionLevelCoverage(
                    path="test.py",
                    total_expressions=7,
                    coverage_gaps=[
                        expression_level_coverage.CoverageGap(
                            location=expression_level_coverage.Location(
                                start=expression_level_coverage.Pair(
                                    line=11, column=16
                                ),
                                stop=expression_level_coverage.Pair(line=11, column=17),
                            ),
                            type_="typing.Any",
                            reason=["TypeIsAny"],
                        )
                    ],
                )
            ],
        )
        with self.assertRaises(expression_level_coverage.ErrorParsingFailure):
            expression_level_coverage._get_expression_level_coverage_response(
                "garbage input"
            )
        with self.assertRaises(expression_level_coverage.ErrorParsingFailure):
            expression_level_coverage._get_expression_level_coverage_response(
                {
                    "response": [
                        {
                            "paths": "path_not_paths.py",
                            "total_expressions": 0,
                            "coverage_gaps": [],
                        }
                    ]
                }
            )

    def test_calculate_percent_covered(self) -> None:
        self.assertEqual(
            expression_level_coverage._calculate_percent_covered(0, 0), 100.0
        )
        self.assertEqual(
            expression_level_coverage._calculate_percent_covered(3, 7), 57.14
        )

    def test_get_total_and_uncovered_expressions(self) -> None:
        coverage = expression_level_coverage.ExpressionLevelCoverage(
            path="test.py",
            total_expressions=7,
            coverage_gaps=[
                expression_level_coverage.CoverageGap(
                    location=expression_level_coverage.Location(
                        start=expression_level_coverage.Pair(line=11, column=16),
                        stop=expression_level_coverage.Pair(line=11, column=17),
                    ),
                    type_="typing.Any",
                    reason=["TypeIsAny"],
                )
            ],
        )
        self.assertEqual(
            expression_level_coverage._get_total_and_uncovered_expressions(coverage),
            (7, 1),
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
            Response({"response": []}).payload,
            "Overall: 100.0% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            Response(
                {
                    "response": [
                        {
                            "path": "test.py",
                            "total_expressions": 0,
                            "coverage_gaps": [],
                        }
                    ]
                }
            ).payload,
            "test.py: 100.0% expressions are covered\nOverall: 100.0% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            Response(
                {
                    "response": [
                        {
                            "path": "test.py",
                            "total_expressions": 7,
                            "coverage_gaps": [
                                {
                                    "location": {
                                        "start": {"line": 11, "column": 16},
                                        "stop": {"line": 11, "column": 17},
                                    },
                                    "type_": "typing.Any",
                                    "reason": ["TypeIsAny"],
                                },
                                {
                                    "location": {
                                        "start": {"line": 12, "column": 11},
                                        "stop": {"line": 12, "column": 12},
                                    },
                                    "type_": "typing.Any",
                                    "reason": ["TypeIsAny"],
                                },
                            ],
                        }
                    ]
                }
            ).payload,
            "test.py: 71.43% expressions are covered\nOverall: 71.43% expressions are covered",
        )
        assert_summary_expression_level_coverage(
            Response(
                {
                    "response": [
                        {
                            "path": "library.py",
                            "total_expressions": 4,
                            "coverage_gaps": [],
                        },
                        {
                            "path": "test.py",
                            "total_expressions": 7,
                            "coverage_gaps": [
                                {
                                    "location": {
                                        "start": {"line": 11, "column": 16},
                                        "stop": {"line": 11, "column": 17},
                                    },
                                    "type_": "typing.Any",
                                    "reason": ["TypeIsAny"],
                                },
                                {
                                    "location": {
                                        "start": {"line": 12, "column": 11},
                                        "stop": {"line": 12, "column": 12},
                                    },
                                    "type_": "typing.Any",
                                    "reason": ["TypeIsAny"],
                                },
                            ],
                        },
                    ]
                }
            ).payload,
            "library.py: 100.0% expressions are covered\ntest.py: 71.43% expressions are covered\nOverall: 81.82% expressions are covered",
        )
