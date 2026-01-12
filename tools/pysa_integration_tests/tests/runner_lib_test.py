# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import textwrap
from typing import List

import testslide
from tools.pyre.tools.pysa_integration_tests import runner_lib as test_runner_lib
from tools.pyre.tools.pysa_integration_tests.runner_lib import (
    FunctionTestAnnotations,
    TestAnnotation,
)


class RunnerLibTest(testslide.TestCase):
    def test_parse_annotations(self) -> None:
        self.assertEqual(
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    def foo() -> None:
                        pass
                """
                )
            ),
            {},
        )
        self.assertEqual(
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue(code=1000)
                    def foo() -> None:
                        pass
                """
                )
            ),
            {
                "foo": FunctionTestAnnotations(
                    definition_line=3,
                    annotations=[
                        TestAnnotation(
                            expected=True,
                            code=1000,
                        )
                    ],
                )
            },
        )
        self.assertEqual(
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue(code=1000, line=1, task='T123', currently_found=False)
                    def foo() -> None:
                        pass
                """
                )
            ),
            {
                "foo": FunctionTestAnnotations(
                    definition_line=3,
                    annotations=[
                        TestAnnotation(
                            expected=True,
                            code=1000,
                            line=1,
                            task="T123",
                            currently_found=False,
                        )
                    ],
                )
            },
        )
        self.assertEqual(
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue(code=1000, line=1)
                    @ExpectIssue(code=1000, line=2)
                    @ExpectNoIssue(code=1001)
                    def foo() -> None:
                        pass
                """
                )
            ),
            {
                "foo": FunctionTestAnnotations(
                    definition_line=5,
                    annotations=[
                        TestAnnotation(
                            expected=True,
                            code=1000,
                            line=1,
                        ),
                        TestAnnotation(
                            expected=True,
                            code=1000,
                            line=2,
                        ),
                        TestAnnotation(
                            expected=False,
                            code=1001,
                        ),
                    ],
                )
            },
        )
        # Ignore unknown decorators
        self.assertEqual(
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue(code=1000)
                    @other_decorator()
                    def foo() -> None:
                        pass
                """
                )
            ),
            {
                "foo": FunctionTestAnnotations(
                    definition_line=4,
                    annotations=[
                        TestAnnotation(
                            expected=True,
                            code=1000,
                        )
                    ],
                )
            },
        )
        with self.assertRaises(test_runner_lib.TestConfigurationException):
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue()
                    def foo() -> None:
                        pass
                """
                )
            )
        with self.assertRaises(test_runner_lib.TestConfigurationException):
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue(code='a')
                    def foo() -> None:
                        pass
                """
                )
            )
        with self.assertRaises(test_runner_lib.TestConfigurationException):
            test_runner_lib.parse_test_annotations_from_source(
                textwrap.dedent(
                    """
                    @ExpectIssue(code=1000, unknown=0)
                    def foo() -> None:
                        pass
                """
                )
            )

    @staticmethod
    def make_issue(code: int, line: int) -> test_runner_lib.Issue:
        return {
            "define": "foo",
            "code": code,
            "path": "foo.py",
            "line": line,
            "column": 0,
            "stop_line": line,
            "stop_column": 1,
            "description": "dummy issue",
            "name": "dummy name",
        }

    def assert_compare_to_test_annotations_success(
        self, issues: List[test_runner_lib.Issue], annotations: List[TestAnnotation]
    ) -> None:
        self.assertEqual(
            test_runner_lib.compare_issues_to_test_annotations(
                function="foo",
                definition_line=0,
                code=1000,
                issues=issues,
                annotations=annotations,
            ),
            [],
        )

    def assert_compare_to_test_annotations_fail(
        self, issues: List[test_runner_lib.Issue], annotations: List[TestAnnotation]
    ) -> None:
        self.assertNotEqual(
            test_runner_lib.compare_issues_to_test_annotations(
                function="foo",
                definition_line=0,
                code=1000,
                issues=issues,
                annotations=annotations,
            ),
            [],
        )

    def test_compare_to_test_annotations(self) -> None:
        self.assert_compare_to_test_annotations_success(
            issues=[],
            annotations=[],
        )
        self.assert_compare_to_test_annotations_success(
            issues=[RunnerLibTest.make_issue(code=1000, line=1)],
            annotations=[TestAnnotation(expected=True, code=1000)],
        )
        # expecting no issue.
        self.assert_compare_to_test_annotations_fail(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=2),
            ],
            annotations=[
                TestAnnotation(expected=False, code=1000),
            ],
        )
        # mismatching lines
        self.assert_compare_to_test_annotations_fail(
            issues=[RunnerLibTest.make_issue(code=1000, line=2)],
            annotations=[TestAnnotation(expected=True, code=1000, line=3)],
        )
        self.assert_compare_to_test_annotations_success(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=1),
                RunnerLibTest.make_issue(code=1000, line=2),
            ],
            annotations=[
                TestAnnotation(expected=True, code=1000, line=1),
                TestAnnotation(expected=True, code=1000, line=2),
            ],
        )
        self.assert_compare_to_test_annotations_fail(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=1),
                RunnerLibTest.make_issue(code=1000, line=2),
            ],
            annotations=[
                TestAnnotation(expected=True, code=1000, line=1),
                TestAnnotation(expected=True, code=1000, line=3),
            ],
        )
        # mismatching number of issues.
        self.assert_compare_to_test_annotations_fail(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=1),
                RunnerLibTest.make_issue(code=1000, line=2),
            ],
            annotations=[
                TestAnnotation(expected=True, code=1000),
            ],
        )
        # mix of ExpectIssue and ExpectNoIssue
        self.assert_compare_to_test_annotations_success(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=2),
            ],
            annotations=[
                TestAnnotation(expected=False, code=1000, line=1),
                TestAnnotation(expected=True, code=1000, line=2),
                TestAnnotation(expected=False, code=1000, line=3),
            ],
        )
        # test with currently_found=True
        self.assert_compare_to_test_annotations_success(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=2),
            ],
            annotations=[
                TestAnnotation(expected=False, code=1000, line=2, currently_found=True),
            ],
        )
        # mix of currently_found=True
        self.assert_compare_to_test_annotations_success(
            issues=[
                RunnerLibTest.make_issue(code=1000, line=2),
                RunnerLibTest.make_issue(code=1000, line=3),
            ],
            annotations=[
                TestAnnotation(expected=False, code=1000, line=2, currently_found=True),
                TestAnnotation(expected=True, code=1000, line=3),
            ],
        )
