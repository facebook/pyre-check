#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest

from tools.pyre.source.errpy.tests.utils.test_common import ASTTestCommon


class AllGrammarTests(ASTTestCommon):
    """These tests focus on individual aspects of Python syntax"""

    def test_ast_expressions(self) -> None:
        self.check_many_cases_in_file("ast_expressions.pytest")

    def test_ast_async_and_await(self) -> None:
        self.check_many_cases_in_file("ast_async_and_await.pytest")

    def test_ast_control_flow(self) -> None:
        self.check_many_cases_in_file("ast_control_flow.pytest")

    def test_ast_variables(self) -> None:
        self.check_many_cases_in_file("ast_variables.pytest")

    def test_ast_functions_and_classes(self) -> None:
        self.check_many_cases_in_file("ast_functions_and_classes.pytest")

    def test_ast_literals(self) -> None:
        self.check_many_cases_in_file("ast_literals.pytest")

    def test_ast_pattern_matching(self) -> None:
        self.check_many_cases_in_file("ast_pattern_matching.pytest")

    def test_ast_statements(self) -> None:
        self.check_many_cases_in_file("ast_statements.pytest")


if __name__ == "__main__":
    unittest.main()
