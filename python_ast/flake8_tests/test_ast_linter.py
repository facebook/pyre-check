#!/usr/bin/env python3

import ast
import unittest
from pathlib import Path
from typing import List, NamedTuple

from .ast_linter import AstChecker, Error


class ExpectedError(NamedTuple):
    line: int
    message: str


class AstVisitorBaseCase(unittest.TestCase):
    def load_checker(self, test_file) -> AstChecker:
        test_repository = "tools/pyre/python_ast/flake8_tests/mock_repository"
        test_file = Path(test_repository) / test_file
        source_code = test_file.read_text()
        tree = ast.parse(source_code)
        return AstChecker(
            tree, source_code.split("\n"), test_repository, str(test_file)
        )

    def assert_errors(self, actual: List[Error], expected: List[ExpectedError]) -> None:
        if len(expected) != len(actual):
            self.fail(
                f"Expected {len(expected)} errors, got {len(actual)}:\n"
                + "\n".join(str(error) for error in actual)
            )
        for expected_error, actual_error in zip(expected, actual):
            self.assertEqual(expected_error.line, actual_error.line)
            self.assertEqual(expected_error.message, actual_error.message)

    def setUp(self) -> None:
        self.checker = self.load_checker("")

    def run_checker(self) -> List[Error]:
        return list(self.checker.run())


class AstVisitorTestCase(AstVisitorBaseCase):
    def setUp(self) -> None:
        self.checker = self.load_checker("a.py")

    def test_linter(self):
        errors = self.run_checker()
        self.assert_errors(
            errors,
            [
                ExpectedError(line=9, message="Assigning to expression of type `int`."),
                ExpectedError(
                    line=11, message="Assigning to expression of type `int`."
                ),
            ],
        )
