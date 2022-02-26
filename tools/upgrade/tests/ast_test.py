# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from .. import ast


class ErrorsTest(unittest.TestCase):
    def test_check_stable(self) -> None:
        ast.check_stable("def foo(): pass", "def foo():\n   pass")
        with self.assertRaises(ast.UnstableAST):
            ast.check_stable("def foo(): pass", "def bar(): pass")
        with self.assertRaises(SyntaxError):
            ast.check_stable("def foo():", "def foo():")

    def test_check_stable_decorator(self) -> None:
        @ast.check_stable_transformation
        def _stable_transformation(input: str) -> str:
            return input + "\n\n# comment"

        _stable_transformation("def foo(): pass")
        with self.assertRaises(SyntaxError):
            # Clients are responsible for passing valid inputs.
            _stable_transformation("def foo(:")

        @ast.check_stable_transformation
        def _unstable_transformation(input: str) -> str:
            return input + "\n\npass"

        with self.assertRaises(ast.UnstableAST):
            _unstable_transformation("def foo(): pass")

        @ast.check_stable_transformation
        def _invalid_syntax_transformation(input: str) -> str:
            return input + "\n\ndef foo(:"

        with self.assertRaises(ast.UnstableAST):
            _invalid_syntax_transformation("def foo(): pass")
