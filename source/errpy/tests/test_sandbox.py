#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest
from difflib import unified_diff
from typing import Optional

import tools.pyre.source.errpy.tests.utils.ast_utils as ast_utils

from tools.pyre.source.errpy.tests.utils.test_common import read_code


class TestSandbox(unittest.TestCase):
    """The sandbox area can be used in order to test ERRPY syntax and compare
    the output AST with that produced by CPython."""

    maxDiff: Optional[int] = (
        None  # this is to display large diffs which we want for this tool
    )

    def check_ast_file(self, fname: str, pretty_print: bool, flat_ast: bool) -> None:
        code = read_code(fname)
        expected = ast_utils.get_cpython_ast(code, pretty_print=pretty_print).strip()

        if pretty_print:
            (got_ast, errors), _ = ast_utils.run_errpy(
                code,
            )
        else:
            (got_ast, errors), _ = ast_utils.run_errpy_ast_only(
                code,
            )

        if errors:
            got_ast += errors

        got_ast = got_ast.strip()

        if got_ast != expected:
            if not flat_ast:
                got_ast = ast_utils.format_ast_with_indentation(got_ast)
                expected = ast_utils.format_ast_with_indentation(expected)

            print("\n\ntest fail\n")
            print("Result:\n" + got_ast)
            print("\nExpect:\n" + expected)

            print(
                "\nDiff:\n"
                + "".join(
                    unified_diff(
                        expected.splitlines(keepends=True),
                        got_ast.splitlines(keepends=True),
                    )
                )
                + "\n"
            )

            self.assertEqual(got_ast, expected)

    def test_sandbox_ast_and_pretty(self) -> None:
        self.check_ast_file("sandbox.pytest", pretty_print=True, flat_ast=False)

    def test_sandbox_just_ast(self) -> None:
        self.check_ast_file("sandbox.pytest", pretty_print=False, flat_ast=False)

    def test_sandbox_flat_ast(self) -> None:
        self.check_ast_file("sandbox.pytest", pretty_print=False, flat_ast=True)

    def test_sandbox_flat_ast_and_pretty(self) -> None:
        self.check_ast_file("sandbox.pytest", pretty_print=True, flat_ast=True)


if __name__ == "__main__":
    unittest.main()
