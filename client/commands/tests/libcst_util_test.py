# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import testslide

from ...language_server import protocol as lsp

from .. import libcst_util


test_root = "/test_root"
test_path = "/test_root/test_project/test_module.py"


def create_lsp_range(line: int, start: int, end: int) -> lsp.LspRange:
    # Handle the offsets for the test creater for readability.
    # The positions should line up with what it shows on IDE's.
    line = line - 1
    start = start - 1
    end = end - 1
    return lsp.LspRange(
        start=lsp.LspPosition(line, start),
        end=lsp.LspPosition(line, end),
    )


class LibcstUtilTest(testslide.TestCase):
    def setUp(self) -> None:
        self.maxDiff = None

    def test_success_case(self) -> None:
        """
        Tests success cases for:
        1. TODO:import statement
        2. function name
        3. imported Types
        4. global scoped variables
        5. out of order variables
        """
        test_code: str = """
import os
from pathlib import Path as TestPath

test_path: str = "TEST_PATH"

def get_path() -> TestPath:
    return TestPath(os.environ[test_path])

def count_level() -> int:
    return x.split("")

x = get_path()
print(count_level())
"""

        # 2 - get_path
        visitor: libcst_util.QualifiedNameWithPositionVisitor = (
            libcst_util.generate_qualified_name_with_position_visitor(
                Path(test_path),
                Path(test_root),
                test_code,
                lsp.PyrePosition(line=7, character=4),
            )
        )
        results = visitor.find_references()
        self.assertEqual(
            results, [create_lsp_range(7, 5, 13), create_lsp_range(13, 5, 13)]
        )

        # 3 - `TestPath` in return statement of `get_path`
        visitor = libcst_util.generate_qualified_name_with_position_visitor(
            Path(test_path),
            Path(test_root),
            test_code,
            lsp.PyrePosition(line=7, character=19),
        )
        results = visitor.find_references()
        self.assertEqual(
            results, [create_lsp_range(7, 19, 27), create_lsp_range(8, 12, 20)]
        )

        # 4 - `test_path`
        visitor = libcst_util.generate_qualified_name_with_position_visitor(
            Path(test_path),
            Path(test_root),
            test_code,
            lsp.PyrePosition(line=5, character=1),
        )
        results = visitor.find_references()
        self.assertEqual(
            results, [create_lsp_range(5, 1, 10), create_lsp_range(8, 32, 41)]
        )

        # 5 - `x`
        visitor = libcst_util.generate_qualified_name_with_position_visitor(
            Path(test_path),
            Path(test_root),
            test_code,
            lsp.PyrePosition(line=11, character=12),
        )
        references_to_x = [create_lsp_range(11, 12, 13), create_lsp_range(13, 1, 2)]
        results = visitor.find_references()
        self.assertEqual(results, references_to_x)

        visitor = libcst_util.generate_qualified_name_with_position_visitor(
            Path(test_path),
            Path(test_root),
            test_code,
            lsp.PyrePosition(line=13, character=1),
        )
        results = visitor.find_references()
        self.assertEqual(results, references_to_x)

    """
    Things we dont' expect to return references for:
    1. Keywords
    2. Literals
        a. string
        b. int
        c. bool
    """

    def test_keyword(self) -> None:
        test_code: str = """
for x in y:
    print(x)

for foo in bar:
    pass
"""
        visitor: libcst_util.QualifiedNameWithPositionVisitor = (
            libcst_util.generate_qualified_name_with_position_visitor(
                Path(test_path),
                Path(test_root),
                test_code,
                lsp.PyrePosition(line=2, character=1),
            )
        )
        results = visitor.find_references()
        self.assertEqual(results, [])

        visitor = libcst_util.generate_qualified_name_with_position_visitor(
            Path(test_path),
            Path(test_root),
            test_code,
            lsp.PyrePosition(line=6, character=4),
        )
        results = visitor.find_references()
        self.assertEqual(results, [])

    def test_int(self) -> None:
        test_code: str = """
a : int = 1
b : int = 1 + 1
"""
        visitor: libcst_util.QualifiedNameWithPositionVisitor = (
            libcst_util.generate_qualified_name_with_position_visitor(
                Path(test_path),
                Path(test_root),
                test_code,
                lsp.PyrePosition(line=2, character=11),
            )
        )
        results = visitor.find_references()
        self.assertEqual(results, [])

    def test_booleans(self) -> None:

        test_code: str = """
def foo() -> None:
    if True:
        return False
    elif False:
        pass
    return True
"""
        visitor: libcst_util.QualifiedNameWithPositionVisitor = (
            libcst_util.generate_qualified_name_with_position_visitor(
                Path(test_path),
                Path(test_root),
                test_code,
                lsp.PyrePosition(line=3, character=8),
            )
        )
        results = visitor.find_references()
        self.assertEqual(results, [])

    def test_string(self) -> None:
        test_code: str = """
c: string = "hello"
d: string = "hello" + "world"
"""
        visitor: libcst_util.QualifiedNameWithPositionVisitor = (
            libcst_util.generate_qualified_name_with_position_visitor(
                Path(test_path),
                Path(test_root),
                test_code,
                lsp.PyrePosition(line=2, character=14),
            )
        )
        results = visitor.find_references()
        self.assertEqual(results, [])
