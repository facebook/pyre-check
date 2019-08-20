# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import textwrap
import unittest

from libcst import parse_module

from ..apply_annotations import _annotate_functions


class ApplyAnnotationsTest(unittest.TestCase):
    def assert_annotate_functions(self, stub: str, source: str, expected: str) -> None:
        stub_file = parse_module(textwrap.dedent(stub.rstrip()))
        source_file = parse_module(textwrap.dedent(source.rstrip()))
        self.assertEqual(
            _annotate_functions(stub_file, source_file).code,
            textwrap.dedent(expected.rstrip()),
        )

    def test_annotate_file(self) -> None:
        self.assert_annotate_functions(
            """
            def foo() -> int: ...
            """,
            """
            def foo():
                return 1
            """,
            """
            def foo() -> int:
                return 1
            """,
        )

        self.assert_annotate_functions(
            """
            def foo() -> int: ...

            class A:
                def foo() -> str: ...
            """,
            """
            def foo():
                return 1
            class A:
                def foo():
                    return ''
            """,
            """
            def foo() -> int:
                return 1
            class A:
                def foo() -> str:
                    return ''
            """,
        )
