# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import textwrap
import unittest
from typing import Tuple

from libcst import Module, parse_module

from ..apply_annotations import _annotate_source


class ApplyAnnotationsTest(unittest.TestCase):
    @staticmethod
    def format_files(
        stub: str, source: str, expected: str
    ) -> Tuple[Module, Module, str]:
        return (
            parse_module(textwrap.dedent(stub.rstrip())),
            parse_module(textwrap.dedent(source.rstrip())),
            textwrap.dedent(expected.rstrip()),
        )

    def assert_annotations(self, stub: str, source: str, expected: str) -> None:
        stub_file, source_file, expected = self.format_files(stub, source, expected)
        self.assertEqual(_annotate_source(stub_file, source_file).code, expected)

    def test_annotate_functions(self) -> None:
        self.assert_annotations(
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

        self.assert_annotations(
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

        self.assert_annotations(
            """
            bar: int = ...
            """,
            """
            bar = foo()
            """,
            """
            bar: int = foo()
            """,
        )

        self.assert_annotations(
            """
            bar: int = ...
            """,
            """
            bar: str = foo()
            """,
            """
            bar: str = foo()
            """,
        )

        self.assert_annotations(
            """
            bar: int = ...
            class A:
                bar: str = ...
            """,
            """
            bar = foo()
            class A:
                bar = foobar()
            """,
            """
            bar: int = foo()
            class A:
                bar: str = foobar()
            """,
        )

        self.assert_annotations(
            """
            bar: int = ...
            class A:
                bar: str = ...
            """,
            """
            bar = foo()
            class A:
                bar = foobar()
            """,
            """
            bar: int = foo()
            class A:
                bar: str = foobar()
            """,
        )

        self.assert_annotations(
            """
            a: int = ...
            b: str = ...
            """,
            """
            def foo() -> Tuple[int, str]:
                return (1, "")

            a, b = foo()
            """,
            """
            b: str
            a: int
            def foo() -> Tuple[int, str]:
                return (1, "")

            a, b = foo()
            """,
        )

        self.assert_annotations(
            """
            x: int = ...
            y: int = ...
            z: int = ...
            """,
            """
            x = y = z = 1
            """,
            """
            z: int
            y: int
            x: int
            x = y = z = 1
            """,
        )
