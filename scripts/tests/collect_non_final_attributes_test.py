# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import textwrap
import unittest
from typing import Dict, List

from libcst import Module, parse_module

from ..collect_non_final_attributes import _get_attributes


class CollectNonFinalAttributesTest(unittest.TestCase):
    @staticmethod
    def format_source(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_attributes(self, source: str, expected: Dict[str, List[str]]) -> None:
        source_file = self.format_source(source)
        self.assertEqual(_get_attributes(source_file), expected)

    def test_collect_non_final_attributes(self) -> None:
        self.assert_attributes(
            source="""
            class Test:
                def __init__(self, x, y):
                    self.x: int = x
                    self.y: int = y

                def modify_attr(self, x: int) -> None:
                    self.x = x
            """,
            expected={"Test": ["x"]},
        )

        self.assert_attributes(
            source="""
            class Test:
                def __init__(self, x, y):
                    self.x: int = x
                    self.y: int = y

                def modify_attr(self, x: int) -> None:
                    self.x = x
                    self.y = x * 2
            """,
            expected={"Test": ["x", "y"]},
        )

        self.assert_attributes(
            source="""
            class Test:
                def __init__(self, x, y):
                    self.x: int = x
                    self.y: int = y

                def noop(self) -> None:
                    pass
            """,
            expected={},
        )

        self.assert_attributes(
            source="""
            class TestA:
                def __init__(self, x, y):
                    self.x: int = x
                    self.y: int = y

                def modify_attr(self, y) -> None:
                    self.y = y

            class TestB:
                def __init__(self, a, b):
                    self.a = a
                    self.b = b

                def modify_attr(self, a, b) -> None:
                    self.a = a
                    self.b = b
            """,
            expected={"TestA": ["y"], "TestB": ["a", "b"]},
        )

        self.assert_attributes(
            source="""
            class Test:
                def __init__(self, a, b):
                    self.a: int = x
                    self.b: int = y

                def modify_attr(self) -> None:
                    self.a += 2
            """,
            expected={"Test": ["a"]},
        )

        self.assert_attributes(
            source="""
            class Test:
                def __init__(self, a, b):
                    self.a: int = x
                    self.b: int = y

                def modify_attr(self) -> None:
                    self.a += 2
                    self.b -= 2
            """,
            expected={"Test": ["a", "b"]},
        )
