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
            a: int
            b: str

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
            x: int
            y: int
            z: int

            x = y = z = 1
            """,
        )

        # Don't add annotations if one is already present
        self.assert_annotations(
            """
            def foo(x: int = 1) -> List[str]: ...
            """,
            """
            from typing import Iterable, Any

            def foo(x = 1) -> Iterable[Any]:
                return ['']
            """,
            """
            from typing import Iterable, Any

            def foo(x: int = 1) -> Iterable[Any]:
                return ['']
            """,
        )

        self.assert_annotations(
            """
            from typing import List

            def foo() -> List[int]: ...
            """,
            """
            def foo():
                return [1]
            """,
            """
            from typing import List

            def foo() -> List[int]:
                return [1]
            """,
        )

        self.assert_annotations(
            """
            from typing import List
            def foo() -> List[int]: ...
            """,
            """
            from typing import Union
            def foo():
                return [1]
            """,
            """
            from typing import Union, List
            def foo() -> List[int]:
                return [1]
            """,
        )

        self.assert_annotations(
            """
            from typing import Any

            a: Dict[Any, Any] = ...
            """,
            """
            def foo() -> int:
                return 1
            a = {}
            a['x'] = foo()
            """,
            """
            from typing import Any

            def foo() -> int:
                return 1
            a: Dict[Any, Any] = {}
            a['x'] = foo()
            """,
        )

        self.assert_annotations(
            """
            from typing import Any, Dict

            b: Dict[Any, Any] = ...
            """,
            """
            from typing import  Tuple

            def foo() -> Tuple[int, str]:
                return 1, ""

            b = {}

            b['z'] = b['x'] = foo()

            """,
            """
            from typing import  Tuple, Any, Dict

            def foo() -> Tuple[int, str]:
                return 1, ""

            b: Dict[Any, Any] = {}

            b['z'] = b['x'] = foo()
            """,
        )
        # Test that tuples with subscripts are handled correctly
        # and top level annotations are added in the correct place
        self.assert_annotations(
            """
            a: int = ...
            """,
            """
            from typing import Tuple

            def foo() -> Tuple[str, int]:
                return "", 1

            b['z'], a = foo()
            """,
            """
            from typing import Tuple
            a: int

            def foo() -> Tuple[str, int]:
                return "", 1

            b['z'], a = foo()
            """,
        )

        # Don't override existing default parameter values
        self.assert_annotations(
            """
            class B:
                def foo(self, x: int = a.b.A.__add__(1), y=None) -> int: ...
            """,
            """
            class B:
                def foo(self, x = A + 1, y = None) -> int:
                    return x

            """,
            """
            class B:
                def foo(self, x: int = A + 1, y = None) -> int:
                    return x
            """,
        )

        self.assert_annotations(
            """
            def foo(x: int) -> int: ...
            """,
            """
            def foo(x) -> int:
                return x
            """,
            """
            def foo(x: int) -> int:
                return x
            """,
        )

        self.assert_annotations(
            """
            async def a(r: Request, z=None) -> django.http.response.HttpResponse: ...
            async def b(r: Request, z=None) -> django.http.response.HttpResponse: ...
            async def c(r: Request, z=None) -> django.http.response.HttpResponse: ...
            """,
            """
            async def a(r: Request, z=None): ...
            async def b(r: Request, z=None): ...
            async def c(r: Request, z=None): ...
            """,
            """
            from django.http.response import HttpResponse

            async def a(r: Request, z=None) -> HttpResponse: ...
            async def b(r: Request, z=None) -> HttpResponse: ...
            async def c(r: Request, z=None) -> HttpResponse: ...
            """,
        )

        self.assert_annotations(
            """
            def foo(x: int) -> int: ...
            """,
            """
            def foo(x) -> int:
                return x
            """,
            """
            def foo(x: int) -> int:
                return x
            """,
        )
