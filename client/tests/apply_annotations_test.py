# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


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
            stub="""
            def foo() -> int: ...
            """,
            source="""
            def foo():
                return 1
            """,
            expected="""
            def foo() -> int:
                return 1
            """,
        )

        self.assert_annotations(
            stub="""
            def foo() -> int: ...

            class A:
                def foo() -> str: ...
            """,
            source="""
            def foo():
                return 1
            class A:
                def foo():
                    return ''
            """,
            expected="""
            def foo() -> int:
                return 1
            class A:
                def foo() -> str:
                    return ''
            """,
        )

        self.assert_annotations(
            stub="""
            bar: int = ...
            """,
            source="""
            bar = foo()
            """,
            expected="""
            bar: int = foo()
            """,
        )

        self.assert_annotations(
            stub="""
            bar: int = ...
            """,
            source="""
            bar: str = foo()
            """,
            expected="""
            bar: str = foo()
            """,
        )

        self.assert_annotations(
            stub="""
            bar: int = ...
            class A:
                bar: str = ...
            """,
            source="""
            bar = foo()
            class A:
                bar = foobar()
            """,
            expected="""
            bar: int = foo()
            class A:
                bar: str = foobar()
            """,
        )

        self.assert_annotations(
            stub="""
            bar: int = ...
            class A:
                bar: str = ...
            """,
            source="""
            bar = foo()
            class A:
                bar = foobar()
            """,
            expected="""
            bar: int = foo()
            class A:
                bar: str = foobar()
            """,
        )

        self.assert_annotations(
            stub="""
            a: int = ...
            b: str = ...
            """,
            source="""
            def foo() -> Tuple[int, str]:
                return (1, "")

            a, b = foo()
            """,
            expected="""
            a: int
            b: str

            def foo() -> Tuple[int, str]:
                return (1, "")

            a, b = foo()
            """,
        )

        self.assert_annotations(
            stub="""
            x: int = ...
            y: int = ...
            z: int = ...
            """,
            source="""
            x = y = z = 1
            """,
            expected="""
            x: int
            y: int
            z: int

            x = y = z = 1
            """,
        )

        # Don't add annotations if one is already present
        self.assert_annotations(
            stub="""
            def foo(x: int = 1) -> List[str]: ...
            """,
            source="""
            from typing import Iterable, Any

            def foo(x = 1) -> Iterable[Any]:
                return ['']
            """,
            expected="""
            from typing import Iterable, Any

            def foo(x: int = 1) -> Iterable[Any]:
                return ['']
            """,
        )

        self.assert_annotations(
            stub="""
            from typing import List

            def foo() -> List[int]: ...
            """,
            source="""
            def foo():
                return [1]
            """,
            expected="""
            from typing import List

            def foo() -> List[int]:
                return [1]
            """,
        )

        self.assert_annotations(
            stub="""
            from typing import List

            def foo() -> List[int]: ...
            """,
            source="""
            from typing import Union

            def foo():
                return [1]
            """,
            expected="""
            from typing import List, Union

            def foo() -> List[int]:
                return [1]
            """,
        )

        self.assert_annotations(
            stub="""
            a: Dict[str, int] = ...
            """,
            source="""
            def foo() -> int:
                return 1
            a = {}
            a['x'] = foo()
            """,
            expected="""
            def foo() -> int:
                return 1
            a: Dict[str, int] = {}
            a['x'] = foo()
            """,
        )

        self.assert_annotations(
            stub="""
            from typing import Any, Dict

            b: Dict[Any, Any] = ...
            """,
            source="""
            from typing import Tuple

            def foo() -> Tuple[int, str]:
                return 1, ""

            b = {}

            b['z'] = b['x'] = foo()

            """,
            expected="""
            from typing import Any, Dict, Tuple

            def foo() -> Tuple[int, str]:
                return 1, ""

            b: Dict[Any, Any] = {}

            b['z'] = b['x'] = foo()
            """,
        )
        # Test that tuples with subscripts are handled correctly
        # and top level annotations are added in the correct place
        self.assert_annotations(
            stub="""
            a: int = ...
            """,
            source="""
            from typing import Tuple

            def foo() -> Tuple[str, int]:
                return "", 1

            b['z'], a = foo()
            """,
            expected="""
            from typing import Tuple
            a: int

            def foo() -> Tuple[str, int]:
                return "", 1

            b['z'], a = foo()
            """,
        )

        # Don't override existing default parameter values
        self.assert_annotations(
            stub="""
            class B:
                def foo(self, x: int = a.b.A.__add__(1), y=None) -> int: ...
            """,
            source="""
            class B:
                def foo(self, x = A + 1, y = None) -> int:
                    return x

            """,
            expected="""
            class B:
                def foo(self, x: int = A + 1, y = None) -> int:
                    return x
            """,
        )

        self.assert_annotations(
            stub="""
            def foo(x: int) -> int: ...
            """,
            source="""
            def foo(x) -> int:
                return x
            """,
            expected="""
            def foo(x: int) -> int:
                return x
            """,
        )

        self.assert_annotations(
            stub="""
            async def a(r: Request, z=None) -> django.http.response.HttpResponse: ...
            async def b(r: Request, z=None) -> django.http.response.HttpResponse: ...
            async def c(r: Request, z=None) -> django.http.response.HttpResponse: ...
            """,
            source="""
            async def a(r: Request, z=None): ...
            async def b(r: Request, z=None): ...
            async def c(r: Request, z=None): ...
            """,
            expected="""
            from django.http.response import HttpResponse

            async def a(r: Request, z=None) -> HttpResponse: ...
            async def b(r: Request, z=None) -> HttpResponse: ...
            async def c(r: Request, z=None) -> HttpResponse: ...
            """,
        )

        self.assert_annotations(
            stub="""
            FOO: a.b.Example = ...
            """,
            source="""
            FOO = bar()
            """,
            expected="""
            from a.b import Example

            FOO: Example = bar()
            """,
        )

        self.assert_annotations(
            stub="""
            FOO: Union[a.b.Example, int] = ...
            """,
            source="""
            FOO = bar()
            """,
            expected="""
            from a.b import Example

            FOO: Union[Example, int] = bar()
            """,
        )

        self.assert_annotations(
            stub="""
            def foo(x: int) -> List[Union[a.b.Example, str]]: ...
            """,
            source="""
            def foo(x: int):
                return [barfoo(), ""]
            """,
            expected="""
            from a.b import Example

            def foo(x: int) -> List[Union[Example, str]]:
                return [barfoo(), ""]
            """,
        )

        self.assert_annotations(
            stub="""
            def foo(x: int) -> Optional[a.b.Example]: ...
            """,
            source="""
            def foo(x: int):
                pass
            """,
            expected="""
            from a.b import Example

            def foo(x: int) -> Optional[Example]:
                pass
            """,
        )

        self.assert_annotations(
            stub="""
            def foo(x: int) -> str: ...
            """,
            source="""
            def foo(x: str):
                pass
            """,
            expected="""
            def foo(x: str) -> str:
                pass
            """,
        )

        self.assert_annotations(
            stub="""
            def foo(x: int)-> Union[
                Coroutine[Any, Any, django.http.response.HttpResponse], str
            ]:
                ...
            """,
            source="""
            def foo(x: int):
                pass
            """,
            expected="""
            from django.http.response import HttpResponse

            def foo(x: int) -> Union[
                Coroutine[Any, Any, HttpResponse], str
            ]:
                pass
            """,
        )

        self.assert_annotations(
            stub="""
            def foo(x: django.http.response.HttpResponse) -> str:
                pass
            """,
            source="""
            def foo(x) -> str:
                pass
            """,
            expected="""
            from django.http.response import HttpResponse

            def foo(x: HttpResponse) -> str:
                pass
            """,
        )

        self.assert_annotations(
            stub="""
            from typing import Any, Dict, List

            from typing import Any, List

            def foo() -> List[Any]: ...
            def goo() -> Dict[Any, Any]: ...
            """,
            source="""
            from typing import Any

            def foo():
                return []

            def goo():
                return {}

            """,
            expected="""
            from typing import Dict, List, Any

            def foo() -> List[Any]:
                return []

            def goo() -> Dict[Any, Any]:
                return {}

            """,
        )

        self.assert_annotations(
            stub="""
            def foo() -> b.b.A: ...
            """,
            source="""
            from c import bar, A

            def foo():
                return bar()
            """,
            expected="""
            from c import bar, A
            from b.b import A

            def foo() -> A:
                return bar()
            """,
        )
        self.assert_annotations(
            stub="""
            def foo() -> b.b.A: ...
            """,
            source="""
            from a import *
            from c import bar, A

            def foo():
                return bar()
            """,
            expected="""
            from a import *
            from c import bar, A
            from b.b import A

            def foo() -> A:
                return bar()
            """,
        )
        self.assert_annotations(
            stub="""
            def foo() -> b.b.A: ...
            """,
            source="""
            from c import A as B, bar

            def foo():
                return bar()
            """,
            expected="""
            from c import A as B, bar
            from b.b import A

            def foo() -> A:
                return bar()
            """,
        )
        # Work around to avoid marking this test file as generated.
        generated = "generated"
        self.assert_annotations(
            stub="""
            def foo() -> int: ...
            """,
            source=f"""
            # @{generated}
            def foo():
                return 1
            """,
            expected=f"""
            # @{generated}
            def foo() -> int:
                return 1
            """,
        )
        self.assert_annotations(
            stub="""
            from typing import Type

            def foo() -> Type[foo.A]: ...
            """,
            source="""
            def foo():
                class A:
                    x = 1
                return A

            """,
            expected="""
            from typing import Type

            def foo() -> Type[foo.A]:
                class A:
                    x = 1
                return A
            """,
        )
        self.assert_annotations(
            stub="""
            def foo() -> db.Connection: ...
            """,
            source="""
            import my.cool.db as db
            def foo():
              return db.Connection()
            """,
            expected="""
            import my.cool.db as db
            def foo() -> db.Connection:
              return db.Connection()
            """,
        )
        self.assert_annotations(
            stub="""
            def foo() -> typing.Sequence[int]: ...
            """,
            source="""
            import typing
            def foo():
              return []
            """,
            expected="""
            import typing
            def foo() -> typing.Sequence[int]:
              return []
            """,
        )

    def test_insert_simple_class(self) -> None:
        """Insert a TypedDict class that is not in the source file."""
        self.assert_annotations(
            stub="""
            from mypy_extensions import TypedDict

            class MovieTypedDict(TypedDict):
                name: str
                year: int
            """,
            source="""
            def foo() -> None:
                pass
            """,
            expected="""
            from mypy_extensions import TypedDict

            class MovieTypedDict(TypedDict):
                name: str
                year: int

            def foo() -> None:
                pass
            """,
        )

    def test_insert_only_class_that_does_not_exist(self) -> None:
        self.assert_annotations(
            stub="""
            from mypy_extensions import TypedDict

            class MovieTypedDict(TypedDict):
                name: str
                year: int

            class ExistingMovieTypedDict(TypedDict):
                name: str
                year: int
            """,
            source="""
            from mypy_extensions import TypedDict

            class ExistingMovieTypedDict(TypedDict):
                name: str
                year: int

            def foo() -> None:
                pass
            """,
            expected="""
            from mypy_extensions import TypedDict

            class MovieTypedDict(TypedDict):
                name: str
                year: int

            class ExistingMovieTypedDict(TypedDict):
                name: str
                year: int

            def foo() -> None:
                pass
            """,
        )
