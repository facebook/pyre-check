# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import textwrap
import unittest
from pathlib import Path
from typing import cast

from ...commands.infer_v2 import (
    _create_module_annotations,
    RawInferOutput,
    RawInferOutputDict,
    ModuleAnnotations,
)


PATH = "test.py"


def _raw_infer_output(
    data: dict[str, object] | None,
) -> RawInferOutput:
    data = data or {}
    for category in RawInferOutput.categories:
        data[category] = data.get(category, [])
        # pyre-ignore[16]: Converting raw dicts to dataclasses.
        for annotation in data[category]:
            annotation["location"] = {"path": PATH}
    return RawInferOutput(data=cast(RawInferOutputDict, data))


def _create_test_module_annotations(
    data: dict[str, object] | None,
    complete_only: bool,
) -> ModuleAnnotations:
    all_module_annotations = _create_module_annotations(
        infer_output=_raw_infer_output(data=data),
        complete_only=complete_only,
    )
    if len(all_module_annotations) != 1:
        raise AssertionError("Expected exactly one module!")
    module_annotations = all_module_annotations[0]
    assert module_annotations.stubs_path(Path("code")) == Path(f"code/{PATH}i")
    return module_annotations


def _assert_stubs_equal(actual: str, expected: str) -> None:
    actual = actual.strip()
    expected = textwrap.dedent(expected.rstrip())
    if actual != expected:
        print(f"---\nactual\n---\n{actual}")
        print(f"---\nexpected\n---\n{expected}")
        raise AssertionError("Stubs not as expected, see stdout")


class StubGenerationTest(unittest.TestCase):
    def _assert_stubs(
        self,
        data: dict[str, object],
        expected: str,
        complete_only: bool = False,
    ) -> None:
        module_annotations = _create_test_module_annotations(
            data=data,
            complete_only=complete_only,
        )
        actual = module_annotations.to_stubs()
        _assert_stubs_equal(actual, expected)

    def test_stubs_defines(self) -> None:
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.Test.ret_int",
                        "parent": "test.Test",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                ]
            },
            """\
            class Test:
                @classmethod
                def ret_int(self) -> int: ...
            """,
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.returns_int",
                        "parent": None,
                        "parameters": [],
                        "decorators": ["staticmethod"],
                        "async": True,
                    }
                ]
            },
            """\
            @staticmethod
            async def returns_int() -> int: ...
            """,
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "annotation": None, "value": "7"},
                            {"name": "x", "annotation": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def with_params(y=7, x: int = 5) -> int: ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "str",
                        "name": "test.returns_string",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def returns_string() -> str: ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "bool",
                        "name": "test.returns_bool",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def returns_bool() -> bool: ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "float",
                        "name": "test.returns_float",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def returns_float() -> float: ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "name": "test.missing_param_test",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": "5"}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def missing_param_test(x: int = 5): ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "float",
                        "name": "test.some_fun.another_fun",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def another_fun() -> float: ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "ret_int",
                        "parent": "test.Test.Test2",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                ]
            },
            "",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "typing.Union[int, str]",
                        "name": "ret_union",
                        "parent": "test.Test.Test2",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                ]
            },
            "",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "typing.Union[int, str]",
                        "name": "ret_union",
                        "parent": "test.Test.Test2",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": ["classmethod"],
                        "async": False,
                    },
                    {
                        "return": "typing.Dict[int, str]",
                        "name": "ret_dict",
                        "parent": "test.Test",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    },
                ],
            },
            """\
            from typing import Dict


            class Test:
                def ret_dict(self) -> Dict[int, str]: ...
            """,
        )
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "b",
                        "parent": "test.Test",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    },
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "a",
                        "parent": "test.Test",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    },
                ],
            },
            """\
            from typing import Dict, Union


            class Test:
                def b(self) -> Union[Dict[str, int], str]: ...
                def a(self) -> Union[Dict[str, int], str]: ...
            """,
        )

    def test_stubs_attributes_and_globals(self) -> None:
        self._assert_stubs(
            {
                "globals": [{"annotation": "int", "name": "global", "parent": None}],
            },
            """\
            global: int = ...
            """,
        )

        self._assert_stubs(
            {
                "attributes": [
                    {
                        "annotation": "int",
                        "name": "attribute_name",
                        "parent": "test.Test",
                    }
                ],
            },
            """\
            class Test:
                attribute_name: int = ...
            """,
        )

    def test_stubs_with_pathlike(self) -> None:
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "Union[PathLike[bytes],"
                        + " PathLike[str], bytes, str]",
                        "name": "test.bar",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def bar(x: int) -> Union['os.PathLike[bytes]',"
            + " 'os.PathLike[str]', bytes, str]: ...",
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "PathLike[str]",
                        "name": "test.bar",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def bar(x: int) -> 'os.PathLike[str]': ...",
        )
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "os.PathLike[str]",
                        "name": "test.bar",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def bar(x: int) -> os.PathLike[str]: ...",
        )
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "typing.Union[os.PathLike[str]]",
                        "name": "test.bar",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            """\
            from typing import Union


            def bar(x: int) -> Union[os.PathLike[str]]: ...
            """,
        )
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "PathLike[Variable[typing.AnyStr <:"
                        + " [str, bytes]]]",
                        "name": "test.bar",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            """\
            from typing import AnyStr


            def bar(x: int) -> PathLike[Variable[AnyStr <: [str, bytes]]]: ...
            """,
        )

    def test_stubs_complete_only(self) -> None:
        """
        Make sure we correctly filter out incomplete annotations when desired
        """
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "annotation": None, "value": "7"},
                            {"name": "x", "annotation": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "",
            complete_only=True,
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.complete_only",
                        "parent": None,
                        "parameters": [
                            {"name": "x", "annotation": "int", "value": "5"}
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def complete_only(x: int = 5) -> int: ...",
            complete_only=True,
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "annotation": "int", "value": "7"},
                            {"name": "x", "annotation": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ],
            },
            "def with_params(y: int = 7, x: int = 5) -> int: ...",
            complete_only=True,
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "int",
                        "name": "test.Test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "annotation": None, "value": "7"},
                            {"name": "x", "annotation": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "",
            complete_only=True,
        )

    def test_stubs_no_typing_import(self) -> None:
        """
        Make sure we don't spuriously import from typing

        NOTE: This logic is almost certainly incomplete - if another function
        in the same module used typing.Union, we would produce incorrect stubs.

        We should determine whether it is truly necessary to import from typing,
        because doing it correctly in edge cases is nontrivial.
        """
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "Union[int, str]",
                        "name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "annotation": None, "value": "7"},
                            {
                                "name": "x",
                                "annotation": "typing.List[int]",
                                "value": "[5]",
                            },
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            """\
            from typing import List


            def with_params(y=7, x: List[int] = [5]) -> Union[int, str]: ...
            """,
        )
