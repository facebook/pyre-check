# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import json
import sys
import textwrap
import unittest
from pathlib import Path
from typing import cast
from unittest.mock import MagicMock, patch

from ... import commands, find_directories, configuration as configuration_module
from ...analysis_directory import AnalysisDirectory
from ...commands.infer_v2 import (
    _create_module_annotations,
    RawInferOutput,
    RawInferOutputDict,
    ModuleAnnotations,
    Infer,
)
from .command_test import (
    mock_arguments,
    mock_configuration,
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
) -> ModuleAnnotations:
    all_module_annotations = _create_module_annotations(
        infer_output=_raw_infer_output(data=data),
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
    ) -> None:
        module_annotations = _create_test_module_annotations(
            data=data,
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


class InferV2Test(unittest.TestCase):
    @staticmethod
    def mock_configuration() -> MagicMock:
        configuration = mock_configuration()
        configuration.search_path = ["path1", "path2"]
        configuration.get_typeshed = lambda: "stub"
        configuration.logger = None
        configuration.strict = False
        configuration.get_existent_ignore_infer_paths = lambda: []
        return configuration

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch.object(
        json,
        "loads",
        return_value=[{"defines": [], "attributes": [], "globals": []}],
    )
    # pyre-ignore[56]
    @patch.object(AnalysisDirectory, "get_filter_roots", return_value=set())
    def test_infer_v2_commandline_calls(
        self,
        analysis_directory_get_filter_roots: MagicMock,
        json_loads: MagicMock,
        find_global_and_local_root: MagicMock,
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = self.mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )

        with patch.object(commands.Command, "_call_client") as call_client:
            command = Infer(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                print_only=True,
                in_place_paths=None,
                annotate_from_existing_stubs=False,
                debug_infer=False,
                read_stdin=False,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-use-v2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Infer.NAME)

        configuration.get_existent_ignore_infer_paths = lambda: ["path1.py", "path2.py"]
        # pyre-ignore[8]
        analysis_directory.get_filter_roots = lambda: {"filter_root_1", "filter_root_2"}
        with patch.object(commands.Command, "_call_client") as call_client:
            command = Infer(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                print_only=True,
                in_place_paths=[],
                annotate_from_existing_stubs=False,
                debug_infer=False,
                read_stdin=False,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-use-v2",
                    "-filter-directories",
                    "filter_root_1;filter_root_2",
                    "-ignore-infer",
                    "path1.py;path2.py",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Infer.NAME)

        with patch.object(commands.Command, "_call_client") as call_client:
            with patch.object(sys.stdin, "read", return_value=""):
                command = Infer(
                    arguments,
                    original_directory,
                    configuration=configuration,
                    analysis_directory=analysis_directory,
                    print_only=True,
                    in_place_paths=None,
                    annotate_from_existing_stubs=False,
                    debug_infer=False,
                    read_stdin=True,
                )
                command.run()
                call_client.assert_not_called()
