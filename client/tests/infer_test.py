# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import textwrap
import unittest
from unittest.mock import MagicMock, patch

from .. import commands, configuration, log
from ..error import Error
from ..infer import (
    FieldStub,
    FunctionStub,
    Infer,
    StubFile,
    _relativize_access,
    dequalify,
    main,
)


def build_json(inference):
    return {
        "line": 0,
        "column": 0,
        "code": 0,
        "name": "name",
        "path": "test.py",
        "description": "",
        "inference": inference,
    }


class HelperTest(unittest.TestCase):
    def test_dequalify(self) -> None:
        self.assertEqual(dequalify("typing.List"), "List")
        self.assertEqual(
            dequalify("typing.Union[typing.List[int]]"), "Union[List[int]]"
        )

    def test__relativize_access(self) -> None:
        self.assertEqual(
            _relativize_access(
                "tools.pyre.client.infer.Stub", "tools/pyre/client/infer.py"
            ),
            ["Stub"],
        )
        self.assertEqual(
            _relativize_access("function_name", "tools/pyre/client/infer.py"),
            ["function_name"],
        )
        self.assertEqual(
            _relativize_access(
                "tools.pyre.client.infer.toplevel_function",
                "tools/pyre/client/infer.py",
            ),
            ["toplevel_function"],
        )
        self.assertEqual(
            _relativize_access(
                "tools.pyre.client.infer.Class.function", "tools/pyre/client/infer.py"
            ),
            ["Class", "function"],
        )
        self.assertEqual(
            _relativize_access(
                "tools.pyre.client.function", "tools/pyre/client/__init__.py"
            ),
            ["function"],
        )


class PyreTest(unittest.TestCase):
    def assert_imports(self, error_json, expected_imports) -> None:
        error = Error(**error_json)
        stub = None
        if FunctionStub.is_instance(error.inference):
            stub = FunctionStub(error.inference)
        elif FieldStub.is_instance(error.inference):
            stub = FieldStub(error.inference)
        assert stub is not None
        self.assertEqual(sorted(list(stub.get_typing_imports())), expected_imports)

    def test_get_typing_imports(self) -> None:
        self.assert_imports(
            build_json(
                {
                    "annotation": "typing.Union[int, str]",
                    "function_name": "ret_int",
                    "parent": "test.Test",
                    "parameters": [{"name": "self", "type": None, "value": None}],
                    "decorators": ["classmethod"],
                    "async": False,
                }
            ),
            ["Union"],
        )

        self.assert_imports(
            build_json(
                {
                    "annotation": "typing.Union[int, str]",
                    "function_name": "ret_int",
                    "parent": "test.Test",
                    "parameters": [
                        {
                            "name": "self",
                            "type": "typing.List[typing.Union[int, str]]",
                            "value": None,
                        }
                    ],
                    "decorators": ["classmethod"],
                    "async": False,
                }
            ),
            ["List", "Union"],
        )

    def assert_stub(self, error_jsons, expected, full_only: bool = False) -> None:
        errors = [Error(**error_json) for error_json in error_jsons]
        self.assertEqual(
            StubFile(errors, full_only=full_only).to_string().strip(),
            textwrap.dedent(expected.rstrip()),
        )

    def test_stubs(self) -> None:
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "ret_int",
                        "parent": "test.Test",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                )
            ],
            """\
            class Test:
                @classmethod
                def ret_int(self) -> int: ...
            """,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "test.returns_int",
                        "parent": None,
                        "parameters": [],
                        "decorators": ["staticmethod"],
                        "async": True,
                    }
                )
            ],
            """\
            @staticmethod
            async def returns_int() -> int: ...
            """,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {"name": "x", "type": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def with_params(y=7, x: int = 5) -> int: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "str",
                        "function_name": "test.returns_string",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def returns_string() -> str: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "bool",
                        "function_name": "test.returns_bool",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def returns_bool() -> bool: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "float",
                        "function_name": "test.returns_float",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def returns_float() -> float: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "function_name": "test.missing_param_test",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": "5"}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def missing_param_test(x: int = 5): ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "float",
                        "function_name": "test.some_fun.another_fun",
                        "parent": None,
                        "parameters": [],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def another_fun() -> float: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "ret_int",
                        "parent": "test.Test.Test2",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                )
            ],
            "",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {"name": "x", "type": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "",
            full_only=True,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "test.full_only",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": "5"}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def full_only(x: int = 5) -> int: ...",
            full_only=True,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {"name": "x", "type": None, "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ),
                build_json(
                    {
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {"name": "x", "type": "int", "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ),
                build_json(
                    {
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": "int", "value": "7"},
                            {"name": "x", "type": None, "value": "5"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ),
            ],
            "def with_params(y: int = 7, x: int = 5) -> int: ...",
            full_only=True,
        )

    def test_field_stubs(self) -> None:
        self.assert_stub(
            [build_json({"annotation": "int", "field_name": "global", "parent": None})],
            """\
            global: int = ...
            """,
        )
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "field_name": "field_name",
                        "parent": "test.Test",
                    }
                )
            ],
            """\
            class Test:
                field_name: int = ...
            """,
        )

    def test_import_from_typing(self) -> None:
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "typing.Union[int, str]",
                        "function_name": "ret_int",
                        "parent": "test.Test",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                )
            ],
            """\
            from typing import Union


            class Test:
                @classmethod
                def ret_int(self) -> Union[int, str]: ...
            """,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "typing.Union[int, str]",
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {"name": "x", "type": "typing.List[int]", "value": "[5]"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            """\
            from typing import List, Union

            def with_params(y=7, x: List[int] = [5]) -> Union[int, str]: ...
            """,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "Union[int, str]",
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {"name": "x", "type": "typing.List[int]", "value": "[5]"},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "from typing import List\n\n"
            + "def with_params(y=7, x: List[int] = [5]) -> Union[int, str]: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "typing.Union[typing.List[int], typing.Any]",
                        "function_name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "type": None, "value": "7"},
                            {
                                "name": "x",
                                "type": "typing.Dict[int, int]",
                                "value": "5",
                            },
                        ],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            """\
            from typing import Any, Dict, List, Union

            def with_params(y=7, x: Dict[int, int] = 5) -> Union[List[int], Any]: ...
            """,
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "typing.Union[typing.List[int], typing.Any]",
                        "field_name": "global",
                        "parent": None,
                    }
                )
            ],
            """\
            from typing import Any, List, Union

            global: Union[List[int], Any] = ...
            """,
        )


def mock_arguments() -> MagicMock:
    arguments = MagicMock()

    arguments.debug = False
    arguments.sequential = False
    arguments.show_error_traces = False
    arguments.verbose = False
    arguments.show_parse_errors = False
    arguments.local_configuration = None
    arguments.logging_sections = None
    arguments.current_directory = "."

    return arguments


def mock_configuration() -> MagicMock:
    configuration = MagicMock()
    configuration.get_search_path = MagicMock()
    configuration.get_typeshed = MagicMock()
    return configuration


class InferTest(unittest.TestCase):
    @patch("json.loads", return_value=[])
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_infer(self, directories_to_analyze, json_loads) -> None:
        arguments = mock_arguments()
        arguments.recursive = False
        arguments.strict = False

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]

        with patch.object(commands.Command, "_call_client") as call_client:
            command = Infer(arguments, configuration, analysis_directory=".")
            self.assertEquals(
                command._flags(),
                [
                    "-show-error-traces",
                    "-project-root",
                    ".",
                    "-infer",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.recursive = True

            command = Infer(arguments, configuration, analysis_directory=".")
            self.assertEquals(
                command._flags(),
                [
                    "-show-error-traces",
                    "-project-root",
                    ".",
                    "-infer",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                    "-recursive-infer",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch.object(log, "initialize")
    @patch.object(log, "cleanup")
    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "validate")
    def test_main(self, validate, read, log_cleanup, log_initialize) -> None:
        with patch.object(sys, "argv", ["infer", "--target", "."]), patch.object(
            Infer, "run"
        ):
            self.assertEqual(main(), 2)
