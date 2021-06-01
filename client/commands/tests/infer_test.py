# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import sys
import textwrap
import unittest
from pathlib import Path
from typing import Any, Dict, Union
from unittest.mock import MagicMock, patch

import libcst as cst

from ... import commands, find_directories, configuration as configuration_module
from ...analysis_directory import AnalysisDirectory
from ...commands import infer
from ...commands.infer import (
    FieldStub,
    FunctionStub,
    Infer,
    StubFile,
    _existing_annotations_as_errors,
    _relativize_access,
    dequalify_and_fix_pathlike,
    generate_stub_files,
)
from ...error import LegacyError
from .command_test import (
    mock_arguments,
    mock_configuration as general_mock_configuration,
)


def build_json(inference) -> Dict[str, Union[int, str]]:
    return {
        "line": 0,
        "column": 0,
        "stop_line": 1,
        "stop_column": 1,
        "code": 0,
        "name": "name",
        "path": "test.py",
        "description": "",
        "inference": inference,
    }


class HelperTest(unittest.TestCase):
    def test_dequalify_and_fix_pathlike(self) -> None:
        self.assertEqual(dequalify_and_fix_pathlike("typing.List"), "List")
        self.assertEqual(
            dequalify_and_fix_pathlike("typing.Union[typing.List[int]]"),
            "Union[List[int]]",
        )

    def test__relativize_access(self) -> None:
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

    def test_existing_annotations_to_stubs(self) -> None:
        def assert_stub_equal(file_content, expected) -> None:
            module = cst.parse_module(textwrap.dedent(file_content))
            expected_stub = textwrap.dedent(expected.rstrip()) + "\n"
            errors = _existing_annotations_as_errors(
                {Path("example/module.py"): module}, "example"
            )
            stub_files = generate_stub_files(full_only=False, errors=errors)
            self.assertTrue(stub_files)
            self.assertEqual(stub_files[0].to_string(), expected_stub)

        # test function stubs
        assert_stub_equal(
            """
            def foo() -> int:
                return 1 + 1
            """,
            "def foo() -> int: ...",
        )

        # functions in classes
        assert_stub_equal(
            """
            class Foo:
                def bar(x: int) -> Union[int, str]:
                    return ""
            """,
            """
            class Foo:
                def bar(x: int) -> Union[int, str]: ...
            """,
        )

        # attributes in classes
        assert_stub_equal(
            """
            class Foo:
                def bar(x: int) -> Union[int, str]:
                    return ""
            """,
            """
            class Foo:
                def bar(x: int) -> Union[int, str]: ...
            """,
        )

        # with decorators
        assert_stub_equal(
            """
            @click
            def foo() -> int:
                return 1 + 1
            """,
            "@@click\n\ndef foo() -> int: ...",
        )

        # attributes
        assert_stub_equal(
            """
            x: int = 10
            """,
            "x: int = ...",
        )
        assert_stub_equal(
            """
            class Foo:
                x: int = 10
            """,
            """
            class Foo:
                x: int = ...
            """,
        )


class PyreTest(unittest.TestCase):
    def assert_imports(self, error_json: Dict[str, Any], expected_imports) -> None:
        error = LegacyError.create(error_json)
        stub = None
        if FunctionStub.is_instance(error.inference):
            stub = FunctionStub(error.inference)
        elif FieldStub.is_instance(error.inference):
            stub = FieldStub(error.inference)
        assert stub is not None
        self.assertEqual(sorted(stub.get_typing_imports()), expected_imports)

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
        errors = [LegacyError.create(error_json) for error_json in error_jsons]
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
                        "annotation": "typing.Union[int, str]",
                        "function_name": "ret_union",
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
                        "annotation": "typing.Union[int, str]",
                        "function_name": "ret_union",
                        "parent": "test.Test.Test2",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": ["classmethod"],
                        "async": False,
                    }
                ),
                build_json(
                    {
                        "annotation": "typing.Dict[int, str]",
                        "function_name": "ret_dict",
                        "parent": "test.Test",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                ),
            ],
            """\
            from typing import Dict


            class Test:
                def ret_dict(self) -> Dict[int, str]: ...
            """,
        )
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "typing.Union[typing.Dict[str, int], str]",
                        "function_name": "b",
                        "parent": "test.Test",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                ),
                build_json(
                    {
                        "annotation": "typing.Union[typing.Dict[str, int], str]",
                        "function_name": "a",
                        "parent": "test.Test",
                        "parameters": [{"name": "self", "type": None, "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                ),
            ],
            """\
            from typing import Dict, Union


            class Test:
                def b(self) -> Union[Dict[str, int], str]: ...
                def a(self) -> Union[Dict[str, int], str]: ...
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

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "Union[PathLike[bytes],"
                        + " PathLike[str], bytes, str]",
                        "function_name": "test.bar",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def bar(x: int) -> Union['os.PathLike[bytes]',"
            + " 'os.PathLike[str]', bytes, str]: ...",
        )

        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "PathLike[str]",
                        "function_name": "test.bar",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def bar(x: int) -> 'os.PathLike[str]': ...",
        )
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "os.PathLike[str]",
                        "function_name": "test.bar",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            "def bar(x: int) -> os.PathLike[str]: ...",
        )
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "typing.Union[os.PathLike[str]]",
                        "function_name": "test.bar",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            """\
            from typing import Union

            def bar(x: int) -> Union[os.PathLike[str]]: ...
            """,
        )
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "PathLike[Variable[typing.AnyStr <:"
                        + " [str, bytes]]]",
                        "function_name": "test.bar",
                        "parent": None,
                        "parameters": [{"name": "x", "type": "int", "value": None}],
                        "decorators": [],
                        "async": False,
                    }
                )
            ],
            """\
            from typing import AnyStr

            def bar(x: int) -> PathLike[Variable[AnyStr <: [str, bytes]]]: ...
            """,
        )

    def test_field_stubs(self) -> None:
        self.assert_stub(
            [
                build_json(
                    {"annotation": "int", "attribute_name": "global", "parent": None}
                )
            ],
            """\
            global: int = ...
            """,
        )
        self.assert_stub(
            [
                build_json(
                    {
                        "annotation": "int",
                        "attribute_name": "attribute_name",
                        "parent": "test.Test",
                    }
                )
            ],
            """\
            class Test:
                attribute_name: int = ...
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
                        "attribute_name": "global",
                        "parent": None,
                    }
                )
            ],
            """\
            from typing import Any, List, Union

            global: Union[List[int], Any] = ...
            """,
        )


def mock_configuration() -> MagicMock:
    configuration = general_mock_configuration()
    configuration.search_path = ["path1", "path2"]
    configuration.get_typeshed = lambda: "stub"
    configuration.logger = None
    configuration.strict = False
    configuration.get_existent_ignore_infer_paths = lambda: []
    return configuration


class InferTest(unittest.TestCase):
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch.object(json, "loads", return_value={"errors": []})
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_infer(
        self, directories_to_analyze, json_loads, find_global_and_local_root
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"

        with patch.object(commands.Command, "_call_client") as call_client:
            command = Infer(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                print_errors=True,
                full_only=True,
                recursive=False,
                in_place=None,
                errors_from_stdin=False,
                annotate_from_existing_stubs=False,
                debug_infer=False,
                full_stub_paths=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-show-error-traces",
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
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Infer.NAME)

        with patch.object(commands.Command, "_call_client") as call_client:

            command = Infer(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                print_errors=True,
                full_only=True,
                recursive=False,
                in_place=None,
                errors_from_stdin=False,
                annotate_from_existing_stubs=False,
                debug_infer=False,
                full_stub_paths=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-show-error-traces",
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
                    analysis_directory=AnalysisDirectory(
                        configuration_module.SimpleSearchPathElement(".")
                    ),
                    print_errors=True,
                    full_only=True,
                    recursive=False,
                    in_place=None,
                    errors_from_stdin=True,
                    annotate_from_existing_stubs=False,
                    debug_infer=False,
                    full_stub_paths=None,
                )
                self.assertEqual(
                    command._flags(),
                    [
                        "-show-error-traces",
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
                    ],
                )
                command.run()
                call_client.assert_not_called()
        configuration.get_existent_ignore_infer_paths = lambda: ["path1.py", "path2.py"]
        with patch.object(commands.Command, "_call_client") as call_client:
            with patch.object(sys.stdin, "read", return_value=""):
                command = Infer(
                    arguments,
                    original_directory,
                    configuration=configuration,
                    analysis_directory=AnalysisDirectory(
                        configuration_module.SimpleSearchPathElement(".")
                    ),
                    print_errors=True,
                    full_only=True,
                    recursive=False,
                    in_place=None,
                    errors_from_stdin=True,
                    annotate_from_existing_stubs=False,
                    debug_infer=False,
                    full_stub_paths=None,
                )
                self.assertEqual(
                    command._flags(),
                    [
                        "-show-error-traces",
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
                        "-ignore-infer",
                        "path1.py;path2.py",
                    ],
                )
                command.run()
                call_client.assert_not_called()

    @patch.object(Path, "rglob")
    @patch.object(infer, "annotate_path")
    def test_annotate_from_existing_stubs_empty_in_place(
        self, annotate_path: MagicMock, recursive_glob: MagicMock
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        command = Infer(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            print_errors=True,
            full_only=True,
            recursive=False,
            in_place=None,
            errors_from_stdin=True,
            annotate_from_existing_stubs=False,
            debug_infer=False,
            full_stub_paths=None,
        )
        root = Path("/root/my-project")
        recursive_glob.return_value = [
            Path("/root/.pyre/my-project/types/foo/bar/baz.pyi")
        ]
        command._annotate_from_stubs(
            root,
            None,
            type_directory=Path("/root/.pyre/my-project/types"),
            in_place=[],
            debug_infer=False,
        )
        annotate_path.assert_called_once_with(
            "/root/.pyre/my-project/types/foo/bar/baz.pyi",
            "/root/my-project/foo/bar/baz.py",
            False,
        )

    @patch.object(Path, "rglob")
    @patch.object(infer, "annotate_path")
    def test_annotate_from_existing_stubs_in_place_directory(
        self, annotate_path: MagicMock, recursive_glob: MagicMock
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        command = Infer(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            print_errors=True,
            full_only=True,
            recursive=False,
            in_place=None,
            errors_from_stdin=True,
            annotate_from_existing_stubs=False,
            debug_infer=False,
            full_stub_paths=None,
        )
        root = Path("/root/my-project")
        recursive_glob.return_value = [
            Path("/root/.pyre/my-project/types/foo/bar/baz.pyi")
        ]

        command._annotate_from_stubs(
            root,
            None,
            type_directory=Path("/root/.pyre/my-project/types"),
            in_place=["foo/bar"],
            debug_infer=False,
        )
        annotate_path.assert_called_once_with(
            "/root/.pyre/my-project/types/foo/bar/baz.pyi",
            "/root/my-project/foo/bar/baz.py",
            False,
        )

    @patch.object(Path, "rglob")
    @patch.object(infer, "annotate_path")
    def test_annotate_from_existing_stubs_no_match(
        self, annotate_path: MagicMock, recursive_glob: MagicMock
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        command = Infer(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            print_errors=True,
            full_only=True,
            recursive=False,
            in_place=None,
            errors_from_stdin=True,
            annotate_from_existing_stubs=False,
            debug_infer=False,
            full_stub_paths=None,
        )
        root = Path("/root/my-project")
        recursive_glob.return_value = [
            Path("/root/.pyre/my-project/types/foo/bar/baz.pyi")
        ]
        command._annotate_from_stubs(
            root,
            None,
            type_directory=Path("/root/.pyre/my-project/types"),
            in_place=["some_other_directory"],
            debug_infer=False,
        )
        annotate_path.assert_not_called()

    @patch.object(Path, "rglob")
    @patch.object(infer, "annotate_path")
    def test_annotate_from_existing_stubs_relative_file_path(
        self, annotate_path: MagicMock, recursive_glob: MagicMock
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        command = Infer(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            print_errors=True,
            full_only=True,
            recursive=False,
            in_place=None,
            errors_from_stdin=True,
            annotate_from_existing_stubs=False,
            debug_infer=False,
            full_stub_paths=None,
        )
        root = Path("/root/my-project")
        recursive_glob.return_value = [
            Path("/root/.pyre/my-project/types/foo/bar/baz.pyi")
        ]
        command._annotate_from_stubs(
            root,
            None,
            type_directory=Path("/root/.pyre/my-project/types"),
            in_place=["foo/bar/baz.py"],
            debug_infer=False,
        )
        annotate_path.assert_called_once_with(
            "/root/.pyre/my-project/types/foo/bar/baz.pyi",
            "/root/my-project/foo/bar/baz.py",
            False,
        )

    @patch.object(Path, "rglob")
    @patch.object(infer, "annotate_path")
    def test_annotate_from_existing_stubs_relative_file_path_not_local_root(
        self, annotate_path: MagicMock, recursive_glob: MagicMock
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        command = Infer(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            print_errors=True,
            full_only=True,
            recursive=False,
            in_place=None,
            errors_from_stdin=True,
            annotate_from_existing_stubs=False,
            debug_infer=False,
            full_stub_paths=None,
        )
        root = Path("/root")
        recursive_glob.return_value = [Path("/root/.pyre/types/foo/bar/types/baz.pyi")]
        command._annotate_from_stubs(
            root,
            None,
            type_directory=Path("/root/.pyre/types"),
            in_place=["foo/bar/types/baz.py"],
            debug_infer=False,
        )
        annotate_path.assert_called_once_with(
            "/root/.pyre/types/foo/bar/types/baz.pyi",
            "/root/foo/bar/types/baz.py",
            False,
        )

    @patch.object(Path, "rglob")
    @patch.object(infer, "annotate_path")
    def test_annotate_from_existing_stubs_relative_local_root(
        self, annotate_path: MagicMock, recursive_glob: MagicMock
    ) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        command = Infer(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            print_errors=True,
            full_only=True,
            recursive=False,
            in_place=None,
            errors_from_stdin=True,
            annotate_from_existing_stubs=False,
            debug_infer=False,
            full_stub_paths=None,
        )
        root = Path("/root")
        recursive_glob.return_value = [
            Path("/root/.pyre/local-root/types/local-root/foo/bar/baz.pyi")
        ]
        command._annotate_from_stubs(
            root,
            None,
            type_directory=Path("/root/.pyre/local-root/types"),
            in_place=["local-root/foo/bar"],
            debug_infer=False,
        )
        annotate_path.assert_called_once_with(
            "/root/.pyre/local-root/types/local-root/foo/bar/baz.pyi",
            "/root/local-root/foo/bar/baz.py",
            False,
        )
