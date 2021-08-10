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
from ...commands.infer import (
    _create_module_annotations,
    sanitize_annotation,
    AnnotateModuleInPlace,
    Infer,
    ModuleAnnotations,
    RawInferOutput,
    RawInferOutputDict,
    StubGenerationOptions,
    TypeAnnotation,
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
    data: dict[str, object] | None, options: StubGenerationOptions
) -> ModuleAnnotations:
    all_module_annotations = _create_module_annotations(
        infer_output=_raw_infer_output(data=data),
        sanitize_path=lambda path: path,
        options=options,
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


class InferUtilsTestSuite(unittest.TestCase):
    def test_sanitize_annotation__dequalify_typing(self) -> None:
        self.assertEqual(sanitize_annotation("typing.List"), "List")
        self.assertEqual(
            sanitize_annotation("typing.Union[typing.List[int]]"),
            "Union[List[int]]",
        )
        self.assertEqual(
            sanitize_annotation("typing.List", dequalify_typing=False), "typing.List"
        )
        self.assertEqual(
            sanitize_annotation(
                "typing.Union[typing.List[int]]", dequalify_typing=False
            ),
            "typing.Union[typing.List[int]]",
        )

    def test_sanitize_annotation__dequalify_all(self) -> None:
        self.assertEqual(sanitize_annotation("int", dequalify_all=True), "int")
        self.assertEqual(
            sanitize_annotation("sql.Column[int]", dequalify_all=True), "Column[int]"
        )
        self.assertEqual(
            sanitize_annotation(
                "sqlalchemy.sql.schema.Column[int]", dequalify_all=True
            ),
            "Column[int]",
        )
        self.assertEqual(
            sanitize_annotation(
                "sqlalchemy.sql.schema.Column[Optional[int]]", dequalify_all=True
            ),
            "Column[Optional[int]]",
        )

    def test_sanitize_annotation__fix_PathLike(self) -> None:
        self.assertEqual(
            sanitize_annotation("PathLike[str]"),
            "'os.PathLike[str]'",
        )
        self.assertEqual(
            sanitize_annotation("os.PathLike[str]"),
            "os.PathLike[str]",
        )
        self.assertEqual(
            sanitize_annotation("Union[PathLike[bytes], PathLike[str], str]"),
            "Union['os.PathLike[bytes]', 'os.PathLike[str]', str]",
        )
        # The libcst code throws an exception on this annotation because it is
        # invalid; this unit test verifies that we try/catch as expected rather than
        # crashing infer.
        self.assertEqual(
            sanitize_annotation("PathLike[Variable[AnyStr <: [str, bytes]]]"),
            "PathLike[Variable[AnyStr <: [str, bytes]]]",
        )


class TypeAnnotationTest(unittest.TestCase):

    no_dequalify_options: StubGenerationOptions = StubGenerationOptions(
        annotate_attributes=False,
        use_future_annotations=True,
        dequalify=False,
    )
    dequalify_options: StubGenerationOptions = StubGenerationOptions(
        annotate_attributes=False,
        use_future_annotations=True,
        dequalify=True,
    )

    def test_raises_on_invalid_type(self) -> None:
        self.assertRaises(
            ValueError, TypeAnnotation.from_raw, 0, self.no_dequalify_options
        )

    def test_sanitized(self) -> None:
        actual = TypeAnnotation.from_raw(
            "foo.Foo[int]", options=self.no_dequalify_options
        )
        self.assertEqual(actual.sanitized(), "foo.Foo[int]")
        self.assertEqual(actual.sanitized(prefix=": "), ": foo.Foo[int]")

        actual = TypeAnnotation.from_raw("foo.Foo[int]", options=self.dequalify_options)
        self.assertEqual(actual.sanitized(), "Foo[int]")
        self.assertEqual(actual.sanitized(prefix=": "), ": Foo[int]")


class AnnotateModuleInPlaceTest(unittest.TestCase):
    def run_test_case(self, stub: str, code: str, expected: str) -> None:
        stub = textwrap.dedent(stub.strip())
        code = textwrap.dedent(code.strip())
        actual = AnnotateModuleInPlace._annotated_code(stub, code)
        if actual.strip() != expected.strip():
            print("Mismatch in code generated by AnnotateModuleInPlace!")
            print(f"expected:\n{expected}\n")
            print(f"actual:\n{actual}\n")
            raise AssertionError("AnnotateModuleInPlace mismatch")

    def test_simple_merge(self) -> None:
        self.run_test_case(
            stub="""
            def f(x: int) -> float: ...
            """,
            code="""
            def f(x):
                return x * 1.5
            """,
            expected="""
            def f(x: int) -> float:
                return x * 1.5
            """,
        )


class StubGenerationTest(unittest.TestCase):
    def _assert_stubs(
        self,
        data: dict[str, object],
        expected: str,
        annotate_attributes: bool = False,
        use_future_annotations: bool = False,
    ) -> None:
        module_annotations = _create_test_module_annotations(
            data=data,
            options=StubGenerationOptions(
                annotate_attributes=annotate_attributes,
                use_future_annotations=use_future_annotations,
                dequalify=False,
            ),
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
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "f",
                        "parent": "test.TestA",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None}
                        ],
                        "decorators": [],
                        "async": False,
                    },
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "f",
                        "parent": "test.TestB",
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

            class TestA:
                def f(self) -> Union[Dict[str, int], str]: ...

            class TestB:
                def f(self) -> Union[Dict[str, int], str]: ...
            """,
        )

    def test_stubs_globals(self) -> None:
        self._assert_stubs(
            {
                "globals": [{"annotation": "int", "name": "global", "parent": None}],
            },
            """\
            global: int = ...
            """,
        )

    def test_stubs_attributes(self) -> None:
        self._assert_stubs(
            {
                "attributes": [
                    {
                        "annotation": "int",
                        "name": "attribute_name",
                        "parent": "test.test",
                    }
                ],
            },
            """\
            class test:
                attribute_name: int = ...
            """,
            annotate_attributes=True,
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
            """,
            annotate_attributes=False,
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

    def test_stubs_use_future_annotations(self) -> None:
        """
        Test
        """
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "Test",
                        "name": "test.Test.f",
                        "parent": "test.Test",
                        "parameters": [
                            {"name": "self", "annotation": None, "value": None},
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ],
            },
            """\
            from __future__ import annotations

            class Test:
                def f(self) -> Test: ...
            """,
            use_future_annotations=True,
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


class InferTest(unittest.TestCase):
    @staticmethod
    def mock_configuration() -> MagicMock:
        configuration = mock_configuration()
        configuration.search_path = ["path1", "path2"]
        configuration.get_typeshed = lambda: "stub"
        configuration.logger = None
        configuration.strict = False
        configuration.get_existent_ignore_infer_paths = lambda: []
        return configuration

    def test_check_working_directory(self) -> None:
        configuration = self.mock_configuration()
        self.assertRaises(
            ValueError,
            lambda: Infer._check_working_directory(
                original_directory="/some/other/directory", configuration=configuration
            ),
        )
        # no error expected
        Infer._check_working_directory(
            original_directory=configuration.project_root, configuration=configuration
        )
        Infer._check_working_directory(
            original_directory=configuration.local_root, configuration=configuration
        )

    def test_infer_should_annotate_in_place(self) -> None:
        def check_should_annotate_in_place(
            paths_to_modify: set[Path],
            path: Path,
            expected: bool,
        ) -> None:
            arguments = mock_arguments()
            configuration = self.mock_configuration()
            original_directory = configuration.project_root
            configuration.get_typeshed.return_value = "stub"
            analysis_directory = AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            )
            infer = Infer(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                print_only=True,
                in_place=False,
                paths_to_modify=paths_to_modify,
                annotate_from_existing_stubs=False,
                debug_infer=False,
                read_stdin=False,
                dequalify=False,
                interprocedural=False,
                annotate_attributes=False,
                use_future_annotations=True,
            )
            self.assertEqual(expected, infer._should_annotate_in_place(path))

        check_should_annotate_in_place(
            paths_to_modify=set(),
            path=Path("any/path/will/do"),
            expected=True,
        )
        check_should_annotate_in_place(
            paths_to_modify={Path("some/directory")},
            path=Path("some/directory/inner/file.py"),
            expected=True,
        )
        check_should_annotate_in_place(
            paths_to_modify={Path("some/directory")},
            path=Path("other/directory/inner/file.py"),
            expected=False,
        )
        check_should_annotate_in_place(
            paths_to_modify={Path("some/file.py")},
            path=Path("some/file.py"),
            expected=True,
        )
        check_should_annotate_in_place(
            paths_to_modify={Path("some/file.py")},
            path=Path("some/other_file.py"),
            expected=False,
        )

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
    def test_infer_commandline_calls(
        self,
        analysis_directory_get_filter_roots: MagicMock,
        json_loads: MagicMock,
        find_global_and_local_root: MagicMock,
    ) -> None:
        arguments = mock_arguments()
        configuration = self.mock_configuration()
        original_directory = configuration.project_root
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
                in_place=False,
                paths_to_modify=set(),
                annotate_from_existing_stubs=False,
                debug_infer=False,
                read_stdin=False,
                dequalify=False,
                interprocedural=False,
                annotate_attributes=False,
                use_future_annotations=True,
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
                    "-infer-mode",
                    "local",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=Infer.NAME)

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
                in_place=False,
                paths_to_modify=set(),
                annotate_from_existing_stubs=False,
                debug_infer=False,
                read_stdin=False,
                dequalify=False,
                interprocedural=True,
                annotate_attributes=False,
                use_future_annotations=True,
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
                    "-infer-mode",
                    "interprocedural",
                    "-filter-directories",
                    "filter_root_1;filter_root_2",
                    "-ignore-infer",
                    "path1.py;path2.py",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=Infer.NAME)

        with patch.object(commands.Command, "_call_client") as call_client:
            with patch.object(sys.stdin, "read", return_value=""):
                command = Infer(
                    arguments,
                    original_directory,
                    configuration=configuration,
                    analysis_directory=analysis_directory,
                    print_only=True,
                    in_place=False,
                    paths_to_modify=set(),
                    annotate_from_existing_stubs=False,
                    debug_infer=False,
                    read_stdin=True,
                    dequalify=False,
                    interprocedural=False,
                    annotate_attributes=False,
                    use_future_annotations=True,
                )
                command.run()
                call_client.assert_not_called()
