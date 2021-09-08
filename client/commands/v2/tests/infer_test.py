# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import tempfile
import textwrap
from pathlib import Path
from typing import Any, Iterable, Tuple, Dict, List, Optional, Set

import testslide

from .... import configuration, command_arguments
from ....tests import setup
from .. import backend_arguments
from ..infer import (
    Arguments,
    InferMode,
    create_infer_arguments,
    create_module_annotations,
    sanitize_annotation,
    should_annotate_in_place,
    RawAnnotationLocation,
    RawGlobalAnnotation,
    RawAttributeAnnotation,
    RawParameter,
    RawDefineAnnotation,
    RawInferOutput,
    RawInferOutputParsingError,
    StubGenerationOptions,
    TypeAnnotation,
    FunctionAnnotation,
    MethodAnnotation,
    GlobalAnnotation,
    AttributeAnnotation,
    ModuleAnnotations,
)


class ArgumentTest(testslide.TestCase):
    def test_serialize_arguments(self) -> None:
        def assert_serialized(
            arguments: Arguments, items: Iterable[Tuple[str, object]]
        ) -> None:
            serialized = arguments.serialize()
            for key, value in items:
                if key not in serialized:
                    self.fail(f"Cannot find key `{key}` in serialized arguments")
                else:
                    self.assertEqual(value, serialized[key])

        assert_serialized(
            Arguments(
                base_arguments=backend_arguments.BaseArguments(
                    log_path="/log",
                    global_root="/project",
                    source_paths=backend_arguments.SimpleSourcePath(
                        [configuration.SimpleSearchPathElement("source")]
                    ),
                ),
                ignore_infer=["/ignore"],
                infer_mode=InferMode.INTERPROCEDURAL,
                paths_to_modify={Path("/derp3.py")},
            ),
            [
                ("log_path", "/log"),
                ("global_root", "/project"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
                ("ignore_infer", ["/ignore"]),
                ("infer_mode", ["Interprocedural"]),
                ("paths_to_modify", ["/derp3.py"]),
            ],
        )


class InferTest(testslide.TestCase):
    def test_create_infer_arguments(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(
                root_path,
                [".pyre", "blocks", "ignores", "search", "local/src"],
            )
            setup.write_configuration_file(
                root_path,
                {
                    "ignore_all_errors": ["blocks", "nonexistent"],
                    "ignore_infer": ["ignores"],
                    "exclude": ["exclude"],
                    "extensions": [".ext"],
                    "workers": 42,
                    "search_path": ["search", "nonexistent"],
                },
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["src"]}, relative="local"
            )

            infer_configuration = configuration.create_configuration(
                command_arguments.CommandArguments(
                    local_configuration="local",
                    dot_pyre_directory=root_path / ".pyre",
                ),
                root_path,
            )

            self.assertEqual(
                create_infer_arguments(
                    infer_configuration,
                    command_arguments.InferArguments(
                        working_directory=Path("/some/directory"),
                        debug_infer=True,
                        sequential=False,
                        interprocedural=False,
                        paths_to_modify={Path("path/to/module.py")},
                    ),
                ),
                Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        log_path=str(root_path / ".pyre/local"),
                        global_root=str(root_path),
                        checked_directory_allowlist=[
                            str(root_path / "local/src"),
                        ],
                        checked_directory_blocklist=[str(root_path / "blocks")],
                        debug=True,
                        excludes=["exclude"],
                        extensions=[".ext"],
                        relative_local_root="local",
                        number_of_workers=42,
                        parallel=True,
                        python_version=infer_configuration.get_python_version(),
                        search_paths=[
                            configuration.SimpleSearchPathElement(
                                str(root_path / "search")
                            )
                        ],
                        source_paths=backend_arguments.SimpleSourcePath(
                            [
                                configuration.SimpleSearchPathElement(
                                    str(root_path / "local/src")
                                )
                            ]
                        ),
                    ),
                    ignore_infer=[str(root_path / "ignores")],
                    infer_mode=InferMode.LOCAL,
                    paths_to_modify={Path("path/to/module.py")},
                ),
            )

    def test_parse_raw_infer_output(self) -> None:
        def assert_parsed(input: Dict[str, object], expected: RawInferOutput) -> None:
            self.assertEqual(RawInferOutput.create_from_json(input), expected)

        def assert_not_parsed(input: str) -> None:
            with self.assertRaises(RawInferOutputParsingError):
                RawInferOutput.create_from_string(input)

        assert_not_parsed("")
        assert_not_parsed("[]")
        assert_not_parsed("42")
        assert_not_parsed('"abc"')

        assert_parsed({}, RawInferOutput())
        assert_parsed({"irrelevant": 42}, RawInferOutput())
        assert_parsed(
            {
                "globals": [
                    {
                        "name": "x",
                        "location": {"qualifier": "test", "path": "test.py", "line": 4},
                        "annotation": "int",
                    }
                ]
            },
            RawInferOutput(
                global_annotations=[
                    RawGlobalAnnotation(
                        name="x",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=4
                        ),
                        annotation="int",
                    )
                ]
            ),
        )
        assert_parsed(
            {
                "attributes": [
                    {
                        "parent": "Foo",
                        "name": "x",
                        "location": {"qualifier": "test", "path": "test.py", "line": 3},
                        "annotation": "int",
                    }
                ]
            },
            RawInferOutput(
                attribute_annotations=[
                    RawAttributeAnnotation(
                        parent="Foo",
                        name="x",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=3
                        ),
                        annotation="int",
                    )
                ]
            ),
        )
        assert_parsed(
            {
                "defines": [
                    {
                        "name": "test.foo",
                        "parent": None,
                        "return": None,
                        "parameters": [],
                        "decorators": [],
                        "location": {"qualifier": "test", "path": "test.py", "line": 1},
                        "async": False,
                    }
                ]
            },
            RawInferOutput(
                define_annotations=[
                    RawDefineAnnotation(
                        name="test.foo",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=1
                        ),
                        is_async=False,
                    )
                ]
            ),
        )
        assert_parsed(
            {
                "defines": [
                    {
                        "name": "test.Foo.foo",
                        "parent": "test.Foo",
                        "return": "int",
                        "parameters": [
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            },
                            {
                                "name": "x",
                                "annotation": "int",
                                "value": "42",
                                "index": 1,
                            },
                        ],
                        "decorators": ["derp"],
                        "location": {"qualifier": "test", "path": "test.py", "line": 1},
                        "async": True,
                    }
                ]
            },
            RawInferOutput(
                define_annotations=[
                    RawDefineAnnotation(
                        name="test.Foo.foo",
                        parent="test.Foo",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=1
                        ),
                        return_="int",
                        parameters=[
                            RawParameter(name="self", index=0),
                            RawParameter(
                                name="x", index=1, annotation="int", value="42"
                            ),
                        ],
                        decorators=["derp"],
                        is_async=True,
                    )
                ]
            ),
        )

    def test_raw_infer_output_split(self) -> None:
        def assert_split(given: Dict[str, object], expected: Dict[str, Any]) -> None:
            input_infer_output = RawInferOutput.create_from_json(given)
            expected_infer_output = {
                path: RawInferOutput.create_from_json(output)
                for (path, output) in expected.items()
            }
            self.assertDictEqual(
                input_infer_output.split_by_path(), expected_infer_output
            )

        foo_global0 = {
            "name": "x",
            "location": {"qualifier": "foo", "path": "foo.py", "line": 1},
            "annotation": "int",
        }
        foo_global1 = {
            "name": "y",
            "location": {"qualifier": "foo", "path": "foo.py", "line": 2},
            "annotation": "int",
        }
        bar_global0 = {
            "name": "z",
            "location": {"qualifier": "bar", "path": "bar.py", "line": 1},
            "annotation": "str",
        }
        bar_attribute0 = {
            "parent": "bar.Foo",
            "name": "a",
            "location": {"qualifier": "bar", "path": "bar.py", "line": 2},
            "annotation": "str",
        }
        baz_define0 = {
            "name": "baz.derp",
            "parent": None,
            "return": None,
            "parameters": [],
            "decorators": [],
            "location": {"qualifier": "baz", "path": "baz.py", "line": 1},
            "async": False,
        }

        assert_split({}, expected={})
        assert_split({"globals": [foo_global0]}, {"foo.py": {"globals": [foo_global0]}})
        assert_split(
            {
                "globals": [
                    foo_global0,
                    bar_global0,
                    foo_global1,
                ]
            },
            {
                "foo.py": {"globals": [foo_global0, foo_global1]},
                "bar.py": {"globals": [bar_global0]},
            },
        )
        assert_split(
            {
                "globals": [
                    foo_global0,
                ],
                "attributes": [bar_attribute0],
                "defines": [baz_define0],
            },
            {
                "foo.py": {
                    "globals": [
                        foo_global0,
                    ]
                },
                "bar.py": {"attributes": [bar_attribute0]},
                "baz.py": {"defines": [baz_define0]},
            },
        )

    def test_should_annotate_in_place(self) -> None:
        def assert_should_annotate_in_place(
            path: Path,
            paths_to_modify: Optional[Set[Path]],
            expected: bool,
        ) -> None:
            self.assertEqual(should_annotate_in_place(path, paths_to_modify), expected)

        assert_should_annotate_in_place(
            paths_to_modify=None,
            path=Path("any/path/will/do"),
            expected=True,
        )
        assert_should_annotate_in_place(
            paths_to_modify={Path("some/directory")},
            path=Path("some/directory/inner/file.py"),
            expected=True,
        )
        assert_should_annotate_in_place(
            paths_to_modify={Path("some/directory")},
            path=Path("other/directory/inner/file.py"),
            expected=False,
        )
        assert_should_annotate_in_place(
            paths_to_modify={Path("some/file.py")},
            path=Path("some/file.py"),
            expected=True,
        )
        assert_should_annotate_in_place(
            paths_to_modify={Path("some/file.py")},
            path=Path("some/other_file.py"),
            expected=False,
        )


@dataclasses.dataclass(frozen=True)
class ExpectedModuleAnnotationItem:
    path: str
    infer_output: RawInferOutput


class ModuleAnnotationTest(testslide.TestCase):
    def test_module_annotations_from_infer_output(self) -> None:
        def assert_result(
            path: str,
            infer_output: RawInferOutput,
            options: StubGenerationOptions,
            expected: ModuleAnnotations,
        ) -> None:
            self.assertEqual(
                ModuleAnnotations.from_infer_output(path, infer_output, options),
                expected,
            )

        default_path = "test.py"
        default_options = StubGenerationOptions()

        assert_result(
            path=default_path,
            infer_output=RawInferOutput(),
            options=default_options,
            expected=ModuleAnnotations(path=default_path, options=default_options),
        )

        assert_result(
            path=default_path,
            infer_output=RawInferOutput(
                define_annotations=[
                    RawDefineAnnotation(
                        name="test.Foo.foo",
                        parent="test.Foo",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=1
                        ),
                        return_="int",
                        decorators=["derp"],
                        is_async=True,
                    ),
                    RawDefineAnnotation(
                        name="test.bar",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=2
                        ),
                    ),
                ],
            ),
            options=default_options,
            expected=ModuleAnnotations(
                path=default_path,
                options=default_options,
                functions=[
                    FunctionAnnotation(
                        name="test.bar",
                        return_annotation=TypeAnnotation.from_raw(
                            None, options=default_options
                        ),
                        parameters=[],
                        decorators=[],
                        is_async=False,
                    )
                ],
                methods=[
                    MethodAnnotation(
                        parent="test.Foo",
                        name="test.Foo.foo",
                        return_annotation=TypeAnnotation.from_raw(
                            "int", options=default_options
                        ),
                        parameters=[],
                        decorators=["derp"],
                        is_async=True,
                    )
                ],
            ),
        )

        assert_result(
            path=default_path,
            infer_output=RawInferOutput(
                global_annotations=[
                    RawGlobalAnnotation(
                        name="x",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=3
                        ),
                        annotation="int",
                    )
                ],
            ),
            options=default_options,
            expected=ModuleAnnotations(
                path=default_path,
                options=default_options,
                globals_=[
                    GlobalAnnotation(
                        name="x",
                        annotation=TypeAnnotation.from_raw(
                            "int", options=default_options
                        ),
                    )
                ],
            ),
        )

        assert_result(
            path=default_path,
            infer_output=RawInferOutput(
                attribute_annotations=[
                    RawAttributeAnnotation(
                        parent="Foo",
                        name="x",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=3
                        ),
                        annotation="int",
                    )
                ],
            ),
            options=default_options,
            expected=ModuleAnnotations(
                path=default_path,
                options=default_options,
            ),
        )

        annotate_attribute_options = StubGenerationOptions(
            annotate_attributes=True,
        )
        assert_result(
            path=default_path,
            infer_output=RawInferOutput(
                attribute_annotations=[
                    RawAttributeAnnotation(
                        parent="Foo",
                        name="x",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=3
                        ),
                        annotation="int",
                    )
                ],
            ),
            options=annotate_attribute_options,
            expected=ModuleAnnotations(
                path=default_path,
                options=annotate_attribute_options,
                attributes=[
                    AttributeAnnotation(
                        parent="Foo",
                        name="x",
                        annotation=TypeAnnotation.from_raw(
                            "int", options=annotate_attribute_options
                        ),
                    )
                ],
            ),
        )

    def test_create_module_annotations(self) -> None:
        def assert_created(
            infer_output: RawInferOutput,
            base_path: Path,
            expected: List[ExpectedModuleAnnotationItem],
        ) -> None:
            default_options = StubGenerationOptions()
            self.assertCountEqual(
                create_module_annotations(infer_output, base_path, default_options),
                [
                    ModuleAnnotations.from_infer_output(
                        path=item.path,
                        infer_output=item.infer_output,
                        options=default_options,
                    )
                    for item in expected
                ],
            )

        foo_global0 = RawGlobalAnnotation(
            name="x",
            location=RawAnnotationLocation(
                qualifier="foo", path="/root/p0/foo.py", line=1
            ),
            annotation="int",
        )
        foo_global1 = RawGlobalAnnotation(
            name="y",
            location=RawAnnotationLocation(
                qualifier="foo", path="/root/p0/foo.py", line=2
            ),
            annotation="str",
        )
        bar_global0 = RawGlobalAnnotation(
            name="x",
            location=RawAnnotationLocation(
                qualifier="bar", path="/root/p1/bar.py", line=1
            ),
            annotation="int",
        )
        bar_attribute0 = RawAttributeAnnotation(
            parent="bar.Foo",
            name="a",
            location=RawAnnotationLocation(
                qualifier="bar", path="/root/p1/bar.py", line=2
            ),
            annotation="bool",
        )

        # Empty case
        assert_created(
            infer_output=RawInferOutput(), base_path=Path("irrelevant"), expected=[]
        )

        # Test proper splits by paths
        assert_created(
            infer_output=RawInferOutput(
                global_annotations=[
                    foo_global0,
                    bar_global0,
                    foo_global1,
                ],
                attribute_annotations=[
                    bar_attribute0,
                ],
            ),
            base_path=Path("/root"),
            expected=[
                ExpectedModuleAnnotationItem(
                    path="p0/foo.py",
                    infer_output=RawInferOutput(
                        global_annotations=[foo_global0, foo_global1],
                    ),
                ),
                ExpectedModuleAnnotationItem(
                    path="p1/bar.py",
                    infer_output=RawInferOutput(
                        global_annotations=[bar_global0],
                        attribute_annotations=[bar_attribute0],
                    ),
                ),
            ],
        )

        # Test relativization & path filtering
        assert_created(
            infer_output=RawInferOutput(
                global_annotations=[
                    foo_global0,
                    bar_global0,
                    foo_global1,
                ],
            ),
            base_path=Path("/root/p1"),
            expected=[
                ExpectedModuleAnnotationItem(
                    path="bar.py",
                    infer_output=RawInferOutput(
                        global_annotations=[bar_global0],
                    ),
                )
            ],
        )

    def test_module_annotation_stubs_path(self) -> None:
        self.assertEqual(
            ModuleAnnotations(
                path="derp.py",
                options=StubGenerationOptions(),
            ).stubs_path(Path("/root")),
            Path("/root/derp.pyi"),
        )


def _assert_stubs_equal(actual: str, expected: str) -> None:
    actual = actual.strip()
    expected = textwrap.dedent(expected.rstrip())
    if actual != expected:
        print(f"---\nactual\n---\n{actual}")
        print(f"---\nexpected\n---\n{expected}")
        raise AssertionError("Stubs not as expected, see stdout")


class InferUtilsTestSuite(testslide.TestCase):
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


class TypeAnnotationTest(testslide.TestCase):
    def test_sanitized(self) -> None:
        actual = TypeAnnotation.from_raw(
            "foo.Foo[int]",
            options=StubGenerationOptions(),
        )
        self.assertEqual(actual.sanitized(), "foo.Foo[int]")
        self.assertEqual(actual.sanitized(prefix=": "), ": foo.Foo[int]")

        actual = TypeAnnotation.from_raw(
            "foo.Foo[int]",
            options=StubGenerationOptions(quote_annotations=True),
        )
        self.assertEqual(actual.sanitized(), '"foo.Foo[int]"')
        self.assertEqual(actual.sanitized(prefix=": "), ': "foo.Foo[int]"')

        actual = TypeAnnotation.from_raw(
            "foo.Foo[int]",
            options=StubGenerationOptions(dequalify=True),
        )
        self.assertEqual(actual.sanitized(), "Foo[int]")
        self.assertEqual(actual.sanitized(prefix=": "), ": Foo[int]")

    def test_typing_imports(self) -> None:
        actual = TypeAnnotation.from_raw(
            "typing.Union[int, typing.Optional[str]]",
            options=StubGenerationOptions(),
        )
        self.assertEqual(actual.typing_imports(), {"Union", "Optional"})

        actual = TypeAnnotation.from_raw(
            "typing.Union[int, typing.Optional[str]]",
            options=StubGenerationOptions(quote_annotations=True),
        )
        self.assertEqual(actual.typing_imports(), set())


class StubGenerationTest(testslide.TestCase):
    def _assert_stubs(
        self,
        data: Dict[str, Any],
        expected: str,
        annotate_attributes: bool = False,
        use_future_annotations: bool = False,
        quote_annotations: bool = False,
    ) -> None:
        test_path = "/root/test.py"
        infer_output = RawInferOutput.create_from_json(
            {
                category: [
                    {
                        "location": {
                            "path": test_path,
                            "qualifier": "test",
                            "line": 1,
                        },
                        **value,
                    }
                    for value in values
                ]
                for category, values in data.items()
            }
        )
        module_annotations = create_module_annotations(
            infer_output=infer_output,
            base_path=Path("/root"),
            options=StubGenerationOptions(
                annotate_attributes=annotate_attributes,
                use_future_annotations=use_future_annotations,
                quote_annotations=quote_annotations,
            ),
        )
        if len(module_annotations) != 1:
            raise AssertionError("Expected exactly one module!")
        module_annotation = module_annotations[0]
        actual = module_annotation.to_stubs()
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
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
                            {"name": "y", "annotation": None, "value": "7", "index": 0},
                            {
                                "name": "x",
                                "annotation": "int",
                                "value": "5",
                                "index": 1,
                            },
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
                            {"name": "x", "annotation": "int", "value": "5", "index": 0}
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
                        ],
                        "decorators": ["classmethod"],
                        "async": False,
                    },
                    {
                        "return": "typing.Dict[int, str]",
                        "name": "ret_dict",
                        "parent": "test.Test",
                        "parameters": [
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
                        ],
                        "decorators": [],
                        "async": False,
                    },
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "a",
                        "parent": "test.Test",
                        "parameters": [
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
                        ],
                        "decorators": [],
                        "async": False,
                    },
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "f",
                        "parent": "test.TestB",
                        "parameters": [
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            }
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
                            {"name": "y", "annotation": None, "value": "7", "index": 0},
                            {
                                "name": "x",
                                "annotation": "typing.List[int]",
                                "value": "[5]",
                                "index": 1,
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

    def test_stubs_quote(self) -> None:
        """
        Test generating stubs with quoted annotations
        """
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "Union[int, str]",
                        "name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {"name": "y", "annotation": None, "value": "7", "index": 0},
                            {
                                "name": "x",
                                "annotation": "typing.List[int]",
                                "value": "[5]",
                                "index": 1,
                            },
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            """\
            def with_params(y=7, x: "typing.List[int]" = [5]) -> "Union[int, str]": ...
            """,
            quote_annotations=True,
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
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            },
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
                            {
                                "name": "x",
                                "annotation": "int",
                                "value": None,
                                "index": 0,
                            }
                        ],
                        "decorators": [],
                        "async": False,
                    }
                ]
            },
            "def bar(x: int) -> Union['os.PathLike[bytes]',"
            + " 'os.PathLike[str]', bytes, str]: ...",
        )
