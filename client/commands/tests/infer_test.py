# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import tempfile
import textwrap
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple

import testslide

from ... import command_arguments, configuration
from ...configuration import search_path
from ...tests import setup
from .. import backend_arguments, frontend_configuration
from ..infer import (
    AnnotateModuleInPlace,
    Arguments,
    AttributeAnnotation,
    create_infer_arguments,
    create_module_annotations,
    FunctionAnnotation,
    GlobalAnnotation,
    MethodAnnotation,
    ModuleAnnotations,
    RawAnnotationLocation,
    RawAttributeAnnotation,
    RawDefineAnnotation,
    RawGlobalAnnotation,
    RawInferOutput,
    RawInferOutputForPath,
    RawParameter,
    should_annotate_in_place,
    StubGenerationOptions,
    TypeAnnotation,
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
                        [search_path.SimpleElement("source")]
                    ),
                ),
                paths_to_modify={Path("/derp3.py")},
            ),
            [
                ("log_path", "/log"),
                ("global_root", "/project"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
                ("paths_to_modify", ["/derp3.py"]),
            ],
        )


class InferTest(testslide.TestCase):

    maxDiff = 2000

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
                    "exclude": ["exclude"],
                    "extensions": [".ext"],
                    "workers": 42,
                    "search_path": ["search", "nonexistent"],
                },
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["src"]}, relative="local"
            )

            infer_configuration = frontend_configuration.OpenSource(
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        dot_pyre_directory=root_path / ".pyre",
                    ),
                    root_path,
                )
            )

            self.assertEqual(
                create_infer_arguments(
                    infer_configuration,
                    command_arguments.InferArguments(
                        working_directory=Path("/some/directory"),
                        debug_infer=True,
                        sequential=False,
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
                        checked_directory_blocklist=[
                            str(root_path / "blocks"),
                            str(root_path / "nonexistent"),
                        ],
                        debug=True,
                        excludes=["exclude"],
                        extensions=[".ext"],
                        relative_local_root="local",
                        number_of_workers=42,
                        parallel=True,
                        python_version=infer_configuration.get_python_version(),
                        search_paths=[
                            search_path.SimpleElement(str(root_path / "search"))
                        ],
                        source_paths=backend_arguments.SimpleSourcePath(
                            [search_path.SimpleElement(str(root_path / "local/src"))]
                        ),
                    ),
                    paths_to_modify={Path("path/to/module.py")},
                ),
            )

    def test_parse_raw_infer_output(self) -> None:
        def assert_parsed(input: Dict[str, object], expected: RawInferOutput) -> None:
            self.assertEqual(RawInferOutput.create_from_json(input), expected)

        def assert_not_parsed(input: str) -> None:
            with self.assertRaises(RawInferOutput.ParsingError):
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
                        is_async=True,
                    )
                ]
            ),
        )

    def test_raw_infer_output_split(self) -> None:
        def assert_split(given: Dict[str, object], expected: Dict[str, Any]) -> None:
            input_infer_output = RawInferOutput.create_from_json(given)
            expected_infer_output = {
                path: RawInferOutputForPath.create_from_json(output)
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
            "location": {"qualifier": "baz", "path": "baz.py", "line": 1},
            "async": False,
        }

        assert_split({}, expected={})
        assert_split(
            {"globals": [foo_global0]},
            {
                "foo.py": {
                    "qualifier": "foo",
                    "globals": [foo_global0],
                },
            },
        )
        assert_split(
            {
                "globals": [
                    foo_global0,
                    bar_global0,
                    foo_global1,
                ]
            },
            {
                "foo.py": {
                    "qualifier": "foo",
                    "globals": [foo_global0, foo_global1],
                },
                "bar.py": {
                    "qualifier": "bar",
                    "globals": [bar_global0],
                },
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
                    "qualifier": "foo",
                    "globals": [
                        foo_global0,
                    ],
                },
                "bar.py": {
                    "qualifier": "bar",
                    "attributes": [bar_attribute0],
                },
                "baz.py": {
                    "qualifier": "baz",
                    "defines": [baz_define0],
                },
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
    infer_output: RawInferOutputForPath


class ModuleAnnotationTest(testslide.TestCase):

    maxDiff = 2000

    def test_module_annotations_from_infer_output(self) -> None:
        def assert_result(
            path: str,
            infer_output: RawInferOutputForPath,
            options: StubGenerationOptions,
            expected: ModuleAnnotations,
        ) -> None:
            self.assertEqual(
                ModuleAnnotations.from_infer_output(path, infer_output, options),
                expected,
            )

        default_path = "test.py"
        default_qualifier = "test"
        default_options = StubGenerationOptions()

        assert_result(
            path=default_path,
            infer_output=RawInferOutputForPath(qualifier=default_qualifier),
            options=default_options,
            expected=ModuleAnnotations(path=default_path, options=default_options),
        )

        assert_result(
            path=default_path,
            infer_output=RawInferOutputForPath(
                qualifier=default_qualifier,
                define_annotations=[
                    RawDefineAnnotation(
                        name="test.Foo.foo",
                        parent="test.Foo",
                        location=RawAnnotationLocation(
                            qualifier="test", path="test.py", line=1
                        ),
                        return_="int",
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
                            None,
                            qualifier=default_qualifier,
                            options=default_options,
                        ),
                        parameters=[],
                        is_async=False,
                    )
                ],
                methods=[
                    MethodAnnotation(
                        parent="test.Foo",
                        name="test.Foo.foo",
                        return_annotation=TypeAnnotation.from_raw(
                            "int",
                            qualifier=default_qualifier,
                            options=default_options,
                        ),
                        parameters=[],
                        is_async=True,
                    )
                ],
            ),
        )

        assert_result(
            path=default_path,
            infer_output=RawInferOutputForPath(
                qualifier=default_qualifier,
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
                            "int",
                            qualifier=default_qualifier,
                            options=default_options,
                        ),
                    )
                ],
            ),
        )

        assert_result(
            path=default_path,
            infer_output=RawInferOutputForPath(
                qualifier=default_qualifier,
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
            infer_output=RawInferOutputForPath(
                qualifier=default_qualifier,
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
                            "int",
                            qualifier=default_qualifier,
                            options=annotate_attribute_options,
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
                qualifier="p0.foo", path="/root/p0/foo.py", line=1
            ),
            annotation="int",
        )
        foo_global1 = RawGlobalAnnotation(
            name="y",
            location=RawAnnotationLocation(
                qualifier="p0.foo", path="/root/p0/foo.py", line=2
            ),
            annotation="str",
        )
        bar_global0 = RawGlobalAnnotation(
            name="x",
            location=RawAnnotationLocation(
                qualifier="p1.bar", path="/root/p1/bar.py", line=1
            ),
            annotation="int",
        )
        bar_attribute0 = RawAttributeAnnotation(
            parent="bar.Foo",
            name="a",
            location=RawAnnotationLocation(
                qualifier="p1.bar", path="/root/p1/bar.py", line=2
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
                    infer_output=RawInferOutputForPath(
                        qualifier="p0.foo",
                        global_annotations=[foo_global0, foo_global1],
                    ),
                ),
                ExpectedModuleAnnotationItem(
                    path="p1/bar.py",
                    infer_output=RawInferOutputForPath(
                        qualifier="p1.bar",
                        global_annotations=[bar_global0],
                        attribute_annotations=[bar_attribute0],
                    ),
                ),
            ],
        )

        # Test relativization & path filtering
        #
        # Note that the qualifier doesn't inherently correspond to the
        # relative path - a local configuration can be nested inside a
        # project, in which case the qualifier is still relative to the
        # project root.
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
                    infer_output=RawInferOutputForPath(
                        qualifier="p1.bar",
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


class TypeAnnotationTest(testslide.TestCase):
    def assert_to_stub(
        self,
        raw_annotation: str,
        expected: str,
        qualifier: str = "foo",
        prefix: str = "",
        runtime_defined: bool = True,
        **stub_generation_options_kwargs: Any,
    ) -> None:
        actual = TypeAnnotation(
            annotation=raw_annotation,
            qualifier=qualifier,
            options=StubGenerationOptions(
                **stub_generation_options_kwargs,
            ),
            runtime_defined=runtime_defined,
        ).to_stub(prefix=prefix)
        self.assertEqual(actual, expected)

    def test_sanitize__fix_PathLike(self) -> None:
        self.assert_to_stub(
            "PathLike[str]",
            "'os.PathLike[str]'",
        )
        self.assert_to_stub(
            "typing.Union[PathLike[bytes], PathLike[str], str]",
            "typing.Union['os.PathLike[bytes]', 'os.PathLike[str]', str]",
        )
        # This type is unparseable. The test verifies that libcst doesn't
        # throw an exception here - we wouldn't want to completely crash
        # just because the backend prints out a type that isn't valid
        # python.
        self.assert_to_stub(
            "PathLike[Variable[AnyStr <: [str, bytes]]]",
            "PathLike[Variable[AnyStr <: [str, bytes]]]",
        )
        # only bare PathLike gets converted, not a qualified type
        self.assert_to_stub(
            "bar.PathLike[str]",
            "bar.PathLike[str]",
        )

    def test_sanitize__strip_qualifier(self) -> None:
        self.assert_to_stub(
            "foo.A",
            "A",
        )
        self.assert_to_stub(
            "typing.Union[foo.A[bar.B], bar.C[foo.B]]",
            "typing.Union[A[bar.B], bar.C[B]]",
        )
        # don't strip the qualifier when it's only a partial match
        self.assertEqual(
            "foo.bar.A",
            "foo.bar.A",
        )

    def test_sanitize__quote_annotations(self) -> None:
        self.assert_to_stub(
            "typing.Union[foo.A[bar.B], bar.C[foo.B]]",
            '"typing.Union[foo.A[bar.B], bar.C[foo.B]]"',
            quote_annotations=True,
        )

    def test_sanitize__runtime_defined(self) -> None:
        self.assert_to_stub(
            "foo.A",
            '"A"',
            runtime_defined=False,
        )

    def test_to_stub_with_prefix(self) -> None:
        self.assert_to_stub(
            "foo.Foo[int]",
            ": Foo[int]",
            prefix=": ",
        )
        self.assert_to_stub(
            "foo.Foo[int]",
            ': "foo.Foo[int]"',
            prefix=": ",
            quote_annotations=True,
        )


class StubGenerationTest(testslide.TestCase):
    def _assert_stubs(
        self,
        data: Dict[str, Any],
        expected: str,
        annotate_attributes: bool = False,
        use_future_annotations: bool = False,
        quote_annotations: bool = False,
        simple_annotations: bool = False,
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
                simple_annotations=simple_annotations,
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
                        "async": False,
                    }
                ]
            },
            """\
            class Test:
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
                        "async": True,
                    }
                ]
            },
            """\
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
                        "async": False,
                    },
                ],
            },
            """\
            class Test:
                def ret_dict(self) -> typing.Dict[int, str]: ...
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
                        "async": False,
                    },
                ],
            },
            """\
            class Test:
                def b(self) -> typing.Union[typing.Dict[str, int], str]: ...
                def a(self) -> typing.Union[typing.Dict[str, int], str]: ...
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
                        "async": False,
                    },
                ],
            },
            """\
            class TestA:
                def f(self) -> typing.Union[typing.Dict[str, int], str]: ...

            class TestB:
                def f(self) -> typing.Union[typing.Dict[str, int], str]: ...
            """,
        )

        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "test.TestA",
                        "name": "f",
                        "parent": "test.TestA",
                        "parameters": [
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            },
                            {
                                "name": "input",
                                "annotation": "test.TestA",
                                "value": None,
                                "index": 0,
                            },
                        ],
                        "async": False,
                    },
                    {
                        "return": "typing.Union[typing.Dict[str, int], str]",
                        "name": "g",
                        "parent": "test.TestA",
                        "parameters": [
                            {
                                "name": "self",
                                "annotation": None,
                                "value": None,
                                "index": 0,
                            },
                            {
                                "name": "input",
                                "annotation": "int",
                                "value": None,
                                "index": 0,
                            },
                        ],
                        "async": False,
                    },
                ],
            },
            """\
            class TestA:
                def f(self, input: "TestA") -> "TestA": ...
                def g(self, input: int) -> typing.Union[typing.Dict[str, int], str]: ...
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
        self._assert_stubs(
            {
                "attributes": [
                    {
                        "annotation": "test.test",
                        "name": "attribute_name",
                        "parent": "test.test",
                    }
                ],
            },
            """\
            class test:
                attribute_name: "test" = ...
            """,
            annotate_attributes=True,
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
                        "async": False,
                    }
                ]
            },
            """\
            def with_params(y=7, x: typing.List[int] = [5]) -> Union[int, str]: ...
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
                        "async": False,
                    }
                ]
            },
            """\
            def with_params(y=7, x: "typing.List[int]" = [5]) -> "Union[int, str]": ...
            """,
            quote_annotations=True,
        )

    def test_stubs_simple(self) -> None:
        """
        Test generating stubs while omitting annotations that aren't guaranteed landable
        """
        self._assert_stubs(
            {
                "defines": [
                    {
                        "return": "None",
                        "name": "test.with_params",
                        "parent": None,
                        "parameters": [
                            {
                                "name": "x",
                                "annotation": "int",
                                "value": None,
                                "index": 0,
                            },
                            {
                                "name": "y",
                                "annotation": "typing.List[int]",
                                "value": None,
                                "index": 1,
                            },
                            {
                                "name": "z",
                                "annotation": "Union[int, str]",
                                "value": None,
                                "index": 2,
                            },
                        ],
                        "async": False,
                    }
                ]
            },
            """\
            def with_params(x: int, y, z: Union[int, str]) -> None: ...
            """,
            simple_annotations=True,
        )


class StubApplicationTest(testslide.TestCase):
    def _normalize(self, block_string: str) -> str:
        return (
            textwrap.dedent(block_string)
            .strip()
            .replace("@_GENERATED", "@" + "generated")
        )

    def _assert_in_place(
        self,
        stub_file_contents: str,
        code_file_contents: str,
        expected_annotated_code_file_contents: Optional[str],
    ) -> None:
        options = StubGenerationOptions(
            annotate_attributes=True,
            use_future_annotations=False,
            dequalify=False,
            quote_annotations=False,
            simple_annotations=False,
        )
        annotated_code = AnnotateModuleInPlace._annotated_code(
            code_path="code_path.py",
            stub=self._normalize(stub_file_contents),
            code=self._normalize(code_file_contents),
            options=options,
        )
        expected_code = (
            self._normalize(expected_annotated_code_file_contents)
            if expected_annotated_code_file_contents is not None
            else None
        )
        self.assertEqual(expected_code, annotated_code)

    def test_apply_functions(self) -> None:
        self._assert_in_place(
            """
            def foo(x: int) -> None: ...
            """,
            """
            def foo(x):
                pass
            """,
            """
            def foo(x: int) -> None:
                pass
            """,
        )
        self._assert_in_place(
            """
            def incomplete_stubs(x: int, y) -> None: ...
            """,
            """
            def incomplete_stubs(x, y: int):
                pass
            """,
            """
            def incomplete_stubs(x: int, y: int) -> None:
                pass
            """,
        )
        self._assert_in_place(
            """
            def incomplete_stubs_with_stars(x: int, *args, **kwargs) -> None: ...
            """,
            """
            def incomplete_stubs_with_stars(x, *args: P.args, **kwargs: P.kwargs):
                pass
            """,
            """
            def incomplete_stubs_with_stars(x: int, *args: P.args, **kwargs: P.kwargs) -> None:
                pass
            """,
        )

    def test_apply_globals(self) -> None:
        self._assert_in_place(
            """
            a: int = ...
            """,
            """
            a = 1 + 1
            """,
            """
            a: int = 1 + 1
            """,
        )

        self._assert_in_place(
            """
            a: int = ...
            b: int = ...
            """,
            """
            a = b = 1 + 1
            """,
            """
            a: int
            b: int

            a = b = 1 + 1
            """,
        )

        self._assert_in_place(
            """
            _: str = ...
            a: str = ...
            """,
            """
            _, a = "string".split("")
            """,
            """
            a: str

            _, a = "string".split("")
            """,
        )

    def test_forward_references(self) -> None:
        self._assert_in_place(
            """
            class Foo:
                def method(self) -> Foo: ...
            """,
            """
            class Foo:
                def method(self):
                    return self
            """,
            """
            class Foo:
                def method(self) -> "Foo":
                    return self
            """,
        )

        self._assert_in_place(
            """
            def foo() -> Foo: ...
            """,
            """
            def foo():
                return Foo()

            class Foo:
                pass
            """,
            """
            def foo() -> "Foo":
                return Foo()

            class Foo:
                pass
            """,
        )

    def test_generated(self) -> None:
        self._assert_in_place(
            """
            def foo() -> None: ...
            """,
            """
            # not generated
            def foo():
                return
            """,
            """
            # not generated
            def foo() -> None:
                return
            """,
        )
        self._assert_in_place(
            """
            def foo() -> None: ...
            """,
            """
            # @_GENERATED
            def foo():
                return
            """,
            None,
        )
