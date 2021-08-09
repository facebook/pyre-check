# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import tempfile
from pathlib import Path
from typing import Iterable, Tuple, Dict

import testslide

from .... import configuration, command_arguments
from ....tests import setup
from .. import backend_arguments
from ..infer import (
    Arguments,
    InferMode,
    create_infer_arguments,
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
                log_path="foo",
                global_root="bar",
                source_paths=backend_arguments.SimpleSourcePath(
                    [configuration.SimpleSearchPathElement("source")]
                ),
            ),
            [
                ("log_path", "foo"),
                ("global_root", "bar"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
            ],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                source_paths=backend_arguments.SimpleSourcePath(),
                excludes=["/excludes"],
                checked_directory_allowlist=["/allows"],
                checked_directory_blocklist=["/blocks"],
                extensions=[".typsy"],
                ignore_infer=["/ignore"],
            ),
            [
                ("excludes", ["/excludes"]),
                ("checked_directory_allowlist", ["/allows"]),
                ("checked_directory_blocklist", ["/blocks"]),
                ("extensions", [".typsy"]),
                ("ignore_infer", ["/ignore"]),
            ],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                source_paths=backend_arguments.SimpleSourcePath(),
            ),
            [("debug", False), ("infer_mode", ["Local"])],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                source_paths=backend_arguments.SimpleSourcePath(),
                debug=True,
                infer_mode=InferMode.INTERPROCEDURAL,
            ),
            [("debug", True), ("infer_mode", ["Interprocedural"])],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                source_paths=backend_arguments.SimpleSourcePath(),
                parallel=True,
                number_of_workers=20,
            ),
            [("parallel", True), ("number_of_workers", 20)],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                source_paths=backend_arguments.SimpleSourcePath(),
                relative_local_root="local",
            ),
            [("local_root", "/project/local")],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                source_paths=backend_arguments.SimpleSourcePath(),
                remote_logging=backend_arguments.RemoteLogging(
                    logger="/logger", identifier="baz"
                ),
                profiling_output=Path("/derp"),
                memory_profiling_output=Path("/derp2"),
            ),
            [
                ("profiling_output", "/derp"),
                ("remote_logging", {"logger": "/logger", "identifier": "baz"}),
                ("memory_profiling_output", "/derp2"),
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
                        debug_infer=True,
                        sequential=False,
                        interprocedural=True,
                    ),
                ),
                Arguments(
                    log_path=str(root_path / ".pyre/local"),
                    global_root=str(root_path),
                    checked_directory_allowlist=[
                        str(root_path / "local/src"),
                    ],
                    checked_directory_blocklist=[str(root_path / "blocks")],
                    debug=True,
                    excludes=["exclude"],
                    extensions=[".ext"],
                    ignore_infer=[str(root_path / "ignores")],
                    infer_mode=InferMode.INTERPROCEDURAL,
                    relative_local_root="local",
                    number_of_workers=42,
                    parallel=True,
                    python_version=infer_configuration.get_python_version(),
                    search_paths=[
                        configuration.SimpleSearchPathElement(str(root_path / "search"))
                    ],
                    source_paths=backend_arguments.SimpleSourcePath(
                        [
                            configuration.SimpleSearchPathElement(
                                str(root_path / "local/src")
                            )
                        ]
                    ),
                ),
            )

    def test_parse_raw_infer_output(self) -> None:
        def assert_parsed(input: Dict[str, object], expected: RawInferOutput) -> None:
            self.assertEqual(RawInferOutput.create(json.dumps(input)), expected)

        def assert_not_parsed(input: str) -> None:
            with self.assertRaises(RawInferOutputParsingError):
                RawInferOutput.create(input)

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
        default_options = StubGenerationOptions(
            annotate_attributes=False, use_future_annotations=False, dequalify=False
        )

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
            annotate_attributes=True, use_future_annotations=False, dequalify=False
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
