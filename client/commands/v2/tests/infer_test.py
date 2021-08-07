# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path
from typing import Iterable, Tuple

import testslide

from .... import configuration, command_arguments
from ....tests import setup
from .. import backend_arguments
from ..infer import Arguments, InferMode, create_infer_arguments


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
