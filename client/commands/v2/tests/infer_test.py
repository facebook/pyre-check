# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import Iterable, Tuple

import testslide

from .... import configuration
from .. import backend_arguments
from ..infer import Arguments, InferMode


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
