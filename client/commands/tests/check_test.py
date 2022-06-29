# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import tempfile
from pathlib import Path
from typing import Iterable, Tuple

import testslide

from ... import command_arguments, configuration
from ...configuration import search_path
from ...error import Error
from ...tests import setup
from .. import backend_arguments, frontend_configuration
from ..check import (
    Arguments,
    create_check_arguments,
    InvalidCheckResponse,
    parse_type_error_response,
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
                additional_logging_sections=["foo", "bar"],
                show_error_traces=True,
                strict=True,
            ),
            [
                ("log_path", "/log"),
                ("global_root", "/project"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
                ("additional_logging_sections", ["foo", "bar"]),
                ("show_error_traces", True),
                ("strict", True),
            ],
        )


class CheckTest(testslide.TestCase):
    def test_create_check_arguments(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(
                root_path, [".pyre", "allows", "blocks", "search", "local/src"]
            )
            setup.write_configuration_file(
                root_path,
                {
                    "do_not_ignore_errors_in": ["allows", "nonexistent"],
                    "ignore_all_errors": ["blocks", "nonexistent"],
                    "exclude": ["exclude"],
                    "extensions": [".ext", "invalid_extension"],
                    "workers": 42,
                    "search_path": ["search", "nonexistent"],
                    "strict": True,
                },
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["src"]}, relative="local"
            )

            check_configuration = frontend_configuration.OpenSource(
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        dot_pyre_directory=root_path / ".pyre",
                    ),
                    root_path,
                )
            )

            self.assertEqual(
                create_check_arguments(
                    check_configuration,
                    command_arguments.CheckArguments(
                        debug=True,
                        sequential=False,
                        show_error_traces=True,
                    ),
                ),
                Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        log_path=str(root_path / ".pyre/local"),
                        global_root=str(root_path),
                        checked_directory_allowlist=[
                            str(root_path / "allows"),
                            str(root_path / "nonexistent"),
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
                        python_version=check_configuration.get_python_version(),
                        search_paths=[
                            search_path.SimpleElement(str(root_path / "search"))
                        ],
                        source_paths=backend_arguments.SimpleSourcePath(
                            [search_path.SimpleElement(str(root_path / "local/src"))]
                        ),
                    ),
                    show_error_traces=True,
                    strict=True,
                ),
            )

    def test_create_check_arguments_artifact_root_no_conflict(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project"])
            setup.ensure_files_exist(root_path, ["project/.buckconfig"])
            setup.write_configuration_file(
                root_path / "project",
                {
                    "targets": ["//foo:bar"],
                },
            )

            arguments = create_check_arguments(
                frontend_configuration.OpenSource(
                    configuration.create_configuration(
                        command_arguments.CommandArguments(
                            dot_pyre_directory=root_path / ".pyre"
                        ),
                        root_path / "project",
                    )
                ),
                command_arguments.CheckArguments(),
            )
            # Make sure we are not overwriting the artifact root for server command
            self.assertNotEqual(
                arguments.base_arguments.source_paths.serialize()["artifact_root"],
                root_path / ".pyre" / backend_arguments.SERVER_ARTIFACT_ROOT_NAME,
            )

    def test_create_check_arguments_logging(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            log_path = root_path / ".pyre"
            logger_path = root_path / "logger"

            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            setup.ensure_files_exist(root_path, ["logger"])
            setup.write_configuration_file(
                root_path,
                {"source_directories": ["src"], "logger": str(logger_path)},
            )

            arguments = create_check_arguments(
                frontend_configuration.OpenSource(
                    configuration.create_configuration(
                        command_arguments.CommandArguments(dot_pyre_directory=log_path),
                        root_path,
                    )
                ),
                command_arguments.CheckArguments(
                    logging_sections="foo,bar,-baz",
                    noninteractive=True,
                    enable_profiling=True,
                    enable_memory_profiling=True,
                    log_identifier="derp",
                ),
            )
            self.assertListEqual(
                list(arguments.additional_logging_sections),
                ["foo", "bar", "-baz", "-progress"],
            )
            self.assertEqual(
                arguments.base_arguments.profiling_output,
                backend_arguments.get_profiling_log_path(log_path),
            )
            self.assertEqual(
                arguments.base_arguments.memory_profiling_output,
                backend_arguments.get_profiling_log_path(log_path),
            )
            self.assertEqual(
                arguments.base_arguments.remote_logging,
                backend_arguments.RemoteLogging(
                    logger=str(logger_path), identifier="derp"
                ),
            )

    def test_parse_response(self) -> None:
        def assert_parsed(response: str, expected: Iterable[Error]) -> None:
            self.assertListEqual(parse_type_error_response(response), list(expected))

        def assert_not_parsed(response: str) -> None:
            with self.assertRaises(InvalidCheckResponse):
                parse_type_error_response(response)

        assert_not_parsed("derp")
        assert_not_parsed("[]")
        assert_not_parsed('["Error"]')
        assert_not_parsed('["TypeErrors"]')
        assert_not_parsed('["TypeErrors", "derp"]')
        assert_not_parsed('["TypeErrors", {}]')

        assert_parsed("{}", [])
        assert_parsed('{"errors": []}', [])
        assert_parsed('{"derp": 42}', [])
        assert_not_parsed('{"errors": ["derp"]}')
        assert_not_parsed('{"errors": [{}]}')
        assert_parsed(
            json.dumps(
                {
                    "errors": [
                        {
                            "line": 1,
                            "column": 1,
                            "stop_line": 3,
                            "stop_column": 3,
                            "path": "test.py",
                            "code": 42,
                            "name": "Fake name",
                            "description": "Fake description",
                        },
                        {
                            "line": 2,
                            "column": 2,
                            "stop_line": 4,
                            "stop_column": 4,
                            "path": "test.py",
                            "code": 43,
                            "name": "Fake name 2",
                            "description": "Fake description 2",
                            "concise_description": "Concise description 2",
                            "long_description": "Long description 2",
                        },
                    ],
                }
            ),
            expected=[
                Error(
                    line=1,
                    column=1,
                    stop_line=3,
                    stop_column=3,
                    path=Path("test.py"),
                    code=42,
                    name="Fake name",
                    description="Fake description",
                ),
                Error(
                    line=2,
                    column=2,
                    stop_line=4,
                    stop_column=4,
                    path=Path("test.py"),
                    code=43,
                    name="Fake name 2",
                    description="Fake description 2",
                    concise_description="Concise description 2",
                    long_description="Long description 2",
                ),
            ],
        )
