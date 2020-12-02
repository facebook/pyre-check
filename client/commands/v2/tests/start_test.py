# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import tempfile
from pathlib import Path
from typing import Iterable, Tuple

import testslide

from .... import command_arguments, commands, configuration
from ....tests import setup
from ..start import (
    Arguments,
    BackgroundEventWaiter,
    CriticalFile,
    EventParsingException,
    LoadSavedStateFromFile,
    LoadSavedStateFromProject,
    MatchPolicy,
    create_server_arguments,
    find_watchman_root,
    get_critical_files,
    get_saved_state_action,
    get_server_identifier,
)


class ArgumentTest(testslide.TestCase):
    def test_serialize_critical_file(self) -> None:
        self.assertDictEqual(
            CriticalFile(policy=MatchPolicy.BASE_NAME, path="foo").serialize(),
            {"base_name": "foo"},
        )
        self.assertDictEqual(
            CriticalFile(policy=MatchPolicy.FULL_PATH, path="/foo/bar").serialize(),
            {"full_path": "/foo/bar"},
        )

    def test_serialize_saved_state_action(self) -> None:
        self.assertTupleEqual(
            LoadSavedStateFromFile(shared_memory_path="/foo/bar").serialize(),
            ("load_from_file", {"shared_memory_path": "/foo/bar"}),
        )
        self.assertTupleEqual(
            LoadSavedStateFromFile(
                shared_memory_path="/foo/bar", changed_files_path="derp.txt"
            ).serialize(),
            (
                "load_from_file",
                {"shared_memory_path": "/foo/bar", "changed_files_path": "derp.txt"},
            ),
        )
        self.assertTupleEqual(
            LoadSavedStateFromProject(project_name="my_project").serialize(),
            ("load_from_project", {"project_name": "my_project"}),
        )
        self.assertTupleEqual(
            LoadSavedStateFromProject(
                project_name="my_project", project_metadata="my_metadata"
            ).serialize(),
            (
                "load_from_project",
                {"project_name": "my_project", "project_metadata": "my_metadata"},
            ),
        )

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
            Arguments(log_path="foo", global_root="bar", source_paths=["source"]),
            [("log_path", "foo"), ("global_root", "bar"), ("source_paths", ["source"])],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                excludes=["/excludes"],
                checked_directory_allowlist=["/allows"],
                checked_directory_blocklist=["/blocks"],
                extensions=[".typsy"],
                taint_models_path=["/taint/model"],
            ),
            [
                ("excludes", ["/excludes"]),
                ("checked_directory_allowlist", ["/allows"]),
                ("checked_directory_blocklist", ["/blocks"]),
                ("extensions", [".typsy"]),
                ("taint_model_paths", ["/taint/model"]),
            ],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                debug=True,
                strict=True,
                show_error_traces=True,
                store_type_check_resolution=True,
                critical_files=[
                    CriticalFile(policy=MatchPolicy.BASE_NAME, path="foo.py"),
                    CriticalFile(policy=MatchPolicy.FULL_PATH, path="/home/bar.txt"),
                ],
            ),
            [
                ("debug", True),
                ("strict", True),
                ("show_error_traces", True),
                ("store_type_check_resolution", True),
                (
                    "critical_files",
                    [{"base_name": "foo.py"}, {"full_path": "/home/bar.txt"}],
                ),
            ],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                parallel=True,
                number_of_workers=20,
            ),
            [("parallel", True), ("number_of_workers", 20)],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                local_root="/project/local",
                watchman_root=Path("/project"),
            ),
            [("local_root", "/project/local"), ("watchman_root", "/project")],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                saved_state_action=LoadSavedStateFromProject(
                    project_name="my_project", project_metadata="my_metadata"
                ),
            ),
            [
                (
                    "saved_state_action",
                    (
                        "load_from_project",
                        {
                            "project_name": "my_project",
                            "project_metadata": "my_metadata",
                        },
                    ),
                )
            ],
        )


class ServerIdentifierTest(testslide.TestCase):
    def test_server_identifier(self) -> None:
        def assert_server_identifier(
            client_configuration: configuration.Configuration, expected: str
        ) -> None:
            self.assertEqual(get_server_identifier(client_configuration), expected)

        assert_server_identifier(
            configuration.Configuration(
                project_root="project", dot_pyre_directory=Path(".pyre")
            ),
            "project",
        )
        assert_server_identifier(
            configuration.Configuration(
                project_root="my/project", dot_pyre_directory=Path(".pyre")
            ),
            "project",
        )
        assert_server_identifier(
            configuration.Configuration(
                project_root="my/project",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="foo",
            ),
            "project/foo",
        )
        assert_server_identifier(
            configuration.Configuration(
                project_root="my/project",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="foo/bar",
            ),
            "project/foo/bar",
        )


class StartTest(testslide.TestCase):
    def test_get_critical_files(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project/local"])
            setup.write_configuration_file(
                root_path, {"critical_files": ["foo", "bar/baz"]}
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["."]}, relative="local"
            )
            setup.ensure_files_exist(root_path, ["foo", "bar/baz"])

            self.assertCountEqual(
                get_critical_files(
                    configuration.create_configuration(
                        command_arguments.CommandArguments(local_configuration="local"),
                        root_path,
                    )
                ),
                [
                    CriticalFile(
                        MatchPolicy.FULL_PATH, str(root_path / ".pyre_configuration")
                    ),
                    CriticalFile(
                        MatchPolicy.FULL_PATH,
                        str(root_path / "local/.pyre_configuration.local"),
                    ),
                    CriticalFile(MatchPolicy.FULL_PATH, str(root_path / "foo")),
                    CriticalFile(MatchPolicy.FULL_PATH, str(root_path / "bar/baz")),
                ],
            )

    def test_get_saved_state_action(self) -> None:
        self.assertIsNone(get_saved_state_action(command_arguments.StartArguments()))
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(load_initial_state_from="foo")
            ),
            LoadSavedStateFromFile(shared_memory_path="foo"),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(
                    load_initial_state_from="foo", changed_files_path="bar"
                )
            ),
            LoadSavedStateFromFile(shared_memory_path="foo", changed_files_path="bar"),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(saved_state_project="my_project")
            ),
            LoadSavedStateFromProject(project_name="my_project"),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(saved_state_project="my_project"),
                relative_local_root="local/root",
            ),
            LoadSavedStateFromProject(
                project_name="my_project", project_metadata="local$root"
            ),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(
                    load_initial_state_from="foo", changed_files_path="bar"
                ),
                relative_local_root="local/root",
            ),
            LoadSavedStateFromFile(shared_memory_path="foo", changed_files_path="bar"),
        )

    def test_find_watchman_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_files_exist(
                root_path,
                ["foo/qux/derp", "foo/bar/.watchmanconfig", "foo/bar/baz/derp"],
            )

            expected_root = root_path / "foo/bar"
            self.assertEqual(
                find_watchman_root(root_path / "foo/bar/baz"), expected_root
            )
            self.assertEqual(find_watchman_root(root_path / "foo/bar"), expected_root)

            self.assertIsNone(find_watchman_root(root_path / "foo/qux"))
            self.assertIsNone(find_watchman_root(root_path / "foo"))
            self.assertIsNone(find_watchman_root(root_path))

    def test_create_server_arguments(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(
                root_path, [".pyre", "allows", "blocks", "search", "taint", "local/src"]
            )
            setup.ensure_files_exist(root_path, ["critical", ".watchmanconfig"])
            setup.write_configuration_file(
                root_path,
                {
                    "do_not_ignore_errors_in": ["allows", "nonexistent"],
                    "ignore_all_errors": ["blocks", "nonexistent"],
                    "critical_files": ["critical"],
                    "exclude": ["exclude"],
                    "extensions": [".ext", "invalid_extension"],
                    "workers": 42,
                    "search_path": ["search", "nonexistent"],
                    "strict": True,
                    "taint_models_path": ["taint"],
                },
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["src"]}, relative="local"
            )

            self.assertEqual(
                create_server_arguments(
                    configuration.create_configuration(
                        command_arguments.CommandArguments(
                            local_configuration="local",
                            dot_pyre_directory=root_path / ".pyre",
                        ),
                        root_path,
                    ),
                    command_arguments.StartArguments(
                        debug=True,
                        no_watchman=False,
                        saved_state_project="project",
                        sequential=False,
                        show_error_traces=True,
                        store_type_check_resolution=True,
                    ),
                ),
                Arguments(
                    log_path=str(root_path / ".pyre/local"),
                    global_root=str(root_path),
                    checked_directory_allowlist=[
                        str(root_path / "local/src"),
                        str(root_path / "allows"),
                    ],
                    checked_directory_blocklist=[str(root_path / "blocks")],
                    critical_files=[
                        CriticalFile(
                            MatchPolicy.FULL_PATH,
                            str(root_path / ".pyre_configuration"),
                        ),
                        CriticalFile(
                            MatchPolicy.FULL_PATH,
                            str(root_path / "local/.pyre_configuration.local"),
                        ),
                        CriticalFile(
                            MatchPolicy.FULL_PATH, str(root_path / "critical")
                        ),
                    ],
                    debug=True,
                    excludes=["exclude"],
                    extensions=[".ext"],
                    local_root=str(root_path / "local"),
                    number_of_workers=42,
                    parallel=True,
                    saved_state_action=LoadSavedStateFromProject(
                        project_name="project", project_metadata="local"
                    ),
                    search_paths=[
                        configuration.SimpleSearchPathElement(str(root_path / "search"))
                    ],
                    show_error_traces=True,
                    source_paths=[str(root_path / "local/src")],
                    store_type_check_resolution=True,
                    strict=True,
                    taint_models_path=[str(root_path / "taint")],
                    watchman_root=root_path,
                ),
            )

    def test_create_server_arguments_watchman_not_found(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            setup.write_configuration_file(
                root_path,
                {"source_directories": ["src"]},
            )
            arguments = create_server_arguments(
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        dot_pyre_directory=root_path / ".pyre",
                    ),
                    root_path,
                ),
                command_arguments.StartArguments(
                    no_watchman=False,
                ),
            )
            self.assertIsNone(arguments.watchman_root)

    def test_create_server_arguments_disable_saved_state(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            setup.write_configuration_file(
                root_path,
                {"source_directories": ["src"]},
            )
            arguments = create_server_arguments(
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        dot_pyre_directory=root_path / ".pyre",
                    ),
                    root_path,
                ),
                command_arguments.StartArguments(
                    no_saved_state=True, saved_state_project="some/project"
                ),
            )
            self.assertIsNone(arguments.saved_state_action)

    def test_background_waiter_socket_create(self) -> None:
        def assert_ok(event_output: str, wait_on_initialization: bool) -> None:
            BackgroundEventWaiter(
                wait_on_initialization=wait_on_initialization
            ).wait_on(io.StringIO(event_output))

        def assert_raises(event_output: str, wait_on_initialization: bool) -> None:
            with self.assertRaises(EventParsingException):
                BackgroundEventWaiter(
                    wait_on_initialization=wait_on_initialization
                ).wait_on(io.StringIO(event_output))

        assert_raises("garbage", wait_on_initialization=False)
        assert_raises("[]", wait_on_initialization=False)
        assert_ok('["SocketCreated", "/path/to/socket"]', wait_on_initialization=False)
        assert_raises('["ServerInitialized"]', wait_on_initialization=False)
        assert_raises('["ServerException", "message"]', wait_on_initialization=False)

        assert_raises("garbage", wait_on_initialization=True)
        assert_raises("[]", wait_on_initialization=True)
        assert_raises(
            '["SocketCreated", "/path/to/socket"]', wait_on_initialization=True
        )
        assert_raises('["ServerException", "message"]', wait_on_initialization=True)
        assert_raises(
            '["SocketCreated", "/path/to/socket"]\n' + '["ServerException", "message"]',
            wait_on_initialization=True,
        )
        assert_raises(
            '["SocketCreated", "/path/to/socket"]\n'
            + '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=True,
        )
        assert_ok(
            '["SocketCreated", "/path/to/socket"]\n' + '["ServerInitialized"]',
            wait_on_initialization=True,
        )
