# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import datetime
import tempfile
from pathlib import Path
from typing import Iterable, Optional, Tuple

import testslide

from ... import (
    backend_arguments,
    command_arguments,
    configuration as configuration_module,
    daemon_socket,
    frontend_configuration,
)
from ...configuration import search_path
from ...find_directories import CODENAV_CONFIGURATION_FILE
from ...identifiers import PyreFlavor
from ...tests import setup
from ..start import (
    Arguments,
    background_server_log_file,
    create_server_arguments,
    CriticalFile,
    daemon_log_path,
    datetime_from_log_path,
    get_critical_files,
    get_saved_state_action,
    LoadSavedStateFromFile,
    LoadSavedStateFromProject,
    MatchPolicy,
    StoreSavedStateToFile,
)


class TestSavedStateConfiguration(frontend_configuration.OpenSource):
    saved_state_project: Optional[frontend_configuration.SavedStateProject]

    def __init__(
        self,
        configuration: configuration_module.Configuration,
        saved_state_project: Optional[frontend_configuration.SavedStateProject] = None,
    ) -> None:
        super().__init__(configuration)
        self.saved_state_project = saved_state_project

    def get_saved_state_project(
        self,
    ) -> Optional[frontend_configuration.SavedStateProject]:
        return self.saved_state_project

    def get_include_suppressed_errors(self) -> Optional[bool]:
        raise NotImplementedError()


class ArgumentTest(testslide.TestCase):
    def test_serialize_critical_file(self) -> None:
        self.assertDictEqual(
            CriticalFile(policy=MatchPolicy.BASE_NAME, path="foo").serialize(),
            {"base_name": "foo"},
        )
        self.assertDictEqual(
            CriticalFile(policy=MatchPolicy.EXTENSION, path="foo").serialize(),
            {"extension": "foo"},
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
        self.assertTupleEqual(
            StoreSavedStateToFile(shared_memory_path="/foo/bar").serialize(),
            ("save_to_file", {"shared_memory_path": "/foo/bar"}),
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
            Arguments(
                base_arguments=backend_arguments.BaseArguments(
                    log_path="foo",
                    global_root="bar",
                    source_paths=backend_arguments.SimpleSourcePath(
                        [search_path.SimpleElement("source")]
                    ),
                ),
                taint_models_path=["/taint/model"],
                strict=True,
                show_error_traces=True,
                store_type_check_resolution=True,
                critical_files=[
                    CriticalFile(policy=MatchPolicy.BASE_NAME, path="foo.py"),
                    CriticalFile(policy=MatchPolicy.EXTENSION, path="txt"),
                    CriticalFile(policy=MatchPolicy.FULL_PATH, path="/home/bar.txt"),
                ],
                watchman_root=Path("/project"),
                saved_state_action=LoadSavedStateFromProject(
                    project_name="my_project", project_metadata="my_metadata"
                ),
                socket_path=Path("not_relevant_for_this_test.sock"),
            ),
            [
                ("log_path", "foo"),
                ("global_root", "bar"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
                ("taint_model_paths", ["/taint/model"]),
                ("strict", True),
                ("show_error_traces", True),
                ("store_type_check_resolution", True),
                (
                    "critical_files",
                    [
                        {"base_name": "foo.py"},
                        {"extension": "txt"},
                        {"full_path": "/home/bar.txt"},
                    ],
                ),
                ("watchman_root", "/project"),
                (
                    "saved_state_action",
                    (
                        "load_from_project",
                        {
                            "project_name": "my_project",
                            "project_metadata": "my_metadata",
                        },
                    ),
                ),
            ],
        )


class StartTest(testslide.TestCase):
    def test_daemon_log_path(self) -> None:
        now = datetime.datetime(2018, 5, 6)
        # classic flavor has empty suffix
        path = daemon_log_path(
            log_directory=Path("some_directory"),
            flavor=PyreFlavor.CLASSIC,
            now=now,
        )
        self.assertEqual(
            path,
            Path("some_directory/server.stderr.2018_05_06_00_00_00_000000"),
        )
        self.assertEqual(datetime_from_log_path(path), now)
        # other flavors have nonempty suffix
        path = daemon_log_path(
            log_directory=Path("some_directory"),
            flavor=PyreFlavor.SHADOW,
            now=now,
        )
        self.assertEqual(
            path,
            Path("some_directory/server__shadow.stderr.2018_05_06_00_00_00_000000"),
        )
        self.assertEqual(datetime_from_log_path(path), now)

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
                    frontend_configuration.OpenSource(
                        configuration_module.create_configuration(
                            command_arguments.CommandArguments(
                                local_configuration="local"
                            ),
                            root_path,
                        )
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

    def test_get_critical_files_code_navigation(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project/local"])
            setup.write_configuration_file(
                root_path, {"critical_files": ["foo", "bar/baz"]}, codenav=True
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["."]}, relative="local", codenav=True
            )
            setup.ensure_files_exist(root_path, ["foo", "bar/baz"])

            self.assertCountEqual(
                get_critical_files(
                    frontend_configuration.OpenSource(
                        configuration_module.create_overridden_configuration(
                            command_arguments.CommandArguments(
                                strict=True,  # override configuration file
                                source_directories=["."],
                                dot_pyre_directory=Path(".pyre"),
                            ),
                            base_directory=Path(root),
                            configuration=CODENAV_CONFIGURATION_FILE,
                        )
                    ),
                    PyreFlavor.CODE_NAVIGATION,
                ),
                [
                    CriticalFile(
                        MatchPolicy.FULL_PATH,
                        str(root_path / ".pyre_configuration.codenav"),
                    ),
                    CriticalFile(MatchPolicy.FULL_PATH, str(root_path / "foo")),
                    CriticalFile(MatchPolicy.FULL_PATH, str(root_path / "bar/baz")),
                ],
            )

    def test_get_critical_files_with_buck(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project/local"])
            setup.write_configuration_file(root_path, {"targets": ["//foo:bar"]})

            self.assertCountEqual(
                get_critical_files(
                    frontend_configuration.OpenSource(
                        configuration_module.create_configuration(
                            command_arguments.CommandArguments(),
                            root_path,
                        )
                    )
                ),
                [
                    CriticalFile(
                        MatchPolicy.FULL_PATH, str(root_path / ".pyre_configuration")
                    ),
                    CriticalFile(
                        MatchPolicy.EXTENSION,
                        "thrift",
                    ),
                ],
            )

    def test_get_saved_state_action(self) -> None:
        empty_configuration = configuration_module.Configuration(
            global_root=Path("irrelevant"), dot_pyre_directory=Path("irrelevant")
        )
        self.assertIsNone(
            get_saved_state_action(
                command_arguments.StartArguments(),
                TestSavedStateConfiguration(empty_configuration),
            )
        )

        saved_state_configuration = TestSavedStateConfiguration(
            empty_configuration,
            saved_state_project=frontend_configuration.SavedStateProject(
                name="test_saved_state"
            ),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(),
                saved_state_configuration,
            ),
            LoadSavedStateFromProject(project_name="test_saved_state"),
        )

        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(load_initial_state_from="foo"),
                saved_state_configuration,
            ),
            LoadSavedStateFromFile(shared_memory_path="foo"),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(
                    load_initial_state_from="foo",
                    changed_files_path="bar",
                ),
                saved_state_configuration,
            ),
            LoadSavedStateFromFile(shared_memory_path="foo", changed_files_path="bar"),
        )

        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(
                    load_initial_state_from="foo", changed_files_path="bar"
                ),
                saved_state_configuration,
                relative_local_root="local/root",
            ),
            LoadSavedStateFromFile(shared_memory_path="foo", changed_files_path="bar"),
        )
        self.assertEqual(
            get_saved_state_action(
                command_arguments.StartArguments(save_initial_state_to="/foo"),
                saved_state_configuration,
            ),
            StoreSavedStateToFile(shared_memory_path="/foo"),
        )

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
                    "only_check_paths": ["allows", "nonexistent"],
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

            server_configuration = frontend_configuration.OpenSource(
                configuration_module.create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        dot_pyre_directory=root_path / ".pyre",
                    ),
                    root_path,
                )
            )
            self.assertEqual(
                create_server_arguments(
                    server_configuration,
                    command_arguments.StartArguments(
                        debug=True,
                        no_watchman=False,
                        sequential=False,
                        show_error_traces=True,
                        store_type_check_resolution=True,
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
                        python_version=server_configuration.get_python_version(),
                        search_paths=[
                            search_path.SimpleElement(str(root_path / "search"))
                        ],
                        source_paths=backend_arguments.SimpleSourcePath(
                            [search_path.SimpleElement(str(root_path / "local/src"))]
                        ),
                    ),
                    additional_logging_sections=["server"],
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
                    saved_state_action=None,
                    show_error_traces=True,
                    store_type_check_resolution=True,
                    strict=True,
                    taint_models_path=[str(root_path / "taint")],
                    watchman_root=root_path,
                    socket_path=daemon_socket.get_socket_path(
                        f"{root_path}//local",
                        flavor=PyreFlavor.CLASSIC,
                    ),
                ),
            )

    def test_create_server_arguments_watchman_not_found(self) -> None:
        with tempfile.TemporaryDirectory(dir="/tmp") as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            setup.write_configuration_file(
                root_path,
                {"source_directories": ["src"]},
            )
            arguments = create_server_arguments(
                frontend_configuration.OpenSource(
                    configuration_module.create_configuration(
                        command_arguments.CommandArguments(
                            dot_pyre_directory=root_path / ".pyre",
                        ),
                        root_path,
                    )
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
                frontend_configuration.OpenSource(
                    configuration_module.create_configuration(
                        command_arguments.CommandArguments(
                            dot_pyre_directory=root_path / ".pyre",
                        ),
                        root_path,
                    )
                ),
                command_arguments.StartArguments(no_saved_state=True),
            )
            self.assertIsNone(arguments.saved_state_action)

    def test_create_server_arguments_skip_initial_type_check(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            setup.write_configuration_file(
                root_path,
                {"source_directories": ["src"]},
            )
            arguments = create_server_arguments(
                frontend_configuration.OpenSource(
                    configuration_module.create_configuration(
                        command_arguments.CommandArguments(
                            dot_pyre_directory=root_path / ".pyre",
                        ),
                        root_path,
                    )
                ),
                command_arguments.StartArguments(
                    skip_initial_type_check=True,
                ),
            )
            self.assertTrue(arguments.skip_initial_type_check)

    def test_create_server_arguments_logging(self) -> None:
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

            arguments = create_server_arguments(
                frontend_configuration.OpenSource(
                    configuration_module.create_configuration(
                        command_arguments.CommandArguments(dot_pyre_directory=log_path),
                        root_path,
                    )
                ),
                command_arguments.StartArguments(
                    logging_sections="foo,bar,-baz",
                    noninteractive=True,
                    enable_profiling=True,
                    enable_memory_profiling=True,
                    _log_identifier="derp",
                ),
            )
            self.assertListEqual(
                list(arguments.additional_logging_sections),
                ["foo", "bar", "-baz", "-progress", "server"],
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

    def test_background_server_log_placement(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            with background_server_log_file(
                root_path, flavor=PyreFlavor.CLASSIC
            ) as log_file:
                print("foo", file=log_file)
            # Make sure that the log content can be read from a known location.
            self.assertEqual(
                (
                    root_path
                    / PyreFlavor.CLASSIC.server_log_subdirectory()
                    / "server.stderr"
                )
                .read_text()
                .strip(),
                "foo",
            )
