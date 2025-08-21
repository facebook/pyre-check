# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import tempfile
from pathlib import Path
from typing import Iterable, Tuple

import testslide

from .. import command_arguments, configuration, frontend_configuration, identifiers
from ..backend_arguments import (
    BaseArguments,
    BuckSourcePath,
    find_buck2_root,
    find_buck_root,
    find_watchman_root,
    get_checked_directory_allowlist,
    get_source_path,
    RemoteLogging,
    SimpleSourcePath,
    WithUnwatchedDependencySourcePath,
)
from ..configuration import search_path
from ..tests import setup


class ArgumentsTest(testslide.TestCase):
    def test_create_remote_logging(self) -> None:
        self.assertIsNone(
            RemoteLogging.create(),
        )
        self.assertIsNone(
            RemoteLogging.create(identifier="foo"),
        )
        self.assertEqual(
            RemoteLogging.create(logger="logger"),
            RemoteLogging(logger="logger", identifier=""),
        )
        self.assertEqual(
            RemoteLogging.create(logger="logger", identifier="foo"),
            RemoteLogging(logger="logger", identifier="foo"),
        )

    def test_serialize_remote_logging(self) -> None:
        self.assertDictEqual(
            RemoteLogging(logger="/bin/logger").serialize(),
            {"logger": "/bin/logger", "identifier": ""},
        )
        self.assertDictEqual(
            RemoteLogging(logger="/bin/logger", identifier="foo").serialize(),
            {"logger": "/bin/logger", "identifier": "foo"},
        )

    def test_serialize_source_paths(self) -> None:
        self.assertDictEqual(
            SimpleSourcePath(
                [
                    search_path.SimpleElement("/source0"),
                    search_path.SimpleElement("/source1"),
                ]
            ).serialize(),
            {"kind": "simple", "paths": ["/source0", "/source1"]},
        )
        self.assertDictEqual(
            WithUnwatchedDependencySourcePath(
                change_indicator_root=Path("/root"),
                unwatched_dependency=configuration.UnwatchedDependency(
                    change_indicator="foo",
                    files=configuration.UnwatchedFiles(
                        root="/derp",
                        checksum_path="CHECKSUMS",
                    ),
                ),
                elements=[
                    search_path.SimpleElement("/source0"),
                    search_path.SimpleElement("/source1"),
                ],
            ).serialize(),
            {
                "kind": "with_unwatched_dependency",
                "unwatched_dependency": {
                    "change_indicator": {"root": "/root", "relative": "foo"},
                    "files": {"root": "/derp", "checksum_path": "CHECKSUMS"},
                },
                "paths": ["/source0", "/source1"],
            },
        )
        self.assertDictEqual(
            BuckSourcePath(
                source_root=Path("/source"),
                artifact_root=Path("/artifact"),
                checked_directory=Path("/source"),
                targets=["//foo:bar", "//foo:baz"],
            ).serialize(),
            {
                "kind": "buck",
                "source_root": "/source",
                "artifact_root": "/artifact",
                "targets": ["//foo:bar", "//foo:baz"],
                "use_buck2": True,
                "kill_buck_after_build": False,
            },
        )
        self.assertDictEqual(
            BuckSourcePath(
                source_root=Path("/source"),
                artifact_root=Path("/artifact"),
                checked_directory=Path("/source"),
                targets=["//foo:bar"],
                targets_fallback_sources=[search_path.SimpleElement("/source")],
                mode="opt",
                isolation_prefix=".lsp",
                bxl_builder="//foo.bxl:build",
                use_buck2=True,
                kill_buck_after_build=True,
            ).serialize(),
            {
                "kind": "buck",
                "source_root": "/source",
                "artifact_root": "/artifact",
                "targets": ["//foo:bar"],
                "targets_fallback_sources": ["/source"],
                "mode": "opt",
                "isolation_prefix": ".lsp",
                "bxl_builder": "//foo.bxl:build",
                "use_buck2": True,
                "kill_buck_after_build": True,
            },
        )

    def test_serialize_base_arguments(self) -> None:
        def assert_serialized(
            arguments: BaseArguments, items: Iterable[Tuple[str, object]]
        ) -> None:
            serialized = arguments.serialize()
            for key, value in items:
                if key not in serialized:
                    self.fail(f"Cannot find key `{key}` in serialized arguments")
                else:
                    self.assertEqual(value, serialized[key])

        assert_serialized(
            BaseArguments(
                log_path="foo",
                global_root="bar",
                source_paths=SimpleSourcePath([search_path.SimpleElement("source")]),
            ),
            [
                ("log_path", "foo"),
                ("global_root", "bar"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
            ],
        )
        assert_serialized(
            BaseArguments(
                log_path="/log",
                global_root="/project",
                source_paths=SimpleSourcePath(),
                excludes=["/excludes"],
                checked_directory_allowlist=["/allows"],
                checked_directory_blocklist=["/blocks"],
                extensions=[".typsy"],
            ),
            [
                ("excludes", ["/excludes"]),
                ("checked_directory_allowlist", ["/allows"]),
                ("checked_directory_blocklist", ["/blocks"]),
                ("extensions", [".typsy"]),
            ],
        )
        assert_serialized(
            BaseArguments(
                log_path="/log",
                global_root="/project",
                source_paths=SimpleSourcePath(),
                debug=True,
                parallel=True,
                number_of_workers=20,
            ),
            [("debug", True), ("parallel", True), ("number_of_workers", 20)],
        )
        assert_serialized(
            BaseArguments(
                log_path="/log",
                global_root="/project",
                source_paths=SimpleSourcePath(),
                relative_local_root="local",
            ),
            [("local_root", "/project/local")],
        )
        assert_serialized(
            BaseArguments(
                log_path="/log",
                global_root="/project",
                source_paths=SimpleSourcePath(),
                remote_logging=RemoteLogging(logger="/logger", identifier="baz"),
                profiling_output=Path("/derp"),
                memory_profiling_output=Path("/derp2"),
            ),
            [
                ("profiling_output", "/derp"),
                ("remote_logging", {"logger": "/logger", "identifier": "baz"}),
                ("memory_profiling_output", "/derp2"),
            ],
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
                find_watchman_root(root_path / "foo/bar/baz", stop_search_after=3),
                expected_root,
            )
            self.assertEqual(
                find_watchman_root(root_path / "foo/bar", stop_search_after=2),
                expected_root,
            )

            self.assertIsNone(
                find_watchman_root(root_path / "foo/qux", stop_search_after=2)
            )
            self.assertIsNone(
                find_watchman_root(root_path / "foo", stop_search_after=1)
            )
            self.assertIsNone(find_watchman_root(root_path, stop_search_after=0))

    def test_find_buck_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_files_exist(
                root_path,
                ["foo/qux/derp", "foo/bar/.buckconfig", "foo/bar/baz/derp"],
            )

            expected_root = root_path / "foo/bar"
            self.assertEqual(
                find_buck_root(root_path / "foo/bar/baz", stop_search_after=3),
                expected_root,
            )
            self.assertEqual(
                find_buck_root(root_path / "foo/bar", stop_search_after=2),
                expected_root,
            )

            self.assertIsNone(
                find_buck_root(root_path / "foo/qux", stop_search_after=2)
            )
            self.assertIsNone(find_buck_root(root_path / "foo", stop_search_after=1))
            self.assertIsNone(find_buck_root(root_path, stop_search_after=0))

    def test_find_buck2_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_files_exist(
                root_path,
                [
                    "foo/.buckconfig",
                    "foo/qux/derp",
                    "foo/bar/.buckconfig",
                    "foo/bar/baz/derp",
                ],
            )

            expected_root = root_path / "foo"
            self.assertEqual(
                find_buck2_root(root_path / "foo/bar/baz", stop_search_after=3),
                expected_root,
            )
            self.assertEqual(
                find_buck2_root(root_path / "foo/bar", stop_search_after=2),
                expected_root,
            )
            self.assertEqual(
                find_buck2_root(root_path / "foo/qux", stop_search_after=2),
                expected_root,
            )
            self.assertEqual(
                find_buck2_root(root_path / "foo", stop_search_after=1), expected_root
            )
            self.assertIsNone(find_buck2_root(root_path, stop_search_after=0))

    def test_get_simple_source_path__exists(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            raw_element = search_path.SimpleRawElement(str(root_path / "src"))
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.Configuration(
                            global_root=root_path / "project",
                            dot_pyre_directory=(root_path / ".pyre"),
                            source_directories=[raw_element],
                        )
                    ),
                    artifact_root_name="irrelevant",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=42,
                    watchman_root=None,
                ),
                SimpleSourcePath([raw_element.to_element()]),
            )

    def test_get_simple_source_path__nonexists(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre"])
            raw_element = search_path.SimpleRawElement(str(root_path / "src"))
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.Configuration(
                            global_root=root_path / "project",
                            dot_pyre_directory=(root_path / ".pyre"),
                            source_directories=[raw_element],
                        )
                    ),
                    artifact_root_name="irrelevant",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=42,
                    watchman_root=None,
                ),
                SimpleSourcePath([]),
            )

    def test_get_with_unwatched_dependency_source_path__exists(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project/local"])
            setup.ensure_files_exist(
                root_path, ["src/indicator", "unwatched_root/CHECKSUMS"]
            )
            watched_root = root_path / "src"
            raw_element = search_path.SimpleRawElement(str(root_path / "src"))
            unwatched_dependency = configuration.UnwatchedDependency(
                change_indicator="indicator",
                files=configuration.UnwatchedFiles(
                    root=str(root_path / "unwatched_root"), checksum_path="CHECKSUMS"
                ),
            )
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.Configuration(
                            global_root=root_path / "project",
                            relative_local_root="local",
                            dot_pyre_directory=(root_path / ".pyre"),
                            source_directories=[raw_element],
                            unwatched_dependency=unwatched_dependency,
                        )
                    ),
                    artifact_root_name="irrelevant",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=None,
                    watchman_root=watched_root,
                ),
                WithUnwatchedDependencySourcePath(
                    elements=[raw_element.to_element()],
                    change_indicator_root=watched_root,
                    unwatched_dependency=unwatched_dependency,
                ),
            )

    def test_get_with_unwatched_dependency_source_path__nonexists(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project"])
            setup.ensure_files_exist(root_path, ["src/indicator"])
            watched_root = root_path / "src"
            raw_element = search_path.SimpleRawElement(str(root_path / "src"))
            unwatched_dependency = configuration.UnwatchedDependency(
                change_indicator="indicator",
                files=configuration.UnwatchedFiles(
                    root=str(root_path / "unwatched_root"), checksum_path="CHECKSUMS"
                ),
            )
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.Configuration(
                            global_root=root_path / "project",
                            dot_pyre_directory=(root_path / ".pyre"),
                            source_directories=[raw_element],
                            unwatched_dependency=unwatched_dependency,
                        )
                    ),
                    artifact_root_name="irrelevant",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=None,
                    watchman_root=watched_root,
                ),
                SimpleSourcePath(
                    elements=[raw_element.to_element()],
                ),
            )

    def test_get_with_unwatched_dependency_source_path__not_watched(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project/local"])
            setup.ensure_files_exist(
                root_path, ["src/indicator", "unwatched_root/CHECKSUMS"]
            )
            raw_element = search_path.SimpleRawElement(str(root_path / "src"))
            unwatched_dependency = configuration.UnwatchedDependency(
                change_indicator="indicator",
                files=configuration.UnwatchedFiles(
                    root=str(root_path / "unwatched_root"), checksum_path="CHECKSUMS"
                ),
            )
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.Configuration(
                            global_root=root_path / "project",
                            relative_local_root="local",
                            dot_pyre_directory=(root_path / ".pyre"),
                            source_directories=[raw_element],
                            unwatched_dependency=unwatched_dependency,
                        )
                    ),
                    artifact_root_name="irrelevant",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=None,
                    watchman_root=None,
                ),
                SimpleSourcePath(
                    elements=[raw_element.to_element()],
                ),
            )

    def test_get_buck_source_path__global(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "buck_root"])
            setup.ensure_files_exist(root_path, ["buck_root/.buckconfig"])
            setup.write_configuration_file(
                root_path / "buck_root",
                {
                    "targets": ["//ct:marle", "//ct:lucca"],
                    "buck_mode": "opt",
                    "isolation_prefix": ".lsp",
                },
            )
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.create_configuration(
                            command_arguments.CommandArguments(
                                dot_pyre_directory=root_path / ".pyre",
                            ),
                            root_path / "buck_root",
                        )
                    ),
                    artifact_root_name="artifact_root",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=None,
                    watchman_root=None,
                ),
                BuckSourcePath(
                    source_root=root_path / "buck_root",
                    artifact_root=root_path / ".pyre" / "artifact_root",
                    checked_directory=root_path / "buck_root",
                    targets=["//ct:marle", "//ct:lucca"],
                    mode="opt",
                    isolation_prefix=".lsp",
                    kill_buck_after_build=False,
                ),
            )

    def test_get_buck2_source_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "repo_root"])
            setup.ensure_files_exist(
                root_path, ["repo_root/.buckconfig", "repo_root/buck_root/.buckconfig"]
            )
            setup.write_configuration_file(
                root_path / "repo_root" / "buck_root",
                {"targets": ["//ct:lavos"], "bxl_builder": "//ct:robo"},
            )
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.create_configuration(
                            command_arguments.CommandArguments(
                                dot_pyre_directory=root_path / ".pyre",
                                use_buck2=True,
                            ),
                            root_path / "repo_root" / "buck_root",
                        )
                    ),
                    artifact_root_name="artifact_root",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=True,
                    number_of_buck_threads=42,
                    watchman_root=None,
                ),
                BuckSourcePath(
                    source_root=root_path / "repo_root",
                    artifact_root=root_path / ".pyre" / "artifact_root",
                    checked_directory=root_path / "repo_root" / "buck_root",
                    targets=["//ct:lavos"],
                    bxl_builder="//ct:robo",
                    use_buck2=True,
                    kill_buck_after_build=True,
                    number_of_threads=42,
                ),
            )

    def test_get_buck_source_path__local(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project/local"])
            setup.ensure_files_exist(root_path, ["project/local/.buckconfig"])
            setup.write_configuration_file(
                root_path / "project",
                {
                    "buck_mode": "opt",
                    "isolation_prefix": ".lsp",
                    "bxl_builder": "//ct:robo",
                },
            )
            setup.write_configuration_file(
                root_path / "project",
                {"targets": ["//ct:chrono"]},
                relative="local",
            )
            self.assertEqual(
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.create_configuration(
                            command_arguments.CommandArguments(
                                local_configuration="local",
                                dot_pyre_directory=root_path / ".pyre",
                            ),
                            root_path / "project",
                        )
                    ),
                    artifact_root_name="artifact_root/local",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=None,
                    watchman_root=None,
                ),
                BuckSourcePath(
                    source_root=root_path / "project/local",
                    artifact_root=root_path / ".pyre" / "artifact_root" / "local",
                    checked_directory=root_path / "project/local",
                    targets=["//ct:chrono"],
                    mode="opt",
                    isolation_prefix=".lsp",
                    bxl_builder="//ct:robo",
                    kill_buck_after_build=False,
                    number_of_threads=None,
                ),
            )

    def test_get_buck_source_path__no_buck_root(self) -> None:
        # Specify an explicit base directory to make sure the content of parent
        # directories will not intervene.
        with tempfile.TemporaryDirectory(dir="/tmp") as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project"])
            with self.assertRaises(configuration.InvalidConfiguration):
                get_source_path(
                    frontend_configuration.OpenSource(
                        configuration.Configuration(
                            global_root=root_path / "project",
                            dot_pyre_directory=(root_path / ".pyre"),
                            targets=["//ct:frog"],
                        )
                    ),
                    artifact_root_name="irrelevant",
                    flavor=identifiers.PyreFlavor.CLASSIC,
                    kill_buck_after_build=False,
                    number_of_buck_threads=None,
                    watchman_root=None,
                )

    def test_get_source_path__no_source_specified(self) -> None:
        with self.assertRaises(configuration.InvalidConfiguration):
            get_source_path(
                frontend_configuration.OpenSource(
                    configuration.Configuration(
                        global_root=Path("project"),
                        dot_pyre_directory=Path(".pyre"),
                        source_directories=None,
                        targets=None,
                    )
                ),
                artifact_root_name="irrelevant",
                flavor=identifiers.PyreFlavor.CLASSIC,
                kill_buck_after_build=False,
                number_of_buck_threads=None,
                watchman_root=None,
            )

    def test_get_source_path__confliciting_source_specified(self) -> None:
        with self.assertRaises(configuration.InvalidConfiguration):
            get_source_path(
                frontend_configuration.OpenSource(
                    configuration.Configuration(
                        global_root=Path("project"),
                        dot_pyre_directory=Path(".pyre"),
                        source_directories=[search_path.SimpleRawElement("src")],
                        targets=["//ct:ayla"],
                    )
                ),
                artifact_root_name="irrelevant",
                flavor=identifiers.PyreFlavor.CLASSIC,
                kill_buck_after_build=False,
                number_of_buck_threads=None,
                watchman_root=None,
            )

    def test_get_checked_directory_for_simple_source_path(self) -> None:
        with tempfile.TemporaryDirectory() as temp_root:
            Path.mkdir(Path(f"{temp_root}/super"))
            Path.mkdir(Path(f"{temp_root}/super/slash"))

            element0 = search_path.SimpleElement("ozzie")
            element1 = search_path.SubdirectoryElement("diva", "flea")
            element2 = search_path.SitePackageElement(f"{temp_root}/super", "slash")

            self.assertCountEqual(
                SimpleSourcePath(
                    [element0, element1, element2, element0]
                ).get_checked_directory_allowlist(),
                [element0.path(), element1.path(), element2.path()],
            )

    def test_get_checked_directory_for_buck_source_path(self) -> None:
        self.assertCountEqual(
            BuckSourcePath(
                source_root=Path("/source"),
                artifact_root=Path("/artifact"),
                checked_directory=Path("/source/ct"),
                targets=[
                    "//ct:robo",
                    "//ct:magus",
                    "future//ct/guardia/...",
                    "//ct/guardia:schala",
                ],
            ).get_checked_directory_allowlist(),
            ["/source/ct"],
        )

    def test_checked_directory_allowlist(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, ["a", "b/c"])

            test_configuration = frontend_configuration.OpenSource(
                configuration.Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    only_check_paths=[
                        str(root_path / "a"),
                        str(root_path / "b" / "c"),
                    ],
                )
            )
            self.assertCountEqual(
                get_checked_directory_allowlist(
                    test_configuration,
                    SimpleSourcePath([search_path.SimpleElement("source")]),
                ),
                [
                    str(root_path / "a"),
                    str(root_path / "b/c"),
                ],
            )

            test_configuration = frontend_configuration.OpenSource(
                configuration.Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    only_check_paths=[
                        str(root_path / "a"),
                        str(root_path / "b" / "c"),
                    ],
                )
            )
            self.assertCountEqual(
                get_checked_directory_allowlist(
                    test_configuration,
                    SimpleSourcePath([search_path.SimpleElement(str(root_path))]),
                ),
                [
                    str(root_path / "a"),
                    str(root_path / "b/c"),
                ],
            )

            test_configuration = frontend_configuration.OpenSource(
                configuration.Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    only_check_paths=[],
                )
            )
            self.assertCountEqual(
                get_checked_directory_allowlist(
                    test_configuration,
                    SimpleSourcePath([search_path.SimpleElement(str(root_path))]),
                ),
                [str(root_path)],
            )
