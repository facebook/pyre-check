# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from .... import command_arguments, configuration
from ....tests import setup
from ..backend_arguments import (
    RemoteLogging,
    SimpleSourcePath,
    BuckSourcePath,
    find_watchman_root,
    find_buck_root,
    get_source_path,
)


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
                    configuration.SimpleSearchPathElement("/source0"),
                    configuration.SimpleSearchPathElement("/source1"),
                ]
            ).serialize(),
            {"kind": "simple", "paths": ["/source0", "/source1"]},
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
            },
        )
        self.assertDictEqual(
            BuckSourcePath(
                source_root=Path("/source"),
                artifact_root=Path("/artifact"),
                checked_directory=Path("/source"),
                targets=["//foo:bar"],
                mode="opt",
                isolation_prefix=".lsp",
            ).serialize(),
            {
                "kind": "buck",
                "source_root": "/source",
                "artifact_root": "/artifact",
                "targets": ["//foo:bar"],
                "mode": "opt",
                "isolation_prefix": ".lsp",
            },
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

    def test_find_buck_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_files_exist(
                root_path,
                ["foo/qux/derp", "foo/bar/.buckconfig", "foo/bar/baz/derp"],
            )

            expected_root = root_path / "foo/bar"
            self.assertEqual(find_buck_root(root_path / "foo/bar/baz"), expected_root)
            self.assertEqual(find_buck_root(root_path / "foo/bar"), expected_root)

            self.assertIsNone(find_buck_root(root_path / "foo/qux"))
            self.assertIsNone(find_buck_root(root_path / "foo"))
            self.assertIsNone(find_buck_root(root_path))

    def test_get_simple_source_path__exists(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "src"])
            element = configuration.SimpleSearchPathElement(str(root_path / "src"))
            self.assertEqual(
                get_source_path(
                    configuration.Configuration(
                        project_root=str(root_path / "project"),
                        dot_pyre_directory=(root_path / ".pyre"),
                        source_directories=[element],
                    ).expand_and_filter_nonexistent_paths(),
                    artifact_root_name="irrelevant",
                ),
                SimpleSourcePath([element]),
            )

    def test_get_simple_source_path__nonexists(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre"])
            element = configuration.SimpleSearchPathElement(str(root_path / "src"))
            self.assertEqual(
                get_source_path(
                    configuration.Configuration(
                        project_root=str(root_path / "project"),
                        dot_pyre_directory=(root_path / ".pyre"),
                        source_directories=[element],
                    ).expand_and_filter_nonexistent_paths(),
                    artifact_root_name="irrelevant",
                ),
                SimpleSourcePath([]),
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
                    configuration.create_configuration(
                        command_arguments.CommandArguments(
                            dot_pyre_directory=root_path / ".pyre",
                        ),
                        root_path / "buck_root",
                    ),
                    artifact_root_name="artifact_root",
                ),
                BuckSourcePath(
                    source_root=root_path / "buck_root",
                    artifact_root=root_path / ".pyre" / "artifact_root",
                    checked_directory=root_path / "buck_root",
                    targets=["//ct:marle", "//ct:lucca"],
                    mode="opt",
                    isolation_prefix=".lsp",
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
                },
            )
            setup.write_configuration_file(
                root_path / "project",
                {"targets": ["//ct:chrono"]},
                relative="local",
            )
            self.assertEqual(
                get_source_path(
                    configuration.create_configuration(
                        command_arguments.CommandArguments(
                            local_configuration="local",
                            dot_pyre_directory=root_path / ".pyre",
                        ),
                        root_path / "project",
                    ),
                    artifact_root_name="artifact_root/local",
                ),
                BuckSourcePath(
                    source_root=root_path / "project/local",
                    artifact_root=root_path / ".pyre" / "artifact_root" / "local",
                    checked_directory=root_path / "project/local",
                    targets=["//ct:chrono"],
                    mode="opt",
                    isolation_prefix=".lsp",
                ),
            )

    def test_get_buck_source_path__no_buck_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(root_path, [".pyre", "project"])
            with self.assertRaises(configuration.InvalidConfiguration):
                get_source_path(
                    configuration.Configuration(
                        project_root=str(root_path / "project"),
                        dot_pyre_directory=(root_path / ".pyre"),
                        targets=["//ct:frog"],
                    ).expand_and_filter_nonexistent_paths(),
                    artifact_root_name="irrelevant",
                )

    def test_get_source_path__no_source_specified(self) -> None:
        with self.assertRaises(configuration.InvalidConfiguration):
            get_source_path(
                configuration.Configuration(
                    project_root="project",
                    dot_pyre_directory=Path(".pyre"),
                    source_directories=None,
                    targets=None,
                ).expand_and_filter_nonexistent_paths(),
                artifact_root_name="irrelevant",
            )

    def test_get_source_path__confliciting_source_specified(self) -> None:
        with self.assertRaises(configuration.InvalidConfiguration):
            get_source_path(
                configuration.Configuration(
                    project_root="project",
                    dot_pyre_directory=Path(".pyre"),
                    source_directories=[configuration.SimpleSearchPathElement("src")],
                    targets=["//ct:ayla"],
                ).expand_and_filter_nonexistent_paths(),
                artifact_root_name="irrelevant",
            )

    def test_get_checked_directory_for_simple_source_path(self) -> None:
        element0 = configuration.SimpleSearchPathElement("ozzie")
        element1 = configuration.SubdirectorySearchPathElement("diva", "flea")
        element2 = configuration.SitePackageSearchPathElement("super", "slash")
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
