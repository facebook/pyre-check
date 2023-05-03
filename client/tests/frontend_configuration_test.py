#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import shutil
import tempfile
from pathlib import Path
from typing import Optional

import testslide

from .. import (
    configuration as configuration_module,
    find_directories,
    frontend_configuration,
)
from ..configuration.search_path import SimpleElement, SimpleRawElement
from ..tests.setup import ensure_directories_exists, switch_environment


class FrontendConfigurationTest(testslide.TestCase):
    def test_dot_pyre_directory(self) -> None:
        self.assertEqual(
            frontend_configuration.OpenSource(
                configuration_module.Configuration(
                    global_root=Path("foo"), dot_pyre_directory=Path(".pyre")
                )
            ).get_dot_pyre_directory(),
            Path(".pyre"),
        )
        self.assertEqual(
            frontend_configuration.OpenSource(
                configuration_module.Configuration(
                    global_root=Path("foo"), dot_pyre_directory=None
                )
            ).get_dot_pyre_directory(),
            Path("foo") / find_directories.LOG_DIRECTORY,
        )

    def test_log_directory(self) -> None:
        def assert_log_directory(
            expected: Path,
            dot_pyre_directory: Path,
            relative_local_root: Optional[str] = None,
        ) -> None:
            self.assertEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("foo"),
                        dot_pyre_directory=dot_pyre_directory,
                        relative_local_root=relative_local_root,
                    )
                ).get_log_directory(),
                expected,
            )

        assert_log_directory(dot_pyre_directory=Path(".pyre"), expected=Path(".pyre"))
        assert_log_directory(
            dot_pyre_directory=Path(".pyre"),
            relative_local_root="bar",
            expected=Path(".pyre/bar"),
        )
        assert_log_directory(
            dot_pyre_directory=Path(".pyre"),
            relative_local_root="bar/baz",
            expected=Path(".pyre/bar/baz"),
        )

    def test_get_binary_from_configuration(self) -> None:
        with switch_environment({}):
            self.assertEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        binary="foo",
                    )
                ).get_binary_location(),
                Path("foo"),
            )

    def test_get_binary_auto_determined(self) -> None:
        self.mock_callable(shutil, "which").for_call(
            find_directories.BINARY_NAME
        ).to_return_value("foo").and_assert_called_once()

        with switch_environment({}):
            self.assertEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        binary=None,
                    )
                ).get_binary_location(),
                Path("foo"),
            )

    def test_get_binary_cannot_auto_determine(self) -> None:
        self.mock_callable(shutil, "which").to_return_value(None).and_assert_called()

        with switch_environment({}):
            self.assertIsNone(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        binary=None,
                    )
                ).get_binary_location(),
            )

    def test_typeshed_existent_search_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a"])
            ensure_directories_exists(
                root_path, ["typeshed/stdlib", "typeshed/stubs/foo"]
            )

            self.assertListEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        search_path=[
                            SimpleRawElement(str(root_path / "a")),
                        ],
                        typeshed=str(root_path / "typeshed"),
                    )
                ).get_existent_typeshed_search_paths(),
                [
                    SimpleElement(str(root_path / "typeshed/stdlib")),
                    SimpleElement(str(root_path / "typeshed/stubs/foo")),
                ],
            )

    def test_existent_search_path_with_typeshed(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a"])
            ensure_directories_exists(
                root_path, ["typeshed/stdlib", "typeshed/stubs/foo"]
            )

            self.assertListEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        search_path=[
                            SimpleRawElement(str(root_path / "a")),
                        ],
                        typeshed=str(root_path / "typeshed"),
                    )
                ).get_existent_search_paths(),
                [
                    SimpleElement(str(root_path / "a")),
                    SimpleElement(str(root_path / "typeshed/stdlib")),
                    SimpleElement(str(root_path / "typeshed/stubs/foo")),
                ],
            )

    def test_get_typeshed_from_configuration(self) -> None:
        with switch_environment({}):
            self.assertEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        typeshed="foo",
                    )
                ).get_typeshed_location(),
                Path("foo"),
            )

    def test_get_typeshed_auto_determined(self) -> None:
        self.mock_callable(
            find_directories, "find_typeshed"
        ).for_call().to_return_value(Path("foo")).and_assert_called_once()

        with switch_environment({}):
            self.assertEqual(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        typeshed=None,
                    )
                ).get_typeshed_location(),
                Path("foo"),
            )

    def test_get_typeshed_cannot_auto_determine(self) -> None:
        self.mock_callable(
            find_directories, "find_typeshed"
        ).for_call().to_return_value(None).and_assert_called_once()

        with switch_environment({}):
            self.assertIsNone(
                frontend_configuration.OpenSource(
                    configuration_module.Configuration(
                        global_root=Path("irrelevant"),
                        dot_pyre_directory=Path(".pyre"),
                        typeshed=None,
                    )
                ).get_typeshed_location(),
            )
