#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import subprocess
from pathlib import Path
from typing import Optional

import testslide

from .. import (
    configuration as configuration_module,
    find_directories,
    frontend_configuration,
)


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

    def test_get_binary_version_ok(self) -> None:
        binary_path = "foo"
        version = "facefacefaceb00"

        self.mock_callable(subprocess, "run").to_return_value(
            subprocess.CompletedProcess(
                args=[binary_path, "-version"], returncode=0, stdout=f"{version}\n"
            )
        ).and_assert_called_once()

        self.assertEqual(
            frontend_configuration.OpenSource(
                configuration_module.Configuration(
                    global_root=Path("irrelevant"),
                    dot_pyre_directory=Path(".pyre"),
                    binary=binary_path,
                )
            ).get_binary_version(),
            version,
        )

    def test_get_binary_version_fail(self) -> None:
        binary_path = "foo"

        self.mock_callable(subprocess, "run").to_return_value(
            subprocess.CompletedProcess(
                args=[binary_path, "-version"], returncode=1, stdout="derp"
            )
        ).and_assert_called_once()

        self.assertIsNone(
            frontend_configuration.OpenSource(
                configuration_module.Configuration(
                    global_root=Path("irrelevant"),
                    dot_pyre_directory=Path(".pyre"),
                    binary=binary_path,
                )
            ).get_binary_version()
        )
