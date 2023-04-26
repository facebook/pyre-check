#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


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
