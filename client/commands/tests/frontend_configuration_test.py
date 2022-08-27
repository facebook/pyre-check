# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import testslide

from ... import configuration
from .. import frontend_configuration


class BaseTest(testslide.TestCase):
    def test_project_identifier(self) -> None:
        def assert_project_identifier(
            client_configuration: configuration.Configuration, expected: str
        ) -> None:
            self.assertEqual(
                frontend_configuration.OpenSource(
                    client_configuration
                ).get_project_identifier(),
                expected,
            )

        assert_project_identifier(
            configuration.Configuration(
                project_root="project", dot_pyre_directory=Path(".pyre")
            ),
            "project",
        )
        assert_project_identifier(
            configuration.Configuration(
                project_root="my/project", dot_pyre_directory=Path(".pyre")
            ),
            "my/project",
        )
        assert_project_identifier(
            configuration.Configuration(
                project_root="my/project",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="foo",
            ),
            "my/project//foo",
        )
        assert_project_identifier(
            configuration.Configuration(
                project_root="my/project",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="foo/bar",
            ),
            "my/project//foo/bar",
        )
