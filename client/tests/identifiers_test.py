# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import Optional

import testslide

from .. import identifiers


class ProjectIdentifierTest(testslide.TestCase):
    def test_project_identifier(self) -> None:
        def assert_project_identifier(
            global_root: Path,
            relative_local_root: Optional[str],
            expected: str,
        ) -> None:
            self.assertEqual(
                identifiers.get_project_identifier(
                    global_root,
                    relative_local_root,
                ),
                expected,
            )

        assert_project_identifier(
            global_root=Path("project"),
            relative_local_root=None,
            expected="project",
        )
        assert_project_identifier(
            global_root=Path("my/project"),
            relative_local_root=None,
            expected="my/project",
        )
        assert_project_identifier(
            global_root=Path("my/project"),
            relative_local_root="foo",
            expected="my/project//foo",
        )
        assert_project_identifier(
            global_root=Path("my/project"),
            relative_local_root="foo/bar",
            expected="my/project//foo/bar",
        )

    def test_simple_name(self) -> None:
        def assert_simple_name(
            flavor: identifiers.PyreFlavor,
            expected: str,
        ) -> None:
            self.assertEqual(
                flavor.simple_name(),
                expected,
            )

        assert_simple_name(identifiers.PyreFlavor.CLASSIC, "Type Checking")
        assert_simple_name(identifiers.PyreFlavor.CODE_NAVIGATION, "Language Services")
        self.assertRaises(
            identifiers.IllegalFlavorException,
            identifiers.PyreFlavor.SHADOW.simple_name,
        )
