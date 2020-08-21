# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import tempfile
import unittest
from pathlib import Path
from typing import Iterable, Optional

from ..find_directories import (
    find_local_root,
    find_parent_directory_containing_file,
    find_project_root,
)


def _ensure_files_exist(root: Path, relatives: Iterable[str]) -> None:
    for relative in relatives:
        full_path = root / relative
        full_path.parent.mkdir(parents=True, exist_ok=True)
        full_path.touch(exist_ok=True)


class FindParentDirectoryContainingFileTest(unittest.TestCase):
    def assert_find_parent_directory_containing_file(
        self, files: Iterable[str], base: str, target: str, expected: Optional[str]
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)
            self.assertEqual(
                find_parent_directory_containing_file(root_path / base, target),
                (root_path / expected) if expected is not None else None,
            )

    def test_find_parent_directory_containing_file(self) -> None:
        self.assert_find_parent_directory_containing_file(
            files=[], base=".", target="a", expected=None
        )

        self.assert_find_parent_directory_containing_file(
            files=["a"], base=".", target="a", expected="."
        )
        self.assert_find_parent_directory_containing_file(
            files=["a"], base=".", target="b", expected=None
        )

        self.assert_find_parent_directory_containing_file(
            files=["a", "b/c"], base="b", target="a", expected="."
        )
        self.assert_find_parent_directory_containing_file(
            files=["a", "b/c"], base="b", target="b", expected=None
        )
        self.assert_find_parent_directory_containing_file(
            files=["a", "b/c"], base="b", target="c", expected="b"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a", "b/c"], base="b", target="d", expected=None
        )

        self.assert_find_parent_directory_containing_file(
            files=["a/b", "a/c/d"], base="a", target="b", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/b", "a/c/d"], base="a", target="c", expected=None
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/b", "a/c/d"], base="a/c", target="b", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/b", "a/c/d"], base="a/c", target="d", expected="a/c"
        )

        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/d", "a/b/c/d"], base=".", target="d", expected=None
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/d", "a/b/c/d"], base="a", target="d", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/d", "a/b/c/d"], base="a", target="d", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/d", "a/b/c/d"], base="a/b", target="d", expected="a/b"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/d", "a/b/c/d"],
            base="a/b/c",
            target="d",
            expected="a/b/c",
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/d", "a/b/c/d"],
            base="a/b/c/d",
            target="d",
            expected="a/b/c",
        )

        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/e", "a/b/c/f"], base="a", target="d", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/e", "a/b/c/f"], base="a/b", target="d", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/e", "a/b/c/f"], base="a/b/c", target="d", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/e", "a/b/c/f"], base="a/b/e", target="d", expected="a"
        )
        self.assert_find_parent_directory_containing_file(
            files=["a/d", "a/b/e", "a/b/c/f"], base="a/b/c/f", target="d", expected="a"
        )


class FindProjectRootTest(unittest.TestCase):
    def assert_find_project_root(
        self, files: Iterable[str], base: str, expected: Optional[str]
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)
            self.assertEqual(
                find_project_root(str(root_path / base)),
                str(root_path / expected) if expected is not None else None,
            )

    def test_find_project_root(self) -> None:
        self.assert_find_project_root(
            files=["a/b/.pyre_configuration", "a/b/c/d"], base="a/b/c", expected="a/b"
        )
        self.assert_find_project_root(
            files=["a/b/c", "a/b/d/e"], base="a/b/d", expected="a/b/d"
        )


class FindLocalRootTest(unittest.TestCase):
    def assert_find_local_root(
        self, files: Iterable[str], base: str, expected: Optional[str]
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)
            self.assertEqual(
                find_local_root(str(root_path / base)),
                str(root_path / expected) if expected is not None else None,
            )

    def test_find_local_root(self) -> None:
        self.assert_find_local_root(
            files=["a/b/.pyre_configuration.local", "a/b/c/d"],
            base="a/b/c",
            expected="a/b",
        )
        self.assert_find_local_root(
            files=["a/b/c", "a/b/d/e"], base="a/b/d", expected=None
        )
        self.assert_find_local_root(
            files=["a/b/.pyre_configuration", "a/b/c/d"], base="a/b/c", expected=None
        )
        self.assert_find_local_root(
            files=["a/b/.pyre_configuration", "a/.pyre_configuration.local", "a/b/c/d"],
            base="a/b/c",
            expected=None,
        )
