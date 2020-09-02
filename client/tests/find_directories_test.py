# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import tempfile
import unittest
from pathlib import Path
from typing import Iterable, Optional, Tuple, Union

from ..find_directories import (
    FoundRoot,
    find_global_and_local_root,
    find_global_root,
    find_parent_directory_containing_directory,
    find_parent_directory_containing_file,
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


class FindParentDirectoryContainingDirectoryTest(unittest.TestCase):
    def assert_find_parent_directory_containing_directory(
        self, files: Iterable[str], base: str, target: str, expected: Optional[str]
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)
            self.assertEqual(
                find_parent_directory_containing_directory(root_path / base, target),
                (root_path / expected) if expected is not None else None,
            )

    def test_find_parent_directory_containing_directory(self) -> None:
        self.assert_find_parent_directory_containing_directory(
            files=[], base=".", target="a", expected=None
        )

        self.assert_find_parent_directory_containing_directory(
            files=["a", "b/c"], base="b", target="a", expected=None
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a", "b/c"], base="b", target="b", expected="."
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a", "b/c"], base="b", target="c", expected=None
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a", "b/c"], base="b", target="d", expected=None
        )

        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d"], base="a", target="b", expected=None
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d"], base="a", target="c", expected="a"
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d"], base="a/c", target="b", expected=None
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d"], base="a/c", target="c", expected="a"
        )

        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/b/c", "a/d/e/b/c"], base=".", target="b", expected=None
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/b/c", "a/d/e/b/c"], base="a", target="b", expected="a"
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/b/c", "a/d/e/b/c"],
            base="a/d",
            target="b",
            expected="a/d",
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/b/c", "a/d/e/b/c"],
            base="a/d/e",
            target="b",
            expected="a/d/e",
        )

        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/a/b/c"], base="a/d/a/b/c", target="a/b", expected="a/d"
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/a/b/c"], base="a/d/a/b", target="a/b", expected="a/d"
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/a/b/c"], base="a/d/a", target="a/b", expected="a/d"
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/a/b/c"], base="a/d", target="a/b", expected="a/d"
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b/c", "a/d/a/b/c"], base="a", target="a/b", expected="."
        )

        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a",
            target="d",
            expected=None,
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a/c",
            target="d",
            expected="a/c",
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a/f",
            target="d",
            expected="a/f",
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a/g",
            target="d",
            expected=None,
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a/c/d",
            target="c/d",
            expected="a",
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a/f/d",
            target="c/d",
            expected="a",
        )
        self.assert_find_parent_directory_containing_directory(
            files=["a/b", "a/c/d/e", "a/f/d/e", "a/g/e"],
            base="a/g",
            target="d/e",
            expected=None,
        )


class FindGlobalRootTest(unittest.TestCase):
    def assert_find_global_root(
        self, files: Iterable[str], base: str, expected: Optional[str]
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)
            self.assertEqual(
                find_global_root(root_path / base),
                (root_path / expected) if expected is not None else None,
            )

    def test_find_global_root(self) -> None:
        self.assert_find_global_root(
            files=["a/b/.pyre_configuration", "a/b/c/d"], base="a/b/c", expected="a/b"
        )
        self.assert_find_global_root(
            files=["a/b/c", "a/b/d/e"], base="a/b/d", expected=None
        )


class FindGlobalAndLocalRootTest(unittest.TestCase):
    @staticmethod
    def to_found_root(
        root_path: Path, expected: Union[None, str, Tuple[str, str]]
    ) -> Optional[FoundRoot]:
        if expected is None:
            return None
        elif isinstance(expected, str):
            return FoundRoot(root_path / expected)
        elif isinstance(expected, tuple):
            return FoundRoot(root_path / expected[0], root_path / expected[1])
        else:
            raise RuntimeError("Malformed argument passed to `expected`")

    def assert_find_roots(
        self,
        files: Iterable[str],
        base: str,
        expected: Union[None, str, Tuple[str, str]],
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)

            self.assertEqual(
                find_global_and_local_root(root_path / base),
                self.to_found_root(root_path, expected),
            )

    def test_find_global_and_local_root(self) -> None:
        self.assert_find_roots(files=["a/b/c"], base="a/b", expected=None)
        self.assert_find_roots(
            files=["a/.pyre_configuration", "a/b/c"], base="a/b", expected="a"
        )
        self.assert_find_roots(
            files=["a/.pyre_configuration.local", "a/b/c"], base="a/b", expected=None
        )

        self.assert_find_roots(
            files=[".pyre_configuration", "a/.pyre_configuration.local", "a/b/c"],
            base="a/b",
            expected=(".", "a"),
        )
        self.assert_find_roots(
            files=[".pyre_configuration", "a/.pyre_configuration", "a/b/c"],
            base="a/b",
            expected="a",
        )
        self.assert_find_roots(
            files=[".pyre_configuration.local", "a/.pyre_configuration", "a/b/c"],
            base="a/b",
            expected="a",
        )
        self.assert_find_roots(
            files=[".pyre_configuration.local", "a/.pyre_configuration.local", "a/b/c"],
            base="a/b",
            expected=None,
        )

        self.assert_find_roots(
            files=["a/.pyre_configuration", "a/b/.pyre_configuration.local", "a/c/d"],
            base="a/c",
            expected="a",
        )
        self.assert_find_roots(
            files=["a/.pyre_configuration", "a/b/.pyre_configuration.local", "a/c/d"],
            base="a/b",
            expected=("a", "a/b"),
        )
        self.assert_find_roots(
            files=["a/.pyre_configuration", "a/b/.pyre_configuration.local", "a/c/d"],
            base="a",
            expected="a",
        )
        self.assert_find_roots(
            files=["a/.pyre_configuration", "a/b/.pyre_configuration.local", "a/c/d"],
            base=".",
            expected=None,
        )
