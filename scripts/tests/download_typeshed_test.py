# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import List

from ..download_typeshed import _entry_path_to_patch_path, _find_entry, FileEntry


class EntryPathToPatchPathTest(unittest.TestCase):
    def assert_path_is(self, path: str, expected: Path) -> None:
        self.assertEqual(_entry_path_to_patch_path(path), expected)

    def test_path_is(self) -> None:
        self.assert_path_is("typeshed-master/stubs/foo.pyi", Path("stubs/foo.patch"))
        self.assert_path_is("typeshed-master", Path(""))


class FindEntryTest(unittest.TestCase):
    def assert_found_as(
        self,
        path: str,
        entries: List[FileEntry],
        expected: FileEntry,
    ) -> None:
        self.assertEqual(_find_entry(Path(path), entries), expected)

    def assert_not_found(self, path: str, entries: List[FileEntry]) -> None:
        self.assertEqual(_find_entry(Path(path), entries), None)

    decimal_entry = FileEntry("typeshed-master/stdlib/decimal.pyi", bytes([1, 5, 0]))
    chunk_entry = FileEntry("typeshed-master/stdlib/chunk.pyi", bytes([1, 0, 0]))
    core_entry = FileEntry("typeshed-master/stubs/click/click/core.pyi", bytes([5, 0]))

    no_data_entry = FileEntry("typeshed-master/stdlib/fake/fake.pyi", None)
    example_entries: List[FileEntry] = [
        decimal_entry,
        chunk_entry,
        core_entry,
        no_data_entry,
    ]

    def test_found_as(self) -> None:

        self.assert_found_as(
            path="stdlib/decimal.patch",
            entries=self.example_entries,
            expected=self.decimal_entry,
        )

        self.assert_found_as(
            path="stdlib/decimal.pyi",
            entries=self.example_entries,
            expected=self.decimal_entry,
        )

        self.assert_found_as(
            path="stdlib/decimal",
            entries=self.example_entries,
            expected=self.decimal_entry,
        )

        self.assert_found_as(
            path="stubs/click/click/core.patch",
            entries=self.example_entries,
            expected=self.core_entry,
        )

    def test_not_found(self) -> None:
        self.assert_not_found(
            path="stdlib/fake.patch",
            entries=self.example_entries,
        )
