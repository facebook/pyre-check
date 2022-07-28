# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from ..typeshed import FileBackedTypeshed, MemoryBackedTypeshed


class TypeshedReaderTest(testslide.TestCase):
    def test_memory_backed_typeshed(self) -> None:
        path0 = Path("foo/bar.pyi")
        path1 = Path("baz.pyi")
        typeshed = MemoryBackedTypeshed({path0: "doom", path1: "ripandtear"})
        self.assertCountEqual(typeshed.all_files(), [path0, path1])
        self.assertEqual(typeshed.get_file_content(path0), "doom")
        self.assertEqual(typeshed.get_file_content(path1), "ripandtear")
        self.assertIsNone(typeshed.get_file_content(Path("doesnotexist")))

    def test_file_backed_typeshed(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            (root_path / "foo").mkdir()
            (root_path / "foo" / "bar.pyi").write_text("doom")
            (root_path / "baz.pyi").write_text("ripandtear")

            path0 = Path("foo/bar.pyi")
            path1 = Path("baz.pyi")
            typeshed = FileBackedTypeshed(root_path)
            self.assertCountEqual(typeshed.all_files(), [path0, path1])
            self.assertEqual(typeshed.get_file_content(path0), "doom")
            self.assertEqual(typeshed.get_file_content(path1), "ripandtear")
            self.assertIsNone(typeshed.get_file_content(Path("doesnotexist")))
