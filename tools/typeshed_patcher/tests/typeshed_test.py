# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import shutil
import tempfile
from pathlib import Path

import testslide

from ..typeshed import (
    FileBackedTypeshed,
    MemoryBackedTypeshed,
    PatchedTypeshed,
    write_to_files,
    write_to_zip,
    ZipBackedTypeshed,
)


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

    def test_zip_backed_typeshed(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            source_path = root_path / "src"
            (source_path / "foo").mkdir(parents=True)
            (source_path / "foo" / "bar.pyi").write_text("doom")
            (source_path / "baz.pyi").write_text("ripandtear")
            shutil.make_archive(
                str(root_path / "test"), format="zip", root_dir=source_path
            )

            path0 = Path("foo/bar.pyi")
            path1 = Path("baz.pyi")
            typeshed = ZipBackedTypeshed(root_path / "test.zip")
            self.assertCountEqual(typeshed.all_files(), [path0, path1])
            self.assertEqual(typeshed.get_file_content(path0), "doom")
            self.assertEqual(typeshed.get_file_content(path1), "ripandtear")
            self.assertIsNone(typeshed.get_file_content(Path("doesnotexist")))

    def test_patched_typeshed(self) -> None:
        path0 = Path("foo/bar.pyi")
        path1 = Path("baz.pyi")
        path2 = Path("foo/qux.pyi")
        base_typeshed = MemoryBackedTypeshed({path0: "doom", path1: "ripandtear"})
        patched_typeshed = PatchedTypeshed(
            base_typeshed,
            {
                path0: "eternal",
                path1: None,
                path2: "bfg",
            },
        )

        self.assertCountEqual(patched_typeshed.all_files(), [path0, path2])
        self.assertEqual(patched_typeshed.get_file_content(path0), "eternal")
        self.assertIsNone(patched_typeshed.get_file_content(path1))
        self.assertEqual(patched_typeshed.get_file_content(path2), "bfg")
        self.assertIsNone(patched_typeshed.get_file_content(Path("doesnotexist")))

    def test_write_to_files(self) -> None:
        path0 = Path("foo/bar.pyi")
        path1 = Path("baz.pyi")
        with tempfile.TemporaryDirectory() as root:
            target_path = Path(root) / "target"
            write_to_files(
                MemoryBackedTypeshed({path0: "doom", path1: "ripandtear"}), target_path
            )

            typeshed = FileBackedTypeshed(target_path)
            self.assertCountEqual(typeshed.all_files(), [path0, path1])
            self.assertEqual(typeshed.get_file_content(path0), "doom")
            self.assertEqual(typeshed.get_file_content(path1), "ripandtear")
            self.assertIsNone(typeshed.get_file_content(Path("doesnotexist")))

    def test_write_to_zip(self) -> None:
        path0 = Path("foo/bar.pyi")
        path1 = Path("baz.pyi")
        with tempfile.TemporaryDirectory() as root:
            zip_path = Path(root) / "test.zip"
            write_to_zip(
                MemoryBackedTypeshed({path0: "doom", path1: "ripandtear"}), zip_path
            )

            typeshed = ZipBackedTypeshed(zip_path)
            self.assertCountEqual(typeshed.all_files(), [path0, path1])
            self.assertEqual(typeshed.get_file_content(path0), "doom")
            self.assertEqual(typeshed.get_file_content(path1), "ripandtear")
            self.assertIsNone(typeshed.get_file_content(Path("doesnotexist")))
