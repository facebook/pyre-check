# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import fcntl
import os
import subprocess
import tempfile
import unittest
from typing import Dict  # noqa
from unittest.mock import call, patch

from ..exceptions import EnvironmentException  # noqa
from ..filesystem import (  # noqa
    Filesystem,
    MercurialBackedFilesystem,
    SharedSourceDirectory,
    __name__ as filesystem_name,
    _find_python_paths,
    acquire_lock,
    remove_if_exists,
)


class FilesystemTest(unittest.TestCase):
    def test_find_python_paths(self) -> None:
        root = tempfile.mkdtemp()

        def create_file(name: str) -> None:
            with open(os.path.join(root, name), "w+"):
                pass

        def create_symlink(target: str, source: str) -> None:
            os.symlink(os.path.join(root, target), os.path.join(root, source))

        create_file("a.py")
        create_file("b.pyi")
        create_file("c.cpp")
        create_symlink("a.py", "link1.py")
        create_symlink("dangling.py", "link2.py")
        create_symlink("c.cpp", "link3.py")
        create_symlink("a.py", "link4.cpp")
        os.mkdir(os.path.join(root, "mypy"))
        os.mkdir(os.path.join(root, "scipyi"))
        os.mkdir(os.path.join(root, "spy.py"))
        create_symlink("spy.py", "directory_symlink.py")
        create_file("mypy/my.py")
        create_file("scipyi/sci.pyi")
        create_symlink("mypy/my.py", "mypy/another.pyi")
        create_symlink("scipyi/sci.pyi", "scipyi/another.py")
        actual_paths = sorted(
            os.path.relpath(path, root) for path in _find_python_paths(root)
        )
        self.assertEqual(
            actual_paths,
            [
                "a.py",
                "b.pyi",
                "directory_symlink.py",
                "link1.py",
                "link2.py",
                "link3.py",
                "mypy/another.pyi",
                "mypy/my.py",
                "scipyi/another.py",
                "scipyi/sci.pyi",
            ],
        )

    def test_merge_source_directory(self) -> None:
        root = os.path.realpath(tempfile.mkdtemp())

        def create_file(name: str) -> None:
            with open(os.path.join(root, name), "w+"):
                pass

        def create_symlink(target: str, source: str) -> None:
            os.symlink(os.path.join(root, target), os.path.join(root, source))

        create_file("a.py")
        create_file("b.pyi")
        create_file("c.cpp")
        create_symlink("a.py", "link1.py")
        create_symlink("dangling.py", "link2.py")
        create_symlink("c.cpp", "link3.py")
        create_symlink("a.py", "link4.cpp")
        os.mkdir(os.path.join(root, "mypy"))
        os.mkdir(os.path.join(root, "scipyi"))
        os.mkdir(os.path.join(root, "spy.py"))
        create_symlink("spy.py", "directory_symlink.py")
        create_file("mypy/my.py")
        create_file("scipyi/sci.pyi")
        create_symlink("mypy/my.py", "mypy/another.pyi")
        create_symlink("scipyi/sci.pyi", "scipyi/another.py")
        shared_source_directory = SharedSourceDirectory([root])
        all_paths = {}  # type: Dict[str, str]
        shared_source_directory._merge_source_directory(root, all_paths)
        self.assertEqual(
            all_paths,
            {
                "a.py": os.path.join(root, "a.py"),
                "b.pyi": os.path.join(root, "b.pyi"),
                "link1.py": os.path.join(root, "a.py"),
                "link3.py": os.path.join(root, "c.cpp"),
                "mypy/another.pyi": os.path.join(root, "mypy/my.py"),
                "mypy/my.py": os.path.join(root, "mypy/my.py"),
                "scipyi/another.py": os.path.join(root, "scipyi/sci.pyi"),
                "scipyi/sci.pyi": os.path.join(root, "scipyi/sci.pyi"),
            },
        )

    @patch("{}.is_empty".format(filesystem_name))
    @patch("os.symlink")
    @patch("subprocess.check_output")
    @patch("os.makedirs")
    @patch("os.path.exists")
    @patch("os.path.realpath")
    def test_merge(
        self,
        os_path_realpath,
        os_path_exists,
        os_makedirs,
        check_output,
        os_symlink,
        is_empty,
    ) -> None:
        is_empty.return_value = False
        os_path_exists.return_value = False
        root = tempfile.mkdtemp()
        os.mkdir(os.path.join(root, "first"))
        os.mkdir(os.path.join(root, "first", "b"))
        os.mkdir(os.path.join(root, "second"))

        def create_file(name: str) -> None:
            with open(name, "w+"):
                pass

        create_file(os.path.join(root, "first", "x.py"))
        create_file(os.path.join(root, "first", "y.py"))
        create_file(os.path.join(root, "first", "b", "z.py"))
        create_file(os.path.join(root, "second", "a.py"))

        def side_effect(path, stderr=None):
            if path[1].endswith("first"):
                serialized = "\n".join(
                    [
                        os.path.join(root, path)
                        for path in ["first/x.py", "first/y.py", "first/b/z.py"]
                    ]
                )
            else:
                serialized = os.path.join(root, "second/a.py")
            return bytes(serialized, "utf-8")

        check_output.side_effect = side_effect
        os_path_realpath.side_effect = lambda x: x
        shared_source_directory = SharedSourceDirectory(
            [os.path.join(root, "first"), os.path.join(root, "second")]
        )
        shared_source_directory._merge()
        shared_root = shared_source_directory.get_root()
        os_makedirs.assert_has_calls([call(shared_root), call(shared_root + "/b")])
        os_symlink.assert_has_calls(
            [
                call(root + "/first/x.py", shared_root + "/x.py"),
                call(root + "/first/y.py", shared_root + "/y.py"),
                call(root + "/first/b/z.py", shared_root + "/b/z.py"),
                call(root + "/second/a.py", shared_root + "/a.py"),
            ],
            any_order=True,
        )

    def test_remove_if_exists(self) -> None:
        # File removal.
        with patch("os.remove") as os_remove, patch("shutil.rmtree") as shutil_rmtree:
            os_remove.side_effect = OSError()
            remove_if_exists("path")
            os_remove.assert_called_once_with("path")
            shutil_rmtree.assert_called_once_with("path")

        # Directory removal.
        with patch("os.remove") as os_remove, patch("shutil.rmtree") as shutil_rmtree:
            shutil_rmtree.side_effect = OSError()
            remove_if_exists("path")
            os_remove.assert_called_once_with("path")
            shutil_rmtree.assert_called_once_with("path")

        # Both throw.
        with patch("os.remove") as os_remove, patch("shutil.rmtree") as shutil_rmtree:
            os_remove.side_effect = FileNotFoundError()
            shutil_rmtree.side_effect = OSError()
            remove_if_exists("path")
            os_remove.assert_called_once_with("path")
            shutil_rmtree.assert_called_once_with("path")

    @patch("fcntl.lockf")
    def test_acquire_lock(self, lock_file: unittest.mock.Mock) -> None:
        (_, path) = tempfile.mkstemp()
        lockfile_file_descriptor = None
        with acquire_lock(path, blocking=False) as file_descriptor:
            lockfile_file_descriptor = file_descriptor

        with acquire_lock(path, blocking=True):
            pass
        lock_file.assert_has_calls(
            [
                call(lockfile_file_descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB),
                call(lockfile_file_descriptor, fcntl.LOCK_UN),
                call(lockfile_file_descriptor, fcntl.LOCK_EX),
                call(lockfile_file_descriptor, fcntl.LOCK_UN),
            ]
        )

        def fail_on_exclusive(_, lock_kind):
            if lock_kind == fcntl.LOCK_EX | fcntl.LOCK_NB:
                raise OSError()
            return None

        lock_file.side_effect = fail_on_exclusive
        with self.assertRaises(OSError):
            with acquire_lock(path, blocking=False):
                pass

    @patch("shutil.rmtree")
    def test_cleanup(self, rmtree) -> None:
        shared_source_directory = SharedSourceDirectory(["first", "second"])
        shared_source_directory.cleanup()
        rmtree.assert_not_called()

        shared_source_directory = SharedSourceDirectory(["first", "second"], True)
        shared_source_directory.cleanup()
        rmtree.assert_called_with(shared_source_directory.get_root())

    @patch.object(subprocess, "check_output")
    def test_filesystem_list(self, check_output):
        filesystem = Filesystem()
        filesystem.list(".", ".pyre_configuration.local")

        filesystem = MercurialBackedFilesystem()
        filesystem.list(".", ".pyre_configuration.local")

        def fail_on_mercurial(arguments, **kwargs):
            if "hg" in arguments:
                raise FileNotFoundError
            else:
                return "external/a/.pyre_configuration.local".encode("utf-8")

        check_output.side_effect = fail_on_mercurial
        with self.assertRaises(EnvironmentException):
            filesystem.list(".", ".pyre_configuration.local")
        check_output.assert_has_calls(
            [
                call(["find", ".", "-name", "*.pyre_configuration.local"]),
                call().decode("utf-8"),
                call().decode().split(),
                call(
                    ["hg", "files", "--include", "**.pyre_configuration.local"],
                    stderr=subprocess.DEVNULL,
                ),
                call().decode("utf-8"),
                call().decode().split(),
                call(
                    ["hg", "files", "--include", "**.pyre_configuration.local"],
                    stderr=subprocess.DEVNULL,
                ),
            ]
        )

    @patch("os.getcwd")
    @patch.object(subprocess, "check_output")
    def test_get_scratch_directory(self, check_output, getcwd):
        check_output.side_effect = FileNotFoundError
        getcwd.return_value = "default"
        shared_source_directory = SharedSourceDirectory(["first", "second"])

        directory = shared_source_directory.get_scratch_directory()
        self.assertEqual(directory, "default/.pyre")

        root = shared_source_directory.get_root()
        self.assertEqual(root, "default/.pyre/shared_source_directory")

        check_output.side_effect = None
        check_output.return_value = "/scratch\n".encode("utf-8")
        shared_source_directory = SharedSourceDirectory(["first", "second"])
        directory = shared_source_directory.get_scratch_directory()
        self.assertEqual(directory, "/scratch")

        root = shared_source_directory.get_root()
        self.assertEqual(root, "/scratch/shared_source_directory")
