# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import fcntl
import os
import subprocess
import tempfile
import unittest
from contextlib import contextmanager
from typing import Dict  # noqa
from unittest.mock import call, patch

from ..exceptions import EnvironmentException  # noqa
from ..filesystem import (  # noqa
    Filesystem,
    MercurialBackedFilesystem,
    SharedAnalysisDirectory,
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

    def test_merge_into_paths(self) -> None:
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
        shared_analysis_directory = SharedAnalysisDirectory([root])
        all_paths = {}  # type: Dict[str, str]
        shared_analysis_directory._merge_into_paths(root, all_paths)
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
        shared_analysis_directory = SharedAnalysisDirectory(
            [os.path.join(root, "first"), os.path.join(root, "second")]
        )
        shared_analysis_directory._merge()
        shared_root = shared_analysis_directory.get_root()
        os_makedirs.assert_has_calls(
            [call(shared_root), call(shared_root + "/b")], any_order=True
        )
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
        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"])
        shared_analysis_directory.cleanup()
        rmtree.assert_not_called()

        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"], isolate=True
        )
        shared_analysis_directory.cleanup()
        rmtree.assert_called_with(shared_analysis_directory.get_root())

    @patch.object(subprocess, "run")
    def test_filesystem_list_bare(self, run):
        filesystem = Filesystem()
        filesystem.list(".", ".pyre_configuration.local")

        def fail_command(arguments, **kwargs):
            return subprocess.CompletedProcess(
                args=[], returncode=1, stdout="".encode("utf-8")
            )

        run.side_effect = fail_command
        self.assertEqual([], filesystem.list(".", ".pyre_configuration.local"))
        run.assert_has_calls(
            [
                call(
                    ["find", ".", "-name", "*.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                ),
                call().stdout.decode("utf-8"),
                call().stdout.decode().split(),
                call(
                    ["find", ".", "-name", "*.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                ),
            ]
        )

    @patch.object(subprocess, "run")
    def test_filesystem_list_mercurial(self, run):
        filesystem = MercurialBackedFilesystem()
        filesystem.list(".", ".pyre_configuration.local")

        def fail_command(arguments, **kwargs):
            return subprocess.CompletedProcess(
                args=[], returncode=1, stdout="".encode("utf-8")
            )

        run.side_effect = fail_command
        self.assertEqual([], filesystem.list(".", ".pyre_configuration.local"))
        run.assert_has_calls(
            [
                call(
                    ["hg", "files", "--include", "**.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.DEVNULL,
                ),
                call().stdout.decode("utf-8"),
                call().stdout.decode().split(),
                call(
                    ["hg", "files", "--include", "**.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.DEVNULL,
                ),
            ]
        )

    @patch("os.getcwd")
    @patch.object(subprocess, "check_output")
    def test_get_scratch_directory(self, check_output, getcwd):
        # No scratch, no local configuration
        check_output.side_effect = FileNotFoundError
        getcwd.return_value = "default"
        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"])

        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "default/.pyre")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "default/.pyre/shared_analysis_directory")

        # Scratch, no local configuration
        check_output.side_effect = None
        check_output.return_value = "/scratch\n".encode("utf-8")
        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"])
        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "/scratch")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "/scratch/shared_analysis_directory")

        # No scratch, using local configuration
        check_output.side_effect = FileNotFoundError
        getcwd.return_value = "default"
        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"], ["path/to/local"], "path/to/local"
        )

        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "default/.pyre")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "default/.pyre/path/to/local")

        # Scratch, using local configuration
        check_output.side_effect = None
        check_output.return_value = "/scratch\n".encode("utf-8")
        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"], ["path/to/local"], "path/to/local"
        )
        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "/scratch")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "/scratch/path/to/local")

    @patch("os.makedirs")
    @patch(filesystem_name + ".acquire_lock")
    @patch.object(SharedAnalysisDirectory, "_clear")
    @patch.object(SharedAnalysisDirectory, "_merge")
    def test_prepare(self, merge, clear, acquire_lock, makedirs):
        @contextmanager
        def acquire(*args, **kwargs):
            yield

        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"])
        acquire_lock.side_effect = acquire
        shared_analysis_directory.prepare()
        merge.assert_has_calls([call()])
        clear.assert_has_calls([call()])
