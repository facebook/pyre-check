# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import builtins
import errno
import fcntl
import os
import pathlib  # noqa
import tempfile
import unittest
from unittest.mock import MagicMock, Mock, call, patch

from .. import (
    filesystem,
)
from ..filesystem import (
    _delete_symbolic_link,
    acquire_lock,
    add_symbolic_link,
    expand_relative_path,
    find_python_paths,
    remove_if_exists,
)


class FilesystemTest(unittest.TestCase):
    def test_find_python_paths(self) -> None:
        root = tempfile.mkdtemp()

        # When there are no paths, returns empty list.
        self.assertListEqual(find_python_paths(root), [])

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
            os.path.relpath(path, root) for path in find_python_paths(root)
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
    def test_acquire_lock(self, lock_file: Mock) -> None:
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

    @patch.object(builtins, "open")
    @patch.object(fcntl, "lockf")
    def test_acquire_lock__release_even_on_exception(
        self, lock_file: MagicMock, open_file: MagicMock
    ) -> None:
        class SomeException(Exception):
            pass

        with self.assertRaises(SomeException):
            with acquire_lock("foo.txt", blocking=True):
                raise SomeException

        file_descriptor = open_file().__enter__().fileno()
        lock_file.assert_has_calls(
            [call(file_descriptor, fcntl.LOCK_EX), call(file_descriptor, fcntl.LOCK_UN)]
        )

    def test_lock_command(self) -> None:
        self.assertEqual(
            filesystem._lock_command(blocking=True, is_shared_reader=True),
            fcntl.LOCK_SH,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=True, is_shared_reader=False),
            fcntl.LOCK_EX,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=False, is_shared_reader=True),
            fcntl.LOCK_SH | fcntl.LOCK_NB,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=False, is_shared_reader=False),
            fcntl.LOCK_EX | fcntl.LOCK_NB,
        )

    @patch("os.unlink")
    def test_delete_symbolic_link(self, unlink):
        # delete succeeds
        unlink.return_value = None
        _delete_symbolic_link("exists")
        unlink.assert_called_once_with("exists")

        # delete fails
        unlink.reset_mock()
        unlink.side_effect = OSError
        self.assertRaises(OSError, _delete_symbolic_link, "exception_occurs")
        unlink.assert_called_once_with("exception_occurs")

    @patch("os.unlink")
    @patch("os.symlink")
    @patch("os.makedirs")
    def test_add_symbolic_link(self, makedirs, symlink, unlink):
        add_symbolic_link("/a/link", "file.py")
        # standard use-cases
        makedirs.assert_called_once_with("/a")
        symlink.assert_called_once_with("file.py", "/a/link")

        symlink.reset_mock()
        makedirs.reset_mock()
        add_symbolic_link("/a/b/c/d/link", "file.py")
        makedirs.assert_called_once_with("/a/b/c/d")
        symlink.assert_called_once_with("file.py", "/a/b/c/d/link")

        # symlink exists
        symlink.reset_mock()
        makedirs.reset_mock()
        error = OSError()
        error.errno = errno.EEXIST
        symlink.side_effect = [error, None]
        add_symbolic_link("/a/b/link", "file.py")
        makedirs.assert_called_once_with("/a/b")
        symlink.assert_called_with("file.py", "/a/b/link")
        unlink.assert_called_once_with("/a/b/link")

        # symlink fails
        symlink.reset_mock()
        makedirs.reset_mock()
        unlink.reset_mock()
        symlink.side_effect = OSError()
        add_symbolic_link("/a/link", "file.py")
        makedirs.assert_called_once_with("/a")
        symlink.assert_called_once_with("file.py", "/a/link")
        unlink.assert_not_called()

    @patch.object(filesystem, "find_paths_with_extensions")
    @patch.object(
        os.path,
        "realpath",
        side_effect=lambda path: path.replace("ANALYSIS_ROOT", "LOCAL_ROOT"),
    )
    def test_compute_symbolic_link_mapping(self, realpath, find_paths_with_extensions):
        find_paths_with_extensions.return_value = [
            "ANALYSIS_ROOT/a.py",
            "ANALYSIS_ROOT/b.thrift",
            "ANALYSIS_ROOT/subX/d.pyi",
            "ANALYSIS_ROOT/subX/e.py",
            "ANALYSIS_ROOT/subY/subZ/g.pyi",
        ]

        self.assertDictEqual(
            filesystem._compute_symbolic_link_mapping(
                "ANALYSIS_ROOT", ["py", "pyi", "thrift"]
            ),
            {
                "LOCAL_ROOT/a.py": "ANALYSIS_ROOT/a.py",
                "LOCAL_ROOT/b.thrift": "ANALYSIS_ROOT/b.thrift",
                "LOCAL_ROOT/subX/d.pyi": "ANALYSIS_ROOT/subX/d.pyi",
                "LOCAL_ROOT/subX/e.py": "ANALYSIS_ROOT/subX/e.py",
                "LOCAL_ROOT/subY/subZ/g.pyi": "ANALYSIS_ROOT/subY/subZ/g.pyi",
            },
        )

    def test_expand_relative_path__globs_are_unchanged(self) -> None:
        self.assertEqual(expand_relative_path("foo", "bar/*/baz"), "foo/bar/*/baz")
        self.assertEqual(
            expand_relative_path("dontcare", "/absolute/path/*/foo"),
            "/absolute/path/*/foo",
        )
