# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import fcntl
import os
import tempfile
import unittest

from unittest.mock import (
    call,
    patch,
)

from ..filesystem import (  # noqa
    __name__ as filesystem_name,
    find,
    remove_if_exists,
    SharedSourceDirectory,
    try_lock,
)


class FilesystemTest(unittest.TestCase):
    def test_find(self):
        root = tempfile.mkdtemp()

        def create_file(name: str) -> None:
            with open(os.path.join(root, name), "w+"):
                pass

        create_file("a.py")
        create_file("b.pyi")
        create_file("c.cpp")
        os.mkdir(os.path.join(root, "mypy"))
        os.mkdir(os.path.join(root, "scipyi"))
        os.mkdir(os.path.join(root, "spy.py"))
        create_file("mypy/my.py")
        create_file("scipyi/sci.pyi")
        actual_paths = sorted(
            os.path.relpath(path, root)
            for path in find(root, match=r".*\.(py|pyi)")
        )
        self.assertEqual(
            actual_paths,
            ['a.py', 'b.pyi', 'mypy/my.py', 'scipyi/sci.pyi']
        )

    @patch('{}.is_empty'.format(filesystem_name))
    @patch('os.symlink')
    @patch('subprocess.check_output')
    @patch('os.makedirs')
    @patch('os.path.exists')
    @patch('os.path.realpath')
    def test_merge(
            self,
            os_path_realpath,
            os_path_exists,
            os_makedirs,
            check_output,
            os_symlink,
            is_empty) -> None:
        is_empty.return_value = False
        os_path_exists.return_value = False

        def side_effect(path):
            if path[1].endswith("first"):
                serialized = "\n".join(
                    [os.path.join(os.getcwd(), path) for path in [
                        "first/x.py",
                        "first/y.py",
                        "first/b/z.py",
                    ]])
            else:
                serialized = os.path.join(os.getcwd(), "second/a.py")
            return bytes(serialized, "utf-8")

        check_output.side_effect = side_effect
        os_path_realpath.side_effect = lambda x: x
        shared_source_directory = SharedSourceDirectory(['first', 'second'])
        shared_source_directory._merge()
        root = shared_source_directory.get_root()
        os_makedirs.assert_has_calls(
            [call(root), call(f"{root}/b")])
        os_symlink.assert_has_calls(
            [
                call(os.getcwd() + "/first/x.py", f"{root}/x.py"),
                call(os.getcwd() + "/first/y.py", f"{root}/y.py"),
                call(os.getcwd() + "/first/b/z.py", f"{root}/b/z.py"),
                call(os.getcwd() + "/second/a.py", f"{root}/a.py"),
            ],
            any_order=True)

    def test_remove_if_exists(self):
        # File removal.
        with patch('os.remove') as os_remove, \
                patch('shutil.rmtree') as shutil_rmtree:
            os_remove.side_effect = OSError()
            remove_if_exists('path')
            os_remove.assert_called_once_with('path')
            shutil_rmtree.assert_called_once_with('path')

        # Directory removal.
        with patch('os.remove') as os_remove, \
                patch('shutil.rmtree') as shutil_rmtree:
            shutil_rmtree.side_effect = OSError()
            remove_if_exists('path')
            os_remove.assert_called_once_with('path')
            shutil_rmtree.assert_called_once_with('path')

        # Both throw.
        with patch('os.remove') as os_remove, \
                patch('shutil.rmtree') as shutil_rmtree:
            os_remove.side_effect = FileNotFoundError()
            shutil_rmtree.side_effect = OSError()
            remove_if_exists('path')
            os_remove.assert_called_once_with('path')
            shutil_rmtree.assert_called_once_with('path')

    @patch('fcntl.lockf')
    def test_try_lock(self, lock_file: unittest.mock.Mock) -> None:
        (_, path) = tempfile.mkstemp()
        lockfile_file_descriptor = None
        with try_lock(path) as file_descriptor:
            lockfile_file_descriptor = file_descriptor
        lock_file.assert_has_calls([
            call(lockfile_file_descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB),
            call(lockfile_file_descriptor, fcntl.LOCK_UN)
        ])

        def fail_on_exclusive(_, lock_kind):
            if lock_kind == fcntl.LOCK_EX | fcntl.LOCK_NB:
                raise OSError()
            return None

        lock_file.side_effect = fail_on_exclusive
        with self.assertRaises(OSError):
            with try_lock(path):
                pass
