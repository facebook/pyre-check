# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import os
import unittest

from unittest.mock import call, patch, mock_open

from tools.pyre.scripts.shared_source_directory import merge, missing


class SharedSourceDirectoryTest(unittest.TestCase):
    @patch('os.path.realpath')
    @patch('fcntl.lockf')
    def test_missing(self, lockf, realpath) -> None:
        with patch('builtins.open', mock_open()) as open:
            def create_io(content):
                file = io.StringIO(content)
                file.fileno = (lambda: 3)
                return file
            open.side_effect = [
                create_io('unrelated\n'),
                create_io('a\nb\nc\n'),
                create_io('a\nb\nc\n'),
            ]
            missing_directories = missing(['a', 'b'])
            self.assertEqual(missing_directories, ['a', 'b'])
            missing_directories = missing(['a', 'd'])
            self.assertEqual(missing_directories, ['d'])
            missing_directories = missing(['c', 'b'])
            self.assertEqual(missing_directories, [])
            open.side_effect = OSError('Injected failure')
            missing_directories = missing(['c', 'b'])
            self.assertEqual(missing_directories, None)

    @patch('os.symlink')
    @patch('os.walk')
    @patch('os.makedirs')
    @patch('os.path.exists')
    def test_merge(
            self,
            os_path_exists,
            os_makedirs,
            os_walk,
            os_symlink) -> None:
        os_path_exists.return_value = False
        os_walk.side_effect = [
            [
                ("first", [], ["x.py", "y.py", "z.cpp"]),
                ("first/b", [], ["z.py"]),
            ],
            [("second", [], ["a.py"])]
        ]
        merge(".pyre/shared_root", ["first", "second"])
        os_makedirs.assert_has_calls(
            [call(".pyre/shared_root"), call(".pyre/shared_root/b")])
        os_symlink.assert_has_calls(
            [
                call(os.getcwd() + "/first/x.py", ".pyre/shared_root/x.py"),
                call(os.getcwd() + "/first/y.py", ".pyre/shared_root/y.py"),
                call(os.getcwd() + "/first/b/z.py", ".pyre/shared_root/b/z.py"),
                call(os.getcwd() + "/second/a.py", ".pyre/shared_root/a.py"),
            ])
