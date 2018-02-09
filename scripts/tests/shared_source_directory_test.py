# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import unittest

from unittest.mock import patch, mock_open

from tools.pyre.scripts.shared_source_directory import missing


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
