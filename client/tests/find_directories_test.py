# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import patch

from .. import find_directories
from ..find_directories import find_local_root, find_project_root, find_root


class InitTest(unittest.TestCase):
    @patch("os.path.isfile")
    def test_find_configuration(self, os_mock_isfile) -> None:
        os_mock_isfile.side_effect = [False, False, False, True]
        self.assertEqual(find_root("/a/b/c/d", "configuration"), "/a")
        os_mock_isfile.side_effect = [True]
        self.assertEqual(find_root("/a", "configuration"), "/a")
        os_mock_isfile.side_effect = [False, False, False]
        self.assertEqual(find_root("/a/b", "configuration"), None)

    def test_find_project_root(self) -> None:
        original_directory = "/a/b/c"
        with patch("os.path.realpath", return_value="realpath"), patch(
            "os.path.isfile", return_value=False
        ) as isfile, patch("os.getcwd", return_value="/a/b/c"):
            isfile.side_effect = (
                lambda directory: directory == "/a/b/.pyre_configuration"
            )
            directory = find_project_root(original_directory)
            self.assertEqual(directory, "/a/b")

        with patch.object(find_directories, "find_root") as mock_find_root:
            original_directory = "/a/b"
            mock_find_root.side_effect = ["/a", "/a/b"]
            directory = find_project_root(original_directory)
            self.assertEqual(directory, "/a")

    def test_find_local_root(self) -> None:
        original_directory = "/a/b/c"
        with patch("os.path.realpath", return_value="realpath"), patch(
            "os.path.isfile", return_value=False
        ) as isfile:
            local_root = find_local_root(original_directory)
            self.assertEqual(local_root, None)

            isfile.side_effect = (
                lambda directory: directory == "/a/b/.pyre_configuration.local"
            )
            local_root = find_local_root(original_directory)
            self.assertEqual(local_root, "/a/b")
