# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from .. import __name__ as client_name
from ..filesystem import __name__ as filesystem_name
from ..find_directories import find_local_root, find_log_directory, find_project_root


class InitTest(unittest.TestCase):
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

        with patch("{}.find_root".format(filesystem_name)) as mock_find_root:
            original_directory = "/a/b"
            mock_find_root.side_effect = ["/a", "/a/b"]
            directory = find_project_root(original_directory)
            self.assertEqual(directory, "/a/b")

    @patch("{}.find_directories.LOG.warning".format(client_name))
    def test_find_local_root(self, warning) -> None:
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

            isfile.side_effect = (
                lambda directory: directory == "/a/b/.pyre_configuration.local"
                or directory == "/a/.pyre_configuration.local"
            )
            find_local_root(original_directory)
            warning.assert_called_once()

    def test_find_log_directory(self) -> None:
        local_configuration = None
        current_directory = "project"
        log_directory = find_log_directory(
            current_directory, local_configuration, dot_pyre_directory="project/.pyre"
        )
        self.assertEqual(log_directory, "project/.pyre")

        local_configuration = "/project/subdirectory"
        current_directory = "/project"
        log_directory = find_log_directory(
            current_directory, local_configuration, "/project/.pyre"
        )
        self.assertEqual(log_directory, "/project/.pyre/subdirectory")
