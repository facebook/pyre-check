# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest

from ..resources import find_log_directory


class ResourcesTest(unittest.TestCase):
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
