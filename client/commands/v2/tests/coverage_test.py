# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import testslide

from .... import configuration
from ..coverage import find_root


class CoverageTest(testslide.TestCase):
    def test_find_root(self) -> None:
        self.assertEqual(
            find_root(
                configuration.Configuration(
                    project_root="/root",
                    dot_pyre_directory=Path("/irrelevant"),
                    relative_local_root="local",
                ),
                working_directory=Path("/irrelevant"),
            ),
            Path("/root/local"),
        )
        self.assertEqual(
            find_root(
                configuration.Configuration(
                    project_root="/root", dot_pyre_directory=Path("/irrelevant")
                ),
                working_directory=Path("/working/dir"),
            ),
            Path("/working/dir"),
        )
