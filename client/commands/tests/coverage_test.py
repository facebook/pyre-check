# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path
from typing import List

import testslide

from ...coverage_collector import FileCoverage
from ...tests import setup
from ..coverage import collect_coverage_for_paths, find_root_path


class CoverageTest(testslide.TestCase):
    def test_find_root(self) -> None:
        self.assertEqual(
            find_root_path(
                local_root=Path("/root/local"),
                working_directory=Path("/irrelevant"),
            ),
            Path("/root/local"),
        )
        self.assertEqual(
            find_root_path(
                local_root=None,
                working_directory=Path("/working/dir"),
            ),
            Path("/working/dir"),
        )

    def test_collect_coverage(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path: Path = Path(root)
            setup.ensure_files_exist(root_path, ["foo.py", "bar.py"])
            foo_path = root_path / "foo.py"
            bar_path = root_path / "bar.py"
            baz_path = root_path / "baz.py"

            data: List[FileCoverage] = collect_coverage_for_paths(
                [foo_path, bar_path, baz_path],
                working_directory=root,
                strict_default=False,
            )

            def is_collected(path: Path) -> bool:
                return any(
                    str(path.relative_to(root_path)) == coverage.filepath
                    for coverage in data
                )

            self.assertTrue(is_collected(foo_path))
            self.assertTrue(is_collected(bar_path))
            self.assertFalse(is_collected(baz_path))
