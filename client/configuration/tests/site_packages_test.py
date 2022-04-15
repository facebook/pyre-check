# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..search_path import SimpleElement
from ..site_packages import SearchStrategy, search_for_paths


class SitePackagesTest(testslide.TestCase):
    def test_search_strategy_from_string(self) -> None:
        self.assertEqual(SearchStrategy.from_string("all"), SearchStrategy.ALL)
        self.assertEqual(SearchStrategy.from_string("none"), SearchStrategy.NONE)
        self.assertEqual(SearchStrategy.from_string("pep561"), SearchStrategy.PEP561)
        self.assertIsNone(SearchStrategy.from_string("derp"))

    def test_search_for_path_disabled(self) -> None:
        self.assertListEqual(
            search_for_paths(SearchStrategy.NONE, site_roots=["derp"]), []
        )

    def test_search_for_path_all(self) -> None:
        self.assertListEqual(
            search_for_paths(SearchStrategy.ALL, site_roots=["/foo", "/bar"]),
            [SimpleElement("/foo"), SimpleElement("/bar")],
        )
