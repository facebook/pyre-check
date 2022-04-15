# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..site_packages import SearchStrategy


class SitePackagesTest(testslide.TestCase):
    def test_search_strategy_from_string(self) -> None:
        self.assertEqual(SearchStrategy.from_string("all"), SearchStrategy.ALL)
        self.assertEqual(SearchStrategy.from_string("none"), SearchStrategy.NONE)
        self.assertEqual(SearchStrategy.from_string("pep561"), SearchStrategy.PEP561)
        self.assertIsNone(SearchStrategy.from_string("derp"))
