# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import unittest
from unittest.mock import patch

from ..gather_views import __name__ as gather_views_name, get_all_views


class GatherViewsTest(unittest.TestCase):
    def test_gather_views(self) -> None:
        class Url:
            def __init__(self, value: int) -> None:
                self.value = value

            def callback(self) -> int:
                return self.value

        class Resolver:
            pass

        class FirstUrls(Resolver):
            url_patterns = [Url(1), Url(2), Url(3)]

        class SecondUrls(Resolver):
            url_patterns = [FirstUrls(), Url(4), Url(5), Url(6)]

        class Urls:
            urlpatterns = [SecondUrls(), Url(7)]

        with patch(f"{gather_views_name}.import_module", return_value=Urls):
            views = get_all_views(
                module_name="urls", url_pattern_type=Url, url_resolver_type=Resolver
            )
            values = [view() for view in views]
            self.assertEqual(values, [1, 2, 3, 4, 5, 6, 7])
