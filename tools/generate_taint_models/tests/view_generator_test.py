# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import unittest
from typing import Any
from unittest.mock import patch

from .. import view_generator


class ViewGeneratorTest(unittest.TestCase):
    def test_view_generator(self) -> None:
        class Url:
            def __init__(self, callback: Any) -> None:
                self.callback = callback

        class Resolver:
            pass

        class FirstUrls(Resolver):
            url_patterns = [Url(lambda: 1), Url(lambda: 2), Url(lambda: 3)]

        class SecondUrls(Resolver):
            url_patterns = [FirstUrls(), Url(lambda: 4), Url(lambda: 5), Url(lambda: 6)]

        class CallableClass:
            def __call__(self):
                return 8

        callable_class_object = CallableClass()

        class Urls:
            urlpatterns = [SecondUrls(), Url(lambda: 7), Url(callable_class_object)]

        with patch(f"{view_generator.__name__}.import_module", return_value=Urls):
            views = view_generator.get_all_views(
                view_generator.DjangoUrls(
                    urls_module="urls", url_pattern_type=Url, url_resolver_type=Resolver
                )
            )
            values = [view() for view in views]
            self.assertEqual(values, [1, 2, 3, 4, 5, 6, 7, 8])

            self.assertEqual(views[7], callable_class_object.__call__)
