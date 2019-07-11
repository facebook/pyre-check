# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import unittest
from typing import Callable, Iterable
from unittest.mock import patch

from .. import model_generator, view_generator


class ViewGeneratorTest(unittest.TestCase):
    def test_view_generator(self) -> None:
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

        class TestGenerator(view_generator.ViewGenerator):
            def compute_models(
                self, functions_to_model: Iterable[Callable[..., object]]
            ) -> Iterable[str]:
                return []

        with patch(f"{view_generator.__name__}.import_module", return_value=Urls):
            model_generator.Configuration.urls_module = "urls"
            model_generator.Configuration.url_pattern_type = Url
            model_generator.Configuration.url_resolver_type = Resolver
            views = TestGenerator().gather_functions_to_model()
            values = [view() for view in views]
            self.assertEqual(values, [1, 2, 3, 4, 5, 6, 7])
