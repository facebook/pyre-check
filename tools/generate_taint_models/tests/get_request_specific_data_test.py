# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable

from ..get_request_specific_data import RequestSpecificDataGenerator


class GetRequestSpecificDataTest(unittest.TestCase):
    def test_compute_models(self):
        def visit_all_views(callback: Callable[..., None]):
            def testA(x: int, y: str, *args: int, **kwargs: str) -> None:
                ...

            callback(testA)

        qualifier = (
            f"{__name__}.GetRequestSpecificDataTest.test_compute_models.visit_all_views"
        )
        source = "TaintSource[RequestSpecificData]"
        self.assertEqual(
            list(RequestSpecificDataGenerator([], []).compute_models(visit_all_views)),
            [
                f"def {qualifier}.testA(x: {source}, y: {source}"
                f", *args: {source}, **kwargs: {source}): ..."
            ],
        )
