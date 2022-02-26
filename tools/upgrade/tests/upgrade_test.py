# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest

from .. import errors
from ..repository import Repository


repository = Repository()


class FilterErrorTest(unittest.TestCase):
    def test_filter_errors(self) -> None:
        error7 = {
            "line": 2,
            "column": 4,
            "path": "local.py",
            "code": 7,
            "name": "Kind",
            "concise_description": "Error",
            "ignore_error": False,
            "external_to_global_root": False,
        }
        error0 = {
            "line": 2,
            "column": 2,
            "path": "local.py",
            "code": 0,
            "name": "Unused ignore",
            "concise_description": "Unused ignore",
            "ignore_error": False,
            "external_to_global_root": False,
        }
        pyre_errors = [error7, error0]
        self.assertEqual(errors._filter_errors(pyre_errors, 44), [])

        self.assertEqual(errors._filter_errors(pyre_errors, 7), [error7])

        self.assertEqual(errors._filter_errors(pyre_errors, 0), [error0])

        self.assertEqual(errors._filter_errors(pyre_errors, None), [error7, error0])
