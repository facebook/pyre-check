# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from unittest import TestCase

from .. import Pipeline
from ..warning_code_filter import WarningCodeFilter


class TestWarningCodeFilter(TestCase):
    def setUp(self) -> None:
        self.warning_code_filter = WarningCodeFilter({6000})

    # pyre-fixme[3]: Return type must be annotated.
    def test_filter_codes(self):
        dict_entries = {"issues": [{"code": 6000}, {"code": 6001}, {"code": 6002}]}
        output, _ = Pipeline([self.warning_code_filter]).run(dict_entries)

        self.assertEqual(len(output["issues"]), 1)
        self.assertEqual(output["issues"][0], {"code": 6000})
