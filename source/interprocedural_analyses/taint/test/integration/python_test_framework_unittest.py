# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from pysa import _test_sink, _test_source

# import pytest


"""
Python test frameworks are ignored by pyre and pysa by default.

See GlobalResolution.source_is_unit_test for implementation details of logic.

The point of this integration test is to highlight known False Negtives due to
this logic.

This test showcases logic for ignoring pytest tests
"""


# Logic for ignoring is the class eventually imports from unittest.case.TestCase
class TestWithIssue(unittest.TestCase):
    def test_issue(self):
        # Expected False Negative
        _test_sink(_test_source())


# Whole file is ignored
def false_negative():
    # Expected False Negative
    _test_sink(_test_source())
