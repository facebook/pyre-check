#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest

from tools.pyre.source.errpy.tests.utils.test_common import ASTTestCommon


class AllGrammarTests(ASTTestCommon):
    """These tests focus on individual aspects of Python syntax"""

    def more_match_test(self, to_test: str) -> None:
        self.check_many_cases_in_file(f"more_match/{to_test}")

    def test_more_match_1(self) -> None:
        self.more_match_test("match1.pytest")

    def test_more_match_2(self) -> None:
        self.more_match_test("match2.pytest")

    def test_more_match_3(self) -> None:
        self.more_match_test("match3.pytest")

    def test_more_match_4(self) -> None:
        self.more_match_test("match4.pytest")

    def test_more_match_5(self) -> None:
        self.more_match_test("match5.pytest")

    def test_more_match_6(self) -> None:
        self.more_match_test("match6.pytest")

    def test_more_match_7(self) -> None:
        self.more_match_test("match7.pytest")

    def test_more_match_8(self) -> None:
        self.more_match_test("match8.pytest")

    def test_more_match_9(self) -> None:
        self.more_match_test("match9.pytest")

    def test_more_match_10(self) -> None:
        self.more_match_test("match10.pytest")

    def test_more_match_11(self) -> None:
        self.more_match_test("match11.pytest")

    def test_more_match_12(self) -> None:
        self.more_match_test("match12.pytest")


if __name__ == "__main__":
    unittest.main()
