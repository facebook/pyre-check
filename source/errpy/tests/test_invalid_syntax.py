#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest

from tools.pyre.source.errpy.tests.utils.error_recovery_common import ErrorRecoveryCommon
from tools.pyre.source.errpy.tests.utils.test_common import INVALID_SYNTAX_TESTS_DIR


class ExpectedFailureTests(ErrorRecoveryCommon):
    """These tests focus aspects of invalid syntax we wish to explicitly fail on"""

    def test_invalid_keywords(self) -> None:
        self.compare_recovered_ast_many(
            "invalid_identifiers.pytest", test_dir=INVALID_SYNTAX_TESTS_DIR
        )

    def test_invalid_types(self) -> None:
        self.compare_recovered_ast_many(
            "invalid_types.pytest", test_dir=INVALID_SYNTAX_TESTS_DIR
        )

    def test_invalid_match_case(self) -> None:
        self.compare_recovered_ast_many(
            "invalid_match_case.pytest", test_dir=INVALID_SYNTAX_TESTS_DIR
        )


if __name__ == "__main__":
    unittest.main()
