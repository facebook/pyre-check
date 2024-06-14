#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest

from tools.pyre.source.errpy.tests.utils.test_common import ASTTestCommon, PRETTY_PRINTER_TESTS_DIR


class AllGrammarTests(ASTTestCommon):
    """These tests focus on individual types of bugs found in errpy which have been fixed"""

    def test_pretty_printer_bugs(self) -> None:
        self.check_many_cases_in_file(
            "pretty_printer_bugs.pytest", flavour=PRETTY_PRINTER_TESTS_DIR
        )


if __name__ == "__main__":
    unittest.main()
