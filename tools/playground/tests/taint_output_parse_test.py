# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import os
import unittest
from pathlib import Path

from ..application import _parse_annotations_from_taint_output


class TestTaintOutputParser(unittest.TestCase):
    def test_parser(self):
        expected_output = {}
        expected_output_file_path = Path(
            os.getcwd() / "taint_output_parsed.expected.json"
        )
        self.assertTrue(
            expected_output_file_path.exists() and expected_output_file_path.is_file()
        )
        with expected_output_file_path.open() as expected_output_file:
            expected_output_file = json.loads(expected_output_file.read())
        taint_output_file_path = Path(os.getcwd() / "taint_output.json")
        self.assertTrue(
            taint_output_file_path.exists() and taint_output_file_path.is_file()
        )
        self.assertEqual(
            _parse_annotations_from_taint_output(taint_output_file_path),
            expected_output,
        )


if __name__ == "__main__":
    unittest.main()
