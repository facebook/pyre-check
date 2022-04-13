# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..exceptions import InvalidPythonVersion
from ..python_version import PythonVersion


class PythonVersionTest(testslide.TestCase):
    def test_from_string(self) -> None:
        def assert_parsed(input: str, expected: PythonVersion) -> None:
            self.assertEqual(PythonVersion.from_string(input), expected)

        def assert_not_parsed(input: str) -> None:
            with self.assertRaises(InvalidPythonVersion):
                PythonVersion.from_string(input)

        assert_not_parsed("")
        assert_not_parsed("derp")
        assert_not_parsed("123abc")
        assert_not_parsed("1.a")
        assert_not_parsed("1.2.a")
        assert_not_parsed(".1")
        assert_not_parsed("1.2.3.4")

        assert_parsed("3", PythonVersion(major=3))
        assert_parsed("3.6", PythonVersion(major=3, minor=6))
        assert_parsed("3.6.7", PythonVersion(major=3, minor=6, micro=7))
