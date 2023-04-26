# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..filesystem import expand_relative_path


class FilesystemTest(unittest.TestCase):
    def test_expand_relative_path__globs_are_unchanged(self) -> None:
        self.assertEqual(expand_relative_path("foo", "bar/*/baz"), "foo/bar/*/baz")
        self.assertEqual(
            expand_relative_path("dontcare", "/absolute/path/*/foo"),
            "/absolute/path/*/foo",
        )
