# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..patch import QualifiedName


class PatchTest(testslide.TestCase):
    def test_qualified_name(self) -> None:
        def assert_name_preserved(name: str) -> None:
            self.assertEqual(QualifiedName.from_string(name).to_string(), name)

        assert_name_preserved("")
        assert_name_preserved("foo")
        assert_name_preserved("foo.bar")
        assert_name_preserved("foo.bar.baz")

        self.assertTrue(QualifiedName.from_string("").is_empty())
        self.assertFalse(QualifiedName.from_string("foo").is_empty())
