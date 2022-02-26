# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import unittest

from .. import model_generator


class ModelGeneratorTest(unittest.TestCase):
    def test_qualifier(self) -> None:
        self.assertEqual(model_generator.qualifier("/root", "/root"), ".")
        self.assertEqual(model_generator.qualifier("/root", "/root/a.py"), "a")
        self.assertEqual(model_generator.qualifier("/root", "/root/dir/a.py"), "dir.a")
        self.assertEqual(
            model_generator.qualifier("/root", "/root/dir/__init__.py"), "dir"
        )
        self.assertEqual(model_generator.qualifier("/root", "/root/a.pyi"), "a")
        self.assertEqual(model_generator.qualifier("/root", "/root/dir/a.pyi"), "dir.a")
        self.assertEqual(
            model_generator.qualifier("/root", "/root/dir/__init__.pyi"), "dir"
        )
