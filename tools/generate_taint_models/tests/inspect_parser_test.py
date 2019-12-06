# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..inspect_parser import extract_qualified_name


class TestClass:
    def method(self) -> None:
        pass


class TestDerived(TestClass):
    pass


class InspectParserTest(unittest.TestCase):
    def test_inherited_methods(self) -> None:
        self.assertEqual(
            extract_qualified_name(TestClass.method),
            "{}.TestClass.method".format(__name__),
        )
        self.assertEqual(
            extract_qualified_name(TestDerived.method),
            "{}.TestClass.method".format(__name__),
        )
