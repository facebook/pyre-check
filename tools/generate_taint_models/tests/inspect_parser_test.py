# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from testing.types import File

from ..inspect_parser import extract_parameters, extract_qualified_name
from ..parameter import Parameter


class TestClass:
    def method(self) -> None:
        pass


class TestDerived(TestClass):
    pass


# pyre-ignore[2]: Annotations intentionally excluded
def test_function(arg1, arg2: "TestClass", arg3: TestClass, *vararg, **kwarg) -> None:
    ...


class TestMethodClass:
    def test_method(
        self,
        # pyre-ignore[2]: Annotations intentionally excluded
        arg1,
        arg2: "TestClass",
        arg3: TestClass,
        # pyre-ignore[2]: Annotations intentionally excluded
        *vararg,
        # pyre-ignore[2]: Annotations intentionally excluded
        **kwarg
    ) -> None:
        ...


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

    def test_extract_parameters(self) -> None:
        expected_parameters = [
            Parameter(name="arg1", annotation="_empty", kind=Parameter.Kind.ARG),
            Parameter(name="arg2", annotation="TestClass", kind=Parameter.Kind.ARG),
            Parameter(name="arg3", annotation="TestClass", kind=Parameter.Kind.ARG),
            Parameter(name="*vararg", annotation="_empty", kind=Parameter.Kind.VARARG),
            Parameter(name="**kwarg", annotation="_empty", kind=Parameter.Kind.KWARG),
        ]
        self.assertEqual(extract_parameters(test_function), expected_parameters)

        expected_parameters = [
            Parameter(name="self", annotation="_empty", kind=Parameter.Kind.ARG)
        ] + expected_parameters
        self.assertEqual(
            extract_parameters(TestMethodClass.test_method), expected_parameters
        )

    def test_thrift_structs(self) -> None:
        self.assertEqual(
            extract_qualified_name(File.__hash__),
            "testing.types.File.__hash__",
        )
