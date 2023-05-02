# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from dataclasses import dataclass
from typing import Any, final, List, Optional

from testing.types import File
from typing_extensions import Annotated

from ..inspect_parser import (
    extract_parameters,
    extract_parameters_with_types,
    extract_qualified_name,
    strip_custom_annotations,
)
from ..parameter import Parameter


class TestClass:
    def method(self) -> None:
        pass


class TestDerived(TestClass):
    pass


@final
@dataclass(frozen=True)
class TestRequestDataclass:
    x1: str
    y: int


@final
@dataclass(frozen=True)
class TestReturnDataclass:
    w: str
    z: int


@final
@dataclass(frozen=True)
class WhateverAnnotation:
    value: str


# pyre-ignore[2]: Annotations intentionally excluded
def test_function(arg1, arg2: "TestClass", arg3: TestClass, *vararg, **kwarg) -> None:
    ...


def test_annotated_parameter_function(
    arg1: "Annotated[TestClass, ExampleAnnotation(accesses=(Access.REVIEWED,))]",  # noqa
    arg2: "Annotated[TestClass, ExampleAnnotation(accesses=(Access.REVIEWED,)), ExampleOtherAnnotation(whatever=(Other.REVIEWED,))]",  # noqa
) -> None:
    ...


def test_dataclass_parameter(data: TestRequestDataclass) -> TestReturnDataclass:
    return TestReturnDataclass(w=data.x1, z=data.y)


def test_args_kwargs_with_any_annotation(*args: Any, **kwargs: Any) -> None:
    pass


def test_no_parameters() -> None:
    pass


def test_mixed_args(
    data1: Optional[TestRequestDataclass],
    data2: Annotated[TestRequestDataclass, WhateverAnnotation(value="test")],
    x: str,
    y,  # pyre-ignore
    *args,  # pyre-ignore
    **kwargs,  # pyre-ignore
) -> None:
    pass


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
        **kwarg,
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

    # Parameter __eq__ was overridden to only check the name let's verify all the attributes
    def _assert_equals_parameters(
        self, parameters: List[Parameter], expected_parameters: List[Parameter]
    ) -> None:
        self.assertEqual(len(parameters), len(expected_parameters))
        for (parameter, expected_parameter) in zip(
            parameters,
            expected_parameters,
        ):
            self.assertEqual(parameter.Kind, expected_parameter.Kind)
            self.assertEqual(parameter.annotation, expected_parameter.annotation)
            self.assertEqual(parameter.name, expected_parameter.name)

    def test_extract_parameters(self) -> None:
        expected_parameters = [
            Parameter(name="arg1", annotation="_empty", kind=Parameter.Kind.ARG),
            Parameter(name="arg2", annotation="TestClass", kind=Parameter.Kind.ARG),
            Parameter(name="arg3", annotation="TestClass", kind=Parameter.Kind.ARG),
            Parameter(name="*vararg", annotation="_empty", kind=Parameter.Kind.VARARG),
            Parameter(name="**kwarg", annotation="_empty", kind=Parameter.Kind.KWARG),
        ]
        self._assert_equals_parameters(
            extract_parameters(test_function), expected_parameters
        )

        expected_parameters = [
            Parameter(name="self", annotation="_empty", kind=Parameter.Kind.ARG)
        ] + expected_parameters
        self._assert_equals_parameters(
            extract_parameters(TestMethodClass.test_method), expected_parameters
        )

        expected_parameters_annotated = [
            Parameter(name="arg1", annotation="TestClass", kind=Parameter.Kind.ARG),
            Parameter(name="arg2", annotation="TestClass", kind=Parameter.Kind.ARG),
        ]
        self._assert_equals_parameters(
            extract_parameters(test_annotated_parameter_function),
            expected_parameters_annotated,
        )

    def test_thrift_structs(self) -> None:
        self.assertEqual(
            extract_qualified_name(File.__hash__),
            "testing.types.File.__hash__",
        )

    def test_strip_custom_annotations(self) -> None:
        self.assertEqual(
            strip_custom_annotations("TestClass"),
            "TestClass",
        )
        self.assertEqual(
            strip_custom_annotations("Tuple[int, int]"),
            "Tuple[int, int]",
        )
        self.assertEqual(
            strip_custom_annotations(
                "Annotated[TestClass, ExampleAnnotation(accesses=(Access.REVIEWED,))]"
            ),
            "TestClass",
        )
        self.assertEqual(
            strip_custom_annotations(
                "Annotated[Tuple[int, int], ExampleAnnotation(accesses=(Access.REVIEWED,))]"
            ),
            "Tuple[int, int]",
        )
        self.assertEqual(
            strip_custom_annotations(
                "Annotated[Optional[TestClass], ExampleAnnotation(accesses=(Access.REVIEWED,))]"
            ),
            "Optional[TestClass]",
        )


class ExtractParametersWithTypesTester(unittest.TestCase):
    def test_dataclass_parameters_annotation(self) -> None:
        annotations = extract_parameters_with_types(
            test_dataclass_parameter, strip_optional=True, strip_annotated=True
        )
        self.assertEqual(
            annotations,
            {"data": TestRequestDataclass},
        )

    def test_with_mixed_args_annotations(self) -> None:
        annotations = extract_parameters_with_types(
            test_mixed_args,
            strip_optional=True,
            strip_annotated=True,
        )
        self.assertEqual(
            annotations,
            {
                "data1": TestRequestDataclass,
                "data2": TestRequestDataclass,
                "x": str,
                "y": None,
                "**kwargs": None,
                "*args": None,
            },
        )

    def test_with_args_kwargs_with_any_annotation(self) -> None:
        annotations = extract_parameters_with_types(
            test_args_kwargs_with_any_annotation,
        )
        self.assertEqual(
            annotations,
            {
                "*args": Any,
                "**kwargs": Any,
            },
        )

    def test_with_no_parameters_annotations(self) -> None:
        annotations = extract_parameters_with_types(
            test_no_parameters,
        )
        self.assertEqual(
            annotations,
            {},
        )
