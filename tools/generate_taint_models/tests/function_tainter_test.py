# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from dataclasses import dataclass
from typing import Any, final, Optional

from pyre_extensions import ParameterSpecification
from typing_extensions import Annotated

from ..function_tainter import taint_callable_dataclass_fields_parameters

TestParams = ParameterSpecification("TestParams")


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


def test_dataclass_parameter(data: TestRequestDataclass) -> TestReturnDataclass:
    return TestReturnDataclass(w=data.x1, z=data.y)


def test_simple_parameter(x1: str, y: int) -> None:
    pass


def test_simple_and_dataclass_parameters(data: TestRequestDataclass, x: str) -> None:
    pass


# pyre-ignore
def test_args_kwargs_without_annotation(*args, **kwargs):
    pass


def test_args_kwargs_with_any_annotation(*args: Any, **kwargs: Any) -> None:
    pass


def test_optional_annotation(data: Optional[TestRequestDataclass]) -> None:
    pass


def test_annotated_annotation(
    data: Annotated[TestRequestDataclass, WhateverAnnotation(value="test")]
) -> None:
    pass


def test_mixed_args(
    data1: Optional[TestRequestDataclass],
    data2: Annotated[TestRequestDataclass, WhateverAnnotation(value="test")],
    x: str,
    *args,  # pyre-ignore
    **kwargs  # pyre-ignore
) -> None:
    pass


class FunctionTainterTest(unittest.TestCase):
    def test_taint_callable_with_dataclass(self) -> None:
        test_dataclass_parameter_models = taint_callable_dataclass_fields_parameters(
            test_dataclass_parameter,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[0]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_dataclass_parameter(data: TaintSource[UserControlled, ParameterPath[_.x1]]) -> TaintSink[ReturnedToUser]: ...",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[1]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_dataclass_parameter(data: TaintSource[UserControlled, ParameterPath[_.y]]) -> TaintSink[ReturnedToUser]: ...",
        )

    def test_taint_callable_with_dataclass_and_simple_parameters(self) -> None:
        test_dataclass_parameter_models = taint_callable_dataclass_fields_parameters(
            test_simple_and_dataclass_parameters,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[0]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_simple_and_dataclass_parameters(data: TaintSource[UserControlled, ParameterPath[_.x1]], x) -> TaintSink[ReturnedToUser]: ...",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[1]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_simple_and_dataclass_parameters(data: TaintSource[UserControlled, ParameterPath[_.y]], x) -> TaintSink[ReturnedToUser]: ...",
        )

    def test_taint_callable_with_dataclass_and_simple_parameters_args_kwargs_without_annotation(
        self,
    ) -> None:
        test_dataclass_parameter_models = taint_callable_dataclass_fields_parameters(
            test_args_kwargs_without_annotation,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        print(str(list(test_dataclass_parameter_models)[0]))
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[0]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_args_kwargs_without_annotation(*args: TaintSource[UserControlled], **kwargs: TaintSource[UserControlled]) -> TaintSink[ReturnedToUser]: ...",
        )

    def test_taint_callable_with_dataclass_and_simple_parameters_args_kwargs_any_annotation(
        self,
    ) -> None:
        test_dataclass_parameter_models = taint_callable_dataclass_fields_parameters(
            test_args_kwargs_with_any_annotation,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        print(str(list(test_dataclass_parameter_models)[0]))

        self.assertEqual(
            str(list(test_dataclass_parameter_models)[0]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_args_kwargs_with_any_annotation(*args: TaintSource[UserControlled], **kwargs: TaintSource[UserControlled]) -> TaintSink[ReturnedToUser]: ...",
        )

    def test_taint_callable_with_dataclass_with_optional_annotation(self) -> None:
        test_dataclass_parameter_models = taint_callable_dataclass_fields_parameters(
            test_optional_annotation,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[0]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_optional_annotation(data: TaintSource[UserControlled, ParameterPath[_.x1]]) -> TaintSink[ReturnedToUser]: ...",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[1]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_optional_annotation(data: TaintSource[UserControlled, ParameterPath[_.y]]) -> TaintSink[ReturnedToUser]: ...",
        )

    def test_taint_callable_with_dataclass_with_annotated_annotation(self) -> None:
        test_dataclass_parameter_models = taint_callable_dataclass_fields_parameters(
            test_annotated_annotation,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[0]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_annotated_annotation(data: TaintSource[UserControlled, ParameterPath[_.x1]]) -> TaintSink[ReturnedToUser]: ...",
        )
        self.assertEqual(
            str(list(test_dataclass_parameter_models)[1]),
            "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_annotated_annotation(data: TaintSource[UserControlled, ParameterPath[_.y]]) -> TaintSink[ReturnedToUser]: ...",
        )

    def test_taint_callable_with_mixed_args(self) -> None:
        test_mixed_args_parameter_models = taint_callable_dataclass_fields_parameters(
            test_mixed_args,
            "TaintSource",
            "UserControlled",
            "TaintSink[ReturnedToUser]",
        )
        self.assertEqual(
            {str(model) for model in test_mixed_args_parameter_models},
            {
                "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_mixed_args(data1: TaintSource[UserControlled, ParameterPath[_.x1]], data2, x, *args, **kwargs) -> TaintSink[ReturnedToUser]: ...",
                "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_mixed_args(data1: TaintSource[UserControlled, ParameterPath[_.y]], data2, x, *args, **kwargs) -> TaintSink[ReturnedToUser]: ...",
                "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_mixed_args(data1, data2: TaintSource[UserControlled, ParameterPath[_.x1]], x, *args, **kwargs) -> TaintSink[ReturnedToUser]: ...",
                "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_mixed_args(data1, data2: TaintSource[UserControlled, ParameterPath[_.y]], x, *args, **kwargs) -> TaintSink[ReturnedToUser]: ...",
                "def tools.pyre.tools.generate_taint_models.tests.function_tainter_test.test_mixed_args(data1, data2, x: TaintSource[UserControlled], *args: TaintSource[UserControlled], **kwargs: TaintSource[UserControlled]) -> TaintSink[ReturnedToUser]: ...",
            },
        )
