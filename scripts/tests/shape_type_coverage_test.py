# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Dict, List

from ...api.query import Annotation, Position
from ..shape_type_coverage import (
    _is_tensor,
    _is_precise_tensor,
    ParametricType,
    _parametric_type,
    ShapeAnnotations,
    _collect_shape_types,
)


class IsTensorTest(unittest.TestCase):
    def assert_is_tensor(self, parametric: ParametricType) -> None:
        self.assertTrue(_is_tensor(parametric))

    def assert_is_not_tensor(self, parametric: ParametricType) -> None:
        self.assertFalse(_is_tensor(parametric))

    def test_is_tensor(self) -> None:
        self.assert_is_tensor(ParametricType("torch.Tensor", []))
        self.assert_is_tensor(
            ParametricType(
                "torch.Tensor",
                [
                    "torch.float32",
                    "typing_extensions.Literal[5]",
                    "typing_extensions.Literal[2]",
                ],
            )
        )
        self.assert_is_tensor(
            ParametricType("torch.Tensor", ["torch.float32", "int", "int"])
        )
        self.assert_is_tensor(
            ParametricType(
                "torch.Tensor", ["torch.float32", "typing_extensions.Literal[5]", "int"]
            )
        )

    def test_is_not_tensor(self) -> None:
        self.assert_is_not_tensor(ParametricType("torch.TensorLike", []))
        self.assert_is_not_tensor(ParametricType("typing_extensions.Literal", ["5"]))


class IsPreciseTensorTest(unittest.TestCase):
    def assert_is_precise_tensor(self, parametric: ParametricType) -> None:
        self.assertTrue(_is_precise_tensor(parametric))

    def assert_is_not_precise_tensor(self, parametric: ParametricType) -> None:
        self.assertFalse(_is_precise_tensor(parametric))

    def test_is_precise_tensor(self) -> None:
        self.assert_is_precise_tensor(ParametricType("torch.Tensor", ["torch.float32"]))
        self.assert_is_precise_tensor(
            ParametricType(
                "torch.Tensor",
                [
                    "torch.float32",
                    "typing_extensions.Literal[5]",
                    "typing_extensions.Literal[2]",
                ],
            )
        )

    def test_is_not_precise_tensor(self) -> None:
        self.assert_is_not_precise_tensor(
            ParametricType(
                "torch.Tensor", ["torch.float32", "typing_extensions.Literal[5]", "int"]
            )
        )
        self.assert_is_not_precise_tensor(
            ParametricType("torch.Tensor", ["torch.float32", "int", "int"])
        )


class ParametricTypeTest(unittest.TestCase):
    def assert_is_not_parametric(self, type_name: str) -> None:
        self.assertEqual(None, _parametric_type(type_name))

    def assert_parametric_with(
        self, type_name: str, parametric: ParametricType
    ) -> None:
        self.assertEqual(_parametric_type(type_name), parametric)

    def test_parametric_with(self) -> None:
        self.assert_parametric_with(
            "torch.Tensor[]", ParametricType("torch.Tensor", [""])
        )
        self.assert_parametric_with(
            "typing_extensions.Literal[5]",
            ParametricType("typing_extensions.Literal", ["5"]),
        )
        self.assert_parametric_with(
            "torch.Tensor[torch.float32, typing_extensions.Literal[5], int]",
            ParametricType(
                "torch.Tensor", ["torch.float32", "typing_extensions.Literal[5]", "int"]
            ),
        )
        self.assert_parametric_with("List[str]", ParametricType("List", ["str"]))

    def test_is_not_parametric(self) -> None:
        self.assert_is_not_parametric("int")
        self.assert_is_not_parametric("")


class CollectShapeTypesTest(unittest.TestCase):
    def assert_collects_as(
        self,
        given: Dict[str, List[Annotation]],
        expected: Dict[str, ShapeAnnotations],
    ) -> None:
        self.assertEqual(expected, _collect_shape_types(given))

    def test_collects_as(self) -> None:
        precise = Annotation(
            "torch.Tensor[torch.float32, typing_extensions.Literal[5], "
            "typing_extensions.Literal[2]]",
            Position(1, 0),
            Position(2, 3),
        )

        imprecise = Annotation(
            "torch.Tensor[torch.float32, int, typing_extensions.Literal[2]]",
            Position(2, 5),
            Position(0, 1),
        )

        non_tensor = Annotation(
            "typing_extensions.Literal[5]",
            Position(0, 0),
            Position(0, 0),
        )
        self.assert_collects_as(given={}, expected={})

        self.assert_collects_as(
            given={"filename": [precise, imprecise, non_tensor]},
            expected={"filename": ShapeAnnotations([precise], [imprecise])},
        )

        self.assert_collects_as(
            given={
                "first_file": [precise],
                "second_file": [imprecise],
            },
            expected={
                "first_file": ShapeAnnotations([precise], []),
                "second_file": ShapeAnnotations([], [imprecise]),
            },
        )
