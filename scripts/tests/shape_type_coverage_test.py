# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Dict, List

from ...api.query import Annotation, Position
from ..shape_type_coverage import (
    _collect_shape_types,
    _extract_multiline_text,
    _extract_substring,
    _is_precise_tensor,
    _is_precise_tensor_dimension,
    _is_tensor,
    _parametric_type,
    _split_list,
    ParametricType,
    ShapeAnnotations,
)


class SplitListTest(unittest.TestCase):
    def assert_splits_as(self, given: str, expected: List[str]) -> None:
        self.assertEqual(_split_list(given), expected)

    def test_split_list(self) -> None:
        self.assert_splits_as(
            given="[]",
            expected=[],
        )
        self.assert_splits_as(
            given="[single]",
            expected=["single"],
        )

        self.assert_splits_as(
            given="[Union[int, str], List[int]]",
            expected=["Union[int, str]", "List[int]"],
        )

        self.assert_splits_as(
            given="[[1, 2], [3, 4], [[5, 6], 7]]",
            expected=["[1, 2]", "[3, 4]", "[[5, 6], 7]"],
        )


class IsPreciseTensorDimensionTest(unittest.TestCase):
    def assert_is_precise_dimension(self, dimension: str) -> None:
        self.assertTrue(_is_precise_tensor_dimension(dimension))

    def assert_is_not_precise_dimension(self, dimension: str) -> None:
        self.assertFalse(_is_precise_tensor_dimension(dimension))

    def test_is_precise_dimension(self) -> None:
        self.assert_is_precise_dimension("typing_extensions.Literal[5]")

        self.assert_is_precise_dimension("Variable[N (bound to int)]")

        self.assert_is_precise_dimension("*Ts")

        self.assert_is_precise_dimension("*anything")

        self.assert_is_precise_dimension("pyre_extensions.IntExpression[N1 + N2]")
        self.assert_is_precise_dimension("pyre_extensions.IntExpression[N1 + (N2//3)]")
        self.assert_is_precise_dimension("pyre_extensions.IntExpression[3N1 + (N2//3)]")
        self.assert_is_precise_dimension("pyre_extensions.IntExpression[5 + N2]")

        self.assert_is_precise_dimension(
            "*Broadcast[ \
                typing.Tuple[ \
                    typing_extensions.Literal[2], \
                    typing_extensions.Literal[1] \
                ], \
                typing.Tuple[ \
                    typing_extensions.Literal[1], \
                    typing_extensions.Literal[2] \
                ] \
            ]"
        )

        self.assert_is_precise_dimension(
            "*Broadcast[ \
                Tuple[*Ts], \
                Broadcast[ \
                    typing.Tuple[ \
                        typing_extensions.Literal[1], \
                        typing_extensions.Literal[2] \
                    ], \
                    Tuple[*Ts2] \
                ] \
            ]"
        )

    def test_is_not_precise_dimension(self) -> None:
        self.assert_is_not_precise_dimension("Variable[N]")

        self.assert_is_not_precise_dimension(
            "*Tuple[typing_extensions.Literal[2], ...]"
        )
        self.assert_is_not_precise_dimension("*Tuple[Any]")

        self.assert_is_not_precise_dimension("int")

        self.assert_is_not_precise_dimension(
            "Broadcast[*name, Tuple[typing_extensions.Literal[1], \
             typing_extensions.Literal[2]]]"
        )

        self.assert_is_not_precise_dimension(
            "*Broadcast[ \
                *name, \
                Tuple[ \
                    typing_extensions.Literal[1], \
                    typing_extensions.Literal[2] \
                ], \
                *name2 \
            ]"
        )

        self.assert_is_not_precise_dimension(
            "*Broadcast[ \
                Tuple[ \
                    typing_extensions.Literal[1], \
                    typing_extensions.Literal[2] \
                ], \
                typing_extensions.Literal[2] \
            ]"
        )

        self.assert_is_not_precise_dimension(
            "*Broadcast[ \
                *name, \
                Tuple[*Ts] \
            ]"
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

        self.assert_is_tensor(
            ParametricType(
                "torch.Tensor",
                [
                    "torch.float32",
                    "typing_extensions.Literal[5]",
                    "pyre_extensions.IntExpression[5 + 3(N1//2)]",
                    "Variable[N (bound to int)]",
                    "*Tuple[Variable[N (bound to int)], typing_extensions.Literal[5], \
                    pyre_extensions.IntExpression[2N - 5]]",
                ],
            )
        )
        self.assert_is_tensor(ParametricType("torch.Tensor", []))

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
        self.assert_is_precise_tensor(
            ParametricType(
                "torch.Tensor",
                ["DType", "Variable[N1 (bound to int)]", "*Ts"],
            )
        )

        self.assert_is_precise_tensor(
            ParametricType(
                "torch.Tensor",
                [
                    "DType",
                    "Variable[N1 (bound to int)]",
                    "Variable[N2 (bound to int)]",
                    "*Ts",
                    "Variable[N3 (bound to int)]",
                    "Variable[N4 (bound to int)]",
                    "*Tuple[*Broadcast[Tuple[typing_extensions.Literal[1], \
                     typing_extensions.Literal[2]], Tuple[*Ts]]]",
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

        self.assert_is_not_precise_tensor(
            ParametricType(
                "torch.Tensor",
                [
                    "torch.float32",
                    "*Tuple[Variable[N1 (bound to int)], ...]",
                ],
            )
        )

        self.assert_is_not_precise_tensor(
            ParametricType(
                "torch.Tensor", ["torch.float32", "typing_extensions.Literal[2]", "Any"]
            )
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
            "torch.Tensor[]", ParametricType("torch.Tensor", [])
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


class ExtractSubstringTests(unittest.TestCase):
    def assert_extracts_as(
        self,
        line: str,
        line_number: int,
        start_position: Position,
        stop_position: Position,
        expected: str,
    ) -> None:
        self.assertEqual(
            _extract_substring(line, line_number, start_position, stop_position),
            expected,
        )

    def test_extract_substring(self) -> None:
        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=1,
            start_position=Position(1, 0),
            stop_position=Position(1, 19),
            expected="linear1(linear2(x))",
        )
        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=1,
            start_position=Position(1, 7),
            stop_position=Position(1, 19),
            expected="(linear2(x))",
        )
        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=1,
            start_position=Position(1, 0),
            stop_position=Position(1, 7),
            expected="linear1",
        )
        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=1,
            start_position=Position(1, 16),
            stop_position=Position(1, 17),
            expected="x",
        )

        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=3,
            start_position=Position(1, 0),
            stop_position=Position(5, 0),
            expected="linear1(linear2(x))",
        )

        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=1,
            start_position=Position(1, 7),
            stop_position=Position(2, 4),
            expected="(linear2(x))",
        )

        self.assert_extracts_as(
            line="linear1(linear2(x))",
            line_number=2,
            start_position=Position(1, 1),
            stop_position=Position(2, 7),
            expected="linear1",
        )

        with self.assertRaises(AssertionError):
            _extract_substring(
                line="linear1(linear2(x))",
                line_number=50,
                start_position=Position(1, 0),
                stop_position=Position(5, 0),
            )


class ExtractMultilineTextTests(unittest.TestCase):
    def assert_extract_text_as(
        self,
        corpus: List[str],
        start: Position,
        stop: Position,
        expected: str,
    ) -> None:
        self.assertEqual(_extract_multiline_text(corpus, start, stop), expected)

    def test_extract_text(self) -> None:
        corpus = ["bar(", "    x,", "    y,", ")"]
        self.assert_extract_text_as(
            corpus,
            start=Position(1, 0),
            stop=Position(4, 1),
            expected="bar(     x,     y, )",
        )
        self.assert_extract_text_as(
            corpus,
            start=Position(1, 3),
            stop=Position(4, 1),
            expected="(     x,     y, )",
        )
        self.assert_extract_text_as(
            corpus,
            start=Position(1, 0),
            stop=Position(2, 5),
            expected="bar(     x",
        )

        self.assert_extract_text_as(
            corpus,
            start=Position(1, 0),
            stop=Position(1, 3),
            expected="bar",
        )
        self.assert_extract_text_as(
            corpus,
            start=Position(1, 1),
            stop=Position(1, 4),
            expected="ar(",
        )
        self.assert_extract_text_as(
            corpus,
            start=Position(1, 0),
            stop=Position(1, 4),
            expected="bar(",
        )
