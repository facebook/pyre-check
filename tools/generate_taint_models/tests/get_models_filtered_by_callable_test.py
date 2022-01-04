# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import Callable, Iterable, List

from ..get_models_filtered_by_callable import ModelsFilteredByCallableGenerator
from ..model import Model
from ..model_generator import ModelGenerator


class TestModel(Model):
    def __init__(self, index: int) -> None:
        self.index = index

    def __eq__(self, other: "TestModel") -> int:
        return self.index == other.index

    def __hash__(self) -> int:
        # pyre-fixme[7]: Expected `int` but got implicit return value of `None`.
        pass


class TestModelGenerator(ModelGenerator[TestModel]):
    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> List[TestModel]:
        return [TestModel(0), TestModel(1), TestModel(2)]


def is_even_index(model: TestModel) -> bool:
    return model.index % 2 == 0


class ModelsFilteredByCallableGeneratorTest(unittest.TestCase):
    def test_compute_models(self) -> None:
        generator = ModelsFilteredByCallableGenerator(
            generator_to_filter=TestModelGenerator(), filter=is_even_index
        )

        self.assertListEqual(generator.compute_models([]), [TestModel(0), TestModel(2)])
