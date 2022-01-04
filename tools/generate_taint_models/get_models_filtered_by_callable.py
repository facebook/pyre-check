# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from typing import Callable, Iterable, List, TypeVar

from .model import Model
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


T = TypeVar("T", bound=Model)


class ModelsFilteredByCallableGenerator(ModelGenerator[T]):
    def __init__(
        self, generator_to_filter: ModelGenerator[T], filter: Callable[[T], bool]
    ) -> None:
        self.generator_to_filter = generator_to_filter
        self.filter = filter

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return self.generator_to_filter.gather_functions_to_model()

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> List[T]:
        return [
            model
            for model in self.generator_to_filter.compute_models(functions_to_model)
            if self.filter(model)
        ]
