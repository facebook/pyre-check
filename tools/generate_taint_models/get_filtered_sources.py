# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from typing import Callable, Iterable

from .model import Model
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class FilteredSourceGenerator(ModelGenerator[Model]):
    def __init__(
        self,
        superset_generator: ModelGenerator[Model],
        subset_generator: ModelGenerator[Model],
    ) -> None:
        self.superset_generator = superset_generator
        self.subset_generator = subset_generator

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        LOG.info("Computing models for the superset...")
        superset_models = self.superset_generator.generate_models()
        LOG.info("Computing models for the subset...")
        subset_models = self.subset_generator.generate_models()
        return set(superset_models) - set(subset_models)
