# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
import os
from abc import ABC, abstractmethod
from typing import Callable, Generic, Iterable, TypeVar

from .model import Model


LOG: logging.Logger = logging.getLogger(__name__)


def qualifier(root: str, path: str) -> str:
    path = os.path.relpath(path, root)
    if path.endswith(".pyi"):
        path = path[:-4]
    elif path.endswith(".py"):
        path = path[:-3]
    qualifier = path.replace("/", ".")
    if qualifier.endswith(".__init__"):
        qualifier = qualifier[:-9]
    return qualifier


T = TypeVar("T", bound=Model, covariant=True)


class ModelGenerator(ABC, Generic[T]):
    @abstractmethod
    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[T]:
        pass

    @abstractmethod
    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        pass

    def generate_models(self) -> Iterable[T]:
        return self.compute_models(self.gather_functions_to_model())
