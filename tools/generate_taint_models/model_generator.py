# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
import os
import time
from abc import ABC, abstractmethod
from typing import (
    Any,
    Callable,
    ClassVar,
    Dict,
    Iterable,
    List,
    Optional,
    Set,
    Type,
    Union,
)

from ...client import log_statistics
from .generator_specifications import DecoratorAnnotationSpecification
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


class ModelGenerator(ABC):
    @abstractmethod
    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        pass

    @abstractmethod
    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        pass

    def generate_models(self) -> Set[Model]:
        return set(self.compute_models(self.gather_functions_to_model()))
