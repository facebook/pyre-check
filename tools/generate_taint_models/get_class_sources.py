# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import logging
from typing import Callable, Iterable, List, Optional

from .constructor_generator import ConstructorGenerator
from .function_tainter import taint_functions
from .model import Model
from .model_generator import Configuration, Registry


LOG: logging.Logger = logging.getLogger(__name__)


class ClassSourceGenerator(ConstructorGenerator):
    """
    This Generator uses classes_to_taint to taint the __init__
    functions of the classes passed as fully qualified strings. All recursive
    subclasses that have had their modules loaded at preprocessing time will
    also be tainted. The purpose of using this flag would be if it is not
    possible for the type system to assess full inheritance statically
    (ex: dynamic subclassing).
    """

    def __init__(
        self,
        whitelisted_classes: Optional[List[str]] = None,
        whitelisted_views: Optional[List[str]] = None,
        classes_to_taint: Optional[List[str]] = None,
    ) -> None:
        super().__init__(classes_to_taint or Configuration.classes_to_taint)
        self.whitelisted_classes: List[str] = (
            whitelisted_classes or Configuration.whitelisted_classes
        )
        self.whitelisted_views: List[str] = (
            whitelisted_views or Configuration.whitelisted_views
        )

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        return taint_functions(
            functions_to_model,
            whitelisted_views=self.whitelisted_views,
            whitelisted_classes=self.whitelisted_classes,
        )


Registry.register("get_class_sources", ClassSourceGenerator, include_by_default=True)
