# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import logging
from typing import Callable, Iterable, List, Optional

from .constructor_generator import gather_all_constructors_in_hierarchy
from .function_tainter import taint_callable_functions
from .model import CallableModel
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class ClassSourceGenerator(ModelGenerator[CallableModel]):
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
        classes_to_taint: List[str],
        whitelisted_classes: Optional[List[str]] = None,
        whitelisted_views: Optional[List[str]] = None,
        taint_annotation: str = "TaintSource[UserControlled]",
    ) -> None:
        self.classes_to_taint: List[str] = classes_to_taint
        self.whitelisted_classes: List[str] = whitelisted_classes or []
        self.whitelisted_views: List[str] = whitelisted_views or []
        self.taint_annotation: str = taint_annotation

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return gather_all_constructors_in_hierarchy(self.classes_to_taint)

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        return taint_callable_functions(
            functions_to_model,
            taint_annotation=self.taint_annotation,
            whitelisted_views=self.whitelisted_views,
            whitelisted_classes=self.whitelisted_classes,
        )
