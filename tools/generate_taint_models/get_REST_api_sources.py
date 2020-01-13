# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .function_tainter import FunctionTainter
from .model import Model
from .model_generator import Configuration, Registry
from .view_generator import ViewGenerator


class RESTApiSourceGenerator(ViewGenerator):
    def __init__(
        self,
        whitelisted_classes: Optional[List[str]] = None,
        whitelisted_views: Optional[List[str]] = None,
        taint_annotation: str = "TaintSource[UserControlled]",
    ) -> None:
        self.whitelisted_classes: List[str] = (
            whitelisted_classes or Configuration.whitelisted_classes
        )
        self.whitelisted_views: List[
            str
        ] = whitelisted_views or Configuration.whitelisted_views
        self.taint_annotation = taint_annotation

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        return FunctionTainter(
            whitelisted_classes=self.whitelisted_classes,
            whitelisted_views=self.whitelisted_views,
            arg=self.taint_annotation,
            vararg=self.taint_annotation,
            kwarg=self.taint_annotation,
        ).taint_functions(functions_to_model)


Registry.register(
    "get_REST_api_sources", RESTApiSourceGenerator, include_by_default=True
)
