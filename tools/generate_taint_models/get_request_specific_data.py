# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Callable, Iterable, List, Optional

from .function_tainter import FunctionTainter
from .inspect_parser import extract_qualified_name
from .model import CallableModel, Model
from .model_generator import Configuration, Registry
from .view_generator import DynamicURLType, ViewGenerator


class RequestSpecificDataGenerator(ViewGenerator):
    def __init__(
        self,
        urls_module: Optional[str] = None,
        url_resolver_type: Optional[DynamicURLType] = None,
        url_pattern_type: Optional[DynamicURLType] = None,
        whitelisted_views: Optional[List[str]] = None,
        whitelisted_classes: Optional[List[str]] = None,
    ) -> None:
        super().__init__(urls_module, url_resolver_type, url_pattern_type)
        self.whitelisted_views: List[
            str
        ] = whitelisted_views or Configuration.whitelisted_views
        self.whitelisted_classes: List[str] = (
            whitelisted_classes or Configuration.whitelisted_classes
        )

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        taint_annotation = "TaintSource[RequestSpecificData]"
        return FunctionTainter(
            whitelisted_views=self.whitelisted_views,
            whitelisted_classes=self.whitelisted_classes,
            arg=taint_annotation,
            kwarg=taint_annotation,
            vararg=taint_annotation,
        ).taint_functions(functions_to_model)


Registry.register(
    "get_request_specific_data", RequestSpecificDataGenerator, include_by_default=False
)
