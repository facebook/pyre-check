# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .function_tainter import taint_functions
from .model import Model
from .model_generator import Configuration, ModelGenerator, Registry
from .view_generator import DynamicURLType, get_all_views


class RESTApiSourceGenerator(ModelGenerator):
    def __init__(
        self,
        urls_module: Optional[str] = None,
        url_resolver_type: Optional[DynamicURLType] = None,
        url_pattern_type: Optional[DynamicURLType] = None,
        whitelisted_classes: Optional[List[str]] = None,
        whitelisted_views: Optional[List[str]] = None,
        taint_annotation: str = "TaintSource[UserControlled]",
    ) -> None:
        self.urls_module: Optional[str] = urls_module or Configuration.urls_module
        self.url_resolver_type: DynamicURLType = (
            url_resolver_type or Configuration.url_resolver_type
        )
        self.url_pattern_type: DynamicURLType = (
            url_pattern_type or Configuration.url_pattern_type
        )
        self.whitelisted_classes: List[str] = (
            whitelisted_classes or Configuration.whitelisted_classes
        )
        self.whitelisted_views: List[
            str
        ] = whitelisted_views or Configuration.whitelisted_views
        self.taint_annotation = taint_annotation

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        urls_module = self.urls_module
        if urls_module is None:
            return []
        return get_all_views(
            urls_module=urls_module,
            url_resolver_type=self.url_resolver_type,
            url_pattern_type=self.url_pattern_type,
        )

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        return taint_functions(
            functions_to_model,
            whitelisted_classes=self.whitelisted_classes,
            whitelisted_views=self.whitelisted_views,
            taint_annotation=self.taint_annotation,
        )


Registry.register(
    "get_REST_api_sources", RESTApiSourceGenerator, include_by_default=True
)
