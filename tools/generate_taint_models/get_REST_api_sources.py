# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .function_tainter import taint_callable_functions
from .model import CallableModel
from .model_generator import ModelGenerator
from .view_generator import DjangoUrls, get_all_views


class RESTApiSourceGenerator(ModelGenerator[CallableModel]):
    def __init__(
        self,
        django_urls: DjangoUrls,
        whitelisted_classes: Optional[List[str]] = None,
        whitelisted_views: Optional[List[str]] = None,
        taint_annotation: str = "TaintSource[UserControlled]",
    ) -> None:
        self.django_urls: DjangoUrls = django_urls
        self.whitelisted_classes: List[str] = whitelisted_classes or []
        self.whitelisted_views: List[str] = whitelisted_views or []
        self.taint_annotation = taint_annotation

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return get_all_views(self.django_urls)

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        return taint_callable_functions(
            functions_to_model,
            whitelisted_classes=self.whitelisted_classes,
            whitelisted_views=self.whitelisted_views,
            taint_annotation=self.taint_annotation,
        )
