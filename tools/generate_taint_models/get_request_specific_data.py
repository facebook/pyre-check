# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Callable, Iterable, List, Optional

from .function_tainter import taint_callable_functions
from .model import CallableModel
from .model_generator import ModelGenerator
from .view_generator import DjangoUrls, get_all_views


class RequestSpecificDataGenerator(ModelGenerator[CallableModel]):
    def __init__(
        self,
        django_urls: DjangoUrls,
        whitelisted_views: Optional[List[str]] = None,
        whitelisted_classes: Optional[List[str]] = None,
    ) -> None:
        self.django_urls: DjangoUrls = django_urls
        self.whitelisted_views: List[str] = whitelisted_views or []
        self.whitelisted_classes: List[str] = whitelisted_classes or []

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        django_urls = self.django_urls
        if django_urls is None:
            return []
        return get_all_views(django_urls)

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> List[CallableModel]:
        taint_annotation = "TaintSource[RequestSpecificData]"
        return taint_callable_functions(
            functions_to_model,
            taint_annotation=taint_annotation,
            whitelisted_views=self.whitelisted_views,
            whitelisted_classes=self.whitelisted_classes,
        )
