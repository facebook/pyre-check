# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .function_tainter import taint_callable_functions
from .generator_specifications import (
    AnnotationSpecification,
    default_entrypoint_taint,
    WhitelistSpecification,
)
from .model import CallableModel
from .model_generator import ModelGenerator
from .view_generator import DjangoUrls, get_all_views


class RESTApiSourceGenerator(ModelGenerator[CallableModel]):
    annotations: AnnotationSpecification
    whitelisted_parameters: WhitelistSpecification

    def __init__(
        self,
        django_urls: DjangoUrls,
        annotations: Optional[AnnotationSpecification] = None,
        whitelisted_parameters: Optional[WhitelistSpecification] = None,
        whitelisted_views: Optional[List[str]] = None,
    ) -> None:
        self.django_urls: DjangoUrls = django_urls
        self.annotations = annotations or default_entrypoint_taint
        self.whitelisted_parameters = whitelisted_parameters or WhitelistSpecification(
            parameter_name={"self"}, parameter_type={"HttpRequest"}
        )
        self.whitelisted_views: List[str] = whitelisted_views or []

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return get_all_views(self.django_urls)

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        return taint_callable_functions(
            functions_to_model,
            whitelisted_views=self.whitelisted_views,
            annotations=self.annotations,
            whitelist=self.whitelisted_parameters,
        )
