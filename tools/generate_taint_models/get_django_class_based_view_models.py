# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
import re
from typing import Callable, Iterable, List, Optional

from ...api.connection import PyreConnection
from ...api.query import PyreCache
from .generator_specifications import (
    AnnotationSpecification,
    default_entrypoint_taint,
    WhitelistSpecification,
)
from .get_methods_of_subclasses import MethodsOfSubclassesGenerator
from .get_models_filtered_by_callable import ModelsFilteredByCallableGenerator
from .model import PyreFunctionDefinitionModel
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class DjangoClassBasedViewModels(ModelGenerator[PyreFunctionDefinitionModel]):
    def __init__(
        self,
        pyre_connection: PyreConnection,
        annotations: Optional[AnnotationSpecification] = None,
        whitelist: Optional[WhitelistSpecification] = None,
        pyre_cache: Optional[PyreCache] = None,
    ) -> None:
        self.pyre_connection = pyre_connection
        self.pyre_cache = pyre_cache
        self.annotations: AnnotationSpecification = (
            annotations or default_entrypoint_taint
        )
        self.whitelist: WhitelistSpecification = whitelist or WhitelistSpecification(
            parameter_name={"self", "cls", "request"},
            parameter_type={"django.http.HttpRequest"},
        )

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> List[PyreFunctionDefinitionModel]:
        pattern: re.Pattern = re.compile(
            "(get|post|put|patch|delete|head|options|trace)"
        )

        def matches_pattern(method: PyreFunctionDefinitionModel) -> bool:
            return bool(pattern.search(method.callable_name))

        return list(
            ModelsFilteredByCallableGenerator(
                generator_to_filter=MethodsOfSubclassesGenerator(
                    base_classes=["django.views.generic.base.View"],
                    pyre_connection=self.pyre_connection,
                    pyre_cache=self.pyre_cache,
                    annotations=self.annotations,
                    whitelist=self.whitelist,
                ),
                filter=matches_pattern,
            ).generate_models()
        )
