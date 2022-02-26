# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from typing import Callable, Iterable, List, Optional

from ...api.connection import PyreConnection
from ...api.query import PyreCache
from .function_tainter import taint_pyre_functions
from .generator_specifications import AnnotationSpecification, WhitelistSpecification
from .model import PyreFunctionDefinitionModel
from .model_generator import ModelGenerator
from .subclass_generator import get_all_subclass_defines_from_pyre


LOG: logging.Logger = logging.getLogger(__name__)


class MethodsOfSubclassesGenerator(ModelGenerator[PyreFunctionDefinitionModel]):
    def __init__(
        self,
        base_classes: List[str],
        pyre_connection: PyreConnection,
        annotations: AnnotationSpecification,
        whitelist: Optional[WhitelistSpecification] = None,
        transitive: bool = False,
        pyre_cache: Optional[PyreCache] = None,
    ) -> None:
        self.base_classes = base_classes
        self.pyre_connection = pyre_connection
        self.pyre_cache = pyre_cache
        self.annotations = annotations
        self.whitelist: WhitelistSpecification = whitelist or WhitelistSpecification(
            parameter_name={"self"}
        )
        self.transitive = transitive

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> List[PyreFunctionDefinitionModel]:
        LOG.info(f"Finding methods on subclasses of {self.base_classes}")

        models: List[PyreFunctionDefinitionModel] = []

        definitions = get_all_subclass_defines_from_pyre(
            self.base_classes,
            pyre_connection=self.pyre_connection,
            transitive=self.transitive,
            pyre_cache=self.pyre_cache,
        )

        if definitions is None:
            LOG.error(f"No definitions found for base classes: {self.base_classes}")
            return []

        for target, defines in definitions.items():
            LOG.debug(f"For {target}, found defines: {defines}")
            models.extend(
                taint_pyre_functions(
                    defines, annotations=self.annotations, whitelist=self.whitelist
                )
            )

        LOG.debug(f"Outputting {len(models)} models")
        return models
