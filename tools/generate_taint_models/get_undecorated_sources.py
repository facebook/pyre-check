# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import inspect
import logging
from importlib import import_module
from typing import Callable, Iterable, List, Set

from .generator_specifications import DecoratorAnnotationSpecification
from .get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .model import CallableModel
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class UndecoratedSourceGenerator(ModelGenerator[CallableModel]):
    """
    This generator allows you to filter the results of `source_generator` by callables
    that do not implement the specified decorator annotations.

    For instance, if you're interested in getting all REST endpoints that don't
    implement a @sanitized_data decorator, you could use this generator via the
    following pattern:

    ...
    generators["get_unsanitized_REST_endpoints"] = UndecoratedSourceGenerator(
      source_generator = RESTApiSourceGenerator(django_urls),
      decorators_to_filter = [
        DecoratorAnnotationSpecification(decorator="sanitized_data"),
      ],
    )
    """

    def __init__(
        self,
        source_generator: ModelGenerator[CallableModel],
        root: str,
        decorators_to_filter: List[DecoratorAnnotationSpecification],
    ) -> None:
        self.source_generator = source_generator
        self.root = root
        self.decorators_to_filter = decorators_to_filter

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Set[CallableModel]:
        unfiltered_models = self.source_generator.generate_models()
        modules_to_filter = set()
        for callable_model in unfiltered_models:
            if inspect.ismethod(callable_model.callable_object):
                continue
            module_name = getattr(callable_model.callable_object, "__module__", None)
            if module_name is not None:
                modules_to_filter.add(module_name)
        paths = [
            import_module(module_name).__file__ for module_name in modules_to_filter
        ]

        models_to_filter = AnnotatedFreeFunctionWithDecoratorGenerator(
            root=self.root,
            annotation_specifications=self.decorators_to_filter,
            # pyre-fixme[6]: For 3rd param expected `Optional[List[str]]` but got
            #  `List[Optional[str]]`.
            paths=paths,
        ).generate_models()
        # pyre-fixme[58]: `-` is not supported for operand types
        #  `Set[CallableModel]` and
        #  `Set[tools.pyre.tools.generate_taint_models.model.FunctionDefinitionModel]`.
        return set(unfiltered_models) - set(models_to_filter)
