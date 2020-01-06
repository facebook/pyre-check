import logging
from typing import Callable, Iterable

from .get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .get_REST_api_sources import RESTApiSourceGenerator
from .model import Model
from .model_generator import ModelGenerator, Registry


LOG: logging.Logger = logging.getLogger(__name__)


class FilteredSourceGenerator(ModelGenerator):
    def __init__(self) -> None:
        self.superset_generator = RESTApiSourceGenerator(
            whitelisted_classes=["ViewerContext", "AuthenticatedVC"],
            arg="TaintSource[DataFromGET]",
            vararg="TaintSource[DataFromGET]",
            kwarg="TaintSource[DataFromGET]",
        )
        self.subset_generator = AnnotatedFreeFunctionWithDecoratorGenerator()

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        superset_generator_functions = self.superset_generator.generate_models()
        subset_generator_functions = self.subset_generator.generate_models()
        all_functions = superset_generator_functions - subset_generator_functions

        return all_functions


Registry.register(
    "get_filtered_sources", FilteredSourceGenerator, include_by_default=False
)
