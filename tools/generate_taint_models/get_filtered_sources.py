import logging
from typing import Callable, Iterable

from .generator_specifications import DecoratorAnnotationSpecification
from .get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .get_REST_api_sources import RESTApiSourceGenerator
from .model import Model
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class FilteredSourceGenerator(ModelGenerator):
    def __init__(
        self, superset_generator: ModelGenerator, subset_generator: ModelGenerator
    ) -> None:
        self.superset_generator = superset_generator
        self.subset_generator = subset_generator

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        LOG.info("Computing models for the superset...")
        superset_models = self.superset_generator.generate_models()
        LOG.info("Computing models for the subset...")
        subset_models = self.subset_generator.generate_models()
        return set(superset_models) - set(subset_models)
