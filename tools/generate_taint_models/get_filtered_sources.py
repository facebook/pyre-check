import logging
from typing import Callable, Iterable, List, Optional

from .generator_specifications import DecoratorAnnotationSpecification
from .get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .get_REST_api_sources import RESTApiSourceGenerator
from .model import Model
from .model_generator import ModelGenerator, Registry


LOG: logging.Logger = logging.getLogger(__name__)


class FilteredSourceGenerator(ModelGenerator):
    def __init__(
        self,
        root: Optional[str] = None,
        annotation_specifications: Optional[
            List[DecoratorAnnotationSpecification]
        ] = None,
        superset_generator: Optional[ModelGenerator] = None,
        subset_generator: Optional[ModelGenerator] = None,
    ) -> None:
        self.superset_generator: ModelGenerator = (
            superset_generator
            or RESTApiSourceGenerator(
                whitelisted_classes=["ViewerContext", "AuthenticatedVC"],
                taint_annotation="TaintSource[DataFromGET]",
            )
        )
        self.subset_generator: ModelGenerator = (
            subset_generator
            or AnnotatedFreeFunctionWithDecoratorGenerator(
                root=root, annotation_specifications=annotation_specifications
            )
        )

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        return (
            self.superset_generator.generate_models()
            - self.subset_generator.generate_models()
        )


Registry.register(
    "get_filtered_sources", FilteredSourceGenerator, include_by_default=False
)
