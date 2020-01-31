import inspect
import logging
from importlib import import_module
from typing import Callable, Iterable, List

from .generator_specifications import DecoratorAnnotationSpecification
from .get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .model import Model
from .model_generator import CallableModelGenerator, ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class UndecoratedSourceGenerator(ModelGenerator):
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
        source_generator: CallableModelGenerator,
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
    ) -> Iterable[Model]:
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
            paths=paths,
        ).generate_models()
        return set(unfiltered_models) - set(models_to_filter)
