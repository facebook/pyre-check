import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_name, extract_view_name
from .model_generator import ModelGenerator
from .taint_annotator import Model


class GraphQLSourceGenerator(ModelGenerator):
    def __init__(self) -> None:
        pass

    def compute_models(self, visit_all_views: Callable[..., None]) -> Iterable[str]:
        graphql_models = set()

        def graphql_visitor(view_function: Callable[..., None]) -> None:
            model = Model(
                vararg=": TaintSource[UserControlled]",
                kwarg=": TaintSource[UserControlled]",
            )
            callable = model.generate(view_function)
            if callable is not None:
                graphql_models.add(callable)

        visit_all_views(graphql_visitor)
        return sorted(graphql_models)
