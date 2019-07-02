import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_name, extract_view_name
from .model_generator import ModelGenerator
from .taint_annotator import Model


class GraphQLSourceGenerator(ModelGenerator):
    def __init__(self) -> None:
        pass

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        graphql_models = set()

        for view_function in functions_to_model:
            model = Model(
                vararg=": TaintSource[UserControlled]",
                kwarg=": TaintSource[UserControlled]",
            )
            callable = model.generate(view_function)
            if callable is not None:
                graphql_models.add(callable)

        return sorted(graphql_models)
