import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_name, extract_view_name
from .model_generator import ModelGenerator


class GraphQLSourceGenerator(ModelGenerator):
    def __init__(self) -> None:
        pass

    def compute_models(self, visit_all_views: Callable[..., None]) -> Iterable[str]:
        graphql_models = set()

        def graphql_visitor(view_function: Callable[..., None]) -> None:
            view_name = extract_view_name(view_function)
            if isinstance(view_function, types.FunctionType):
                callable = inspect.signature(view_function)
            elif isinstance(view_function, types.MethodType):
                # pyre-ignore
                callable = inspect.signature(view_function.__func__)
            else:
                return
            parameters = []
            for parameter_name in callable.parameters:
                parameter = callable.parameters[parameter_name]
                if (
                    parameter.kind == inspect.Parameter.VAR_KEYWORD
                    or parameter.kind == inspect.Parameter.VAR_POSITIONAL
                ):
                    parameters.append(
                        f"{extract_name(parameter)}: TaintSource[UserControlled]"
                    )
                else:
                    parameters.append(extract_name(parameter))
            graphql_models.add(f"def {view_name}({', '.join(parameters)}): ...")

        visit_all_views(graphql_visitor)
        return sorted(graphql_models)
