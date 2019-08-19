# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
from typing import Callable, Iterable, Set, Union

from .generator_specs import DecoratorAnnotationSpec
from .model import FunctionDefinitionModel
from .model_generator import Configuration, ModelGenerator, Registry, qualifier
from .module_loader import find_all_paths, load_module


FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]


class AnnotatedFreeFunctionWithDecoratorGenerator(ModelGenerator):
    def _annotate_fns(
        self, spec: DecoratorAnnotationSpec, root: str, path: str
    ) -> Iterable[str]:

        found_functions: Set[FunctionDefinition] = set()
        module = load_module(path)

        if not module:
            return []

        class FreeFunctionVisitor(ast.NodeVisitor):
            target_decorator: str

            def __init__(self, target_decorator: str) -> None:
                self.target_decorator = target_decorator

            def handle_functions(self, node: FunctionDefinition) -> None:
                for decorator in node.decorator_list:
                    if (
                        isinstance(decorator, ast.Name)
                        and decorator.id == self.target_decorator
                    ) or (
                        isinstance(decorator, ast.Call)
                        and isinstance(decorator.func, ast.Name)
                        # pyre-fixme[16]: `expr` has no attribute `id`.
                        and decorator.func.id == self.target_decorator
                    ):
                        found_functions.add(node)

            def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
                self.handle_functions(node)

            def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
                self.handle_functions(node)

            def visit_ClassDef(self, node: ast.ClassDef) -> None:
                # We only want free functions, so we should stop traversing the
                # tree once we see a class definition
                pass

        visitor = FreeFunctionVisitor(spec.decorator)
        visitor.visit(module)

        module_qualifier = qualifier(root, path)

        models: Set[str] = set()

        for found_function in found_functions:
            models.add(
                FunctionDefinitionModel(
                    qualifier=module_qualifier,
                    definition=found_function,
                    arg=spec.arg_annotation,
                    vararg=spec.vararg_annotation,
                    kwarg=spec.kwarg_annotation,
                    returns=spec.return_annotation,
                ).generate()
            )

        return models

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        annotated_fns = set()

        for path in find_all_paths():
            for spec in Configuration.annotation_specs:
                annotated_fns.update(self._annotate_fns(spec, Configuration.root, path))

        return sorted(annotated_fns)


Registry.register(
    "get_annotated_free_functions_with_decorator",
    AnnotatedFreeFunctionWithDecoratorGenerator,
    include_by_default=False,
)
