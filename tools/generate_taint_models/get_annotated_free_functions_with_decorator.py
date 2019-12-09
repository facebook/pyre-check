# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import logging
from dataclasses import dataclass
from typing import Callable, Iterable, List, Set, Tuple, Union

from .generator_specs import DecoratorAnnotationSpec
from .model import FunctionDefinitionModel
from .model_generator import Configuration, ModelGenerator, Registry, qualifier
from .module_loader import find_all_paths, load_module


LOG: logging.Logger = logging.getLogger(__name__)
FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]


class AnnotatedFreeFunctionWithDecoratorGenerator(ModelGenerator):
    def _annotate_fns(
        self, spec: DecoratorAnnotationSpec, root: str, path: str
    ) -> Iterable[str]:

        found_functions: Set[FunctionDefinition] = set()
        module = load_module(path)

        if not module:
            return []

        @dataclass
        class Decorator:
            name: str
            arguments: Set[str] = None
            keywords: Set[Tuple[str, str]] = None

            def has_attributes(self) -> bool:
                return self.arguments or self.keywords

        class FreeFunctionVisitor(ast.NodeVisitor):
            def __init__(self, target_decorator: str) -> None:
                self.target_decorator_list: List[Decorator] = []
                self._parse_target_decorator(target_decorator)

            def _parse_target_decorator(self, target_decorator: str) -> None:
                """
                Responsible for parsing the target decorator to extract the
                decorator name, named and unnamed attributes.
                """
                # We need to create a well formed decorator so we attach a bogus
                # function to the decorators.
                well_formed_decorator = target_decorator + """\ndef foo(): ..."""
                try:
                    parsed_ast = ast.parse(well_formed_decorator)
                except SyntaxError as err:
                    LOG.error("The decorator format was unrecognizable.")
                    raise err

                decorator_list = parsed_ast.body[0].decorator_list
                if len(decorator_list) < 1:
                    LOG.error("No target decorators were specified.")
                    raise Exception("No target decorators were specified.")

                for decorator in decorator_list:
                    if isinstance(decorator, ast.Call):
                        target_decorator_name = decorator.func.id
                        target_decorator_arguments = {
                            argument.s
                            for argument in decorator.args
                            if isinstance(argument, ast.Str)
                        }
                        target_decorator_keywords = {
                            (keyword.arg, keyword.value.s)
                            for keyword in decorator.keywords
                            if isinstance(keyword.value, ast.Str)
                        }
                        self.target_decorator_list.append(
                            Decorator(
                                target_decorator_name,
                                target_decorator_arguments,
                                target_decorator_keywords,
                            )
                        )
                    else:
                        self.target_decorator_list.append(
                            Decorator(decorator.id, set(), set())
                        )

            def _are_attributes_matching(
                self, decorator: ast.Call, target_decorator: Decorator
            ) -> bool:
                # Handle unnamed attributes.
                decorator_arguments_set = {
                    argument.s
                    for argument in decorator.args
                    if isinstance(argument, ast.Str)
                }
                # Handle named attributes.
                decorator_keywords_set = {
                    (keyword.arg, keyword.value.s)
                    for keyword in decorator.keywords
                    if isinstance(keyword.value, ast.Str)
                }

                return target_decorator.arguments.issubset(
                    decorator_arguments_set
                ) and target_decorator.keywords.issubset(decorator_keywords_set)

            def handle_functions(self, node: FunctionDefinition) -> None:
                ## TODO T58744796: In the future, change this to support
                ## filtering on multiple decorators.
                target_decorator: Decorator = self.target_decorator_list[0]
                for decorator in node.decorator_list:
                    if (
                        isinstance(decorator, ast.Name)
                        and decorator.id == target_decorator.name
                        and not target_decorator.has_attributes()
                    ):
                        found_functions.add(node)
                    elif (
                        isinstance(decorator, ast.Call)
                        and isinstance(decorator.func, ast.Name)
                        and decorator.func.id == target_decorator.name
                        and self._are_attributes_matching(decorator, target_decorator)
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
