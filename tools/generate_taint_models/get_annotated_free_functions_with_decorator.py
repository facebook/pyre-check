# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import logging
from dataclasses import dataclass
from typing import Callable, Iterable, List, Optional, Set, Tuple, Union

from .decorator_parser import DecoratorParser
from .generator_specifications import DecoratorAnnotationSpecification
from .model import FunctionDefinitionModel, Model
from .model_generator import ModelGenerator, qualifier
from .module_loader import find_all_paths, load_module


LOG: logging.Logger = logging.getLogger(__name__)
FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]


class AnnotatedFreeFunctionWithDecoratorGenerator(ModelGenerator):
    def __init__(
        self,
        root: str,
        annotation_specifications: List[DecoratorAnnotationSpecification],
    ) -> None:
        self.root: str = root
        self.annotation_specifications: List[DecoratorAnnotationSpecification] = (
            annotation_specifications
        )

    def _annotate_functions(
        self, annotation_specification: DecoratorAnnotationSpecification, path: str
    ) -> Iterable[Model]:

        module = load_module(path)

        if not module:
            return []

        class FreeFunctionVisitor(ast.NodeVisitor):
            def __init__(self, target_decorator: str) -> None:
                self.decorator_parser = DecoratorParser(target_decorator)
                self.found_functions: List[FunctionDefinition] = []

            def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
                if self.decorator_parser.function_matches_target_decorators(node):
                    self.found_functions.append(node)

            def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
                if self.decorator_parser.function_matches_target_decorators(node):
                    self.found_functions.append(node)

            def visit_ClassDef(self, node: ast.ClassDef) -> None:
                # We only want free functions, so we should stop traversing the
                # tree once we see a class definition
                pass

        visitor = FreeFunctionVisitor(annotation_specification.decorator)
        visitor.visit(module)

        module_qualifier = qualifier(self.root, path)

        models: Set[FunctionDefinitionModel] = set()
        for found_function in visitor.found_functions:
            try:
                function_definition_model = FunctionDefinitionModel(
                    qualifier=module_qualifier,
                    definition=found_function,
                    arg=annotation_specification.arg_annotation,
                    vararg=annotation_specification.vararg_annotation,
                    kwarg=annotation_specification.kwarg_annotation,
                    returns=annotation_specification.return_annotation,
                )
                models.add(function_definition_model)
            except ValueError:
                pass

        return models

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        annotated_functions = set()

        for path in find_all_paths(self.root):
            for annotation_specification in self.annotation_specifications:
                annotated_functions.update(
                    self._annotate_functions(annotation_specification, path)
                )

        return sorted(annotated_functions)
