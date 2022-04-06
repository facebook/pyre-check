# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import logging
from typing import Dict, Iterable, List

from .annotated_function_generator import AnnotatedFunctionGenerator, FunctionVisitor
from .decorator_parser import DecoratorParser
from .generator_specifications import DecoratorAnnotationSpecification
from .model import FunctionDefinitionModel


LOG: logging.Logger = logging.getLogger(__name__)


class FreeFunctionWithDecoratorVisitor(FunctionVisitor):
    def __init__(
        self, target_decorators: List[DecoratorAnnotationSpecification]
    ) -> None:
        super().__init__()
        self.decorator_parsers: Dict[
            DecoratorAnnotationSpecification, DecoratorParser
        ] = {
            target_decorator: DecoratorParser(target_decorator.decorator)
            for target_decorator in target_decorators
        }

    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        for decorator_specification, parser in self.decorator_parsers.items():
            if parser.function_matches_target_decorators(node):
                self.found_functions[decorator_specification].append(node)

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        for decorator_specification, parser in self.decorator_parsers.items():
            if parser.function_matches_target_decorators(node):
                self.found_functions[decorator_specification].append(node)

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        # We only want free functions, so we should stop traversing the
        # tree once we see a class definition
        pass


class AnnotatedFreeFunctionWithDecoratorGenerator(AnnotatedFunctionGenerator):
    def _annotate_functions(self, path: str) -> Iterable[FunctionDefinitionModel]:
        visitor = FreeFunctionWithDecoratorVisitor(self.annotation_specifications)
        return self._annotate_functions_with_visitor(path, visitor)
