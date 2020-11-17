# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import logging
import re
from collections import defaultdict
from typing import Callable, Dict, Iterable, List, Optional, Set, Union

from .decorator_parser import DecoratorParser
from .generator_specifications import DecoratorAnnotationSpecification
from .model import FunctionDefinitionModel
from .model_generator import ModelGenerator, qualifier
from .module_loader import find_all_paths, load_module


LOG: logging.Logger = logging.getLogger(__name__)
FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]


class AnnotatedFreeFunctionWithDecoratorGenerator(
    ModelGenerator[FunctionDefinitionModel]
):
    def __init__(
        self,
        root: str,
        annotation_specifications: List[DecoratorAnnotationSpecification],
        paths: Optional[List[str]] = None,
        exclude_paths: Optional[List[re.Pattern]] = None,
    ) -> None:
        self._paths: Optional[List[str]] = paths
        self.exclude_paths: List[re.Pattern] = exclude_paths or []
        self.root = root
        self.annotation_specifications: List[
            DecoratorAnnotationSpecification
        ] = annotation_specifications

    @property
    def paths(self) -> List[str]:
        paths = self._paths
        if paths is None:
            paths = list(find_all_paths(self.root))
            self._paths = paths
        return [
            path
            for path in paths
            if all(not exclude.search(path) for exclude in self.exclude_paths)
        ]

    def _annotate_functions(self, path: str) -> Iterable[FunctionDefinitionModel]:

        module = load_module(path)

        if not module:
            return []

        class FreeFunctionVisitor(ast.NodeVisitor):
            def __init__(
                self, target_decorators: List[DecoratorAnnotationSpecification]
            ) -> None:
                self.decorator_parsers: Dict[
                    DecoratorAnnotationSpecification, DecoratorParser
                ] = {
                    target_decorator: DecoratorParser(target_decorator.decorator)
                    for target_decorator in target_decorators
                }

                self.found_functions: Dict[
                    DecoratorAnnotationSpecification, List[FunctionDefinition]
                ] = defaultdict(list)

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

        visitor = FreeFunctionVisitor(self.annotation_specifications)
        visitor.visit(module)

        module_qualifier = qualifier(self.root, path)

        models: Set[FunctionDefinitionModel] = set()
        for specification, found_functions in visitor.found_functions.items():
            for found_function in found_functions:
                try:
                    function_definition_model = FunctionDefinitionModel(
                        qualifier=module_qualifier,
                        definition=found_function,
                        annotations=specification.annotations,
                        whitelist=specification.whitelist,
                    )
                    models.add(function_definition_model)
                except ValueError:
                    pass

        return models

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[FunctionDefinitionModel]:
        annotated_functions = set()

        for path in self.paths:
            annotated_functions.update(self._annotate_functions(path))

        return sorted(annotated_functions)
