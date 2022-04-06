# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import logging
import re
from abc import abstractmethod
from collections import defaultdict
from typing import Callable, Dict, Iterable, List, Optional, Set, Union

from .generator_specifications import DecoratorAnnotationSpecification
from .model import FunctionDefinitionModel
from .model_generator import ModelGenerator, qualifier
from .module_loader import find_all_paths, load_module

LOG: logging.Logger = logging.getLogger(__name__)
FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]


class FunctionVisitor(ast.NodeVisitor):
    def __init__(self) -> None:
        self.found_functions: Dict[
            DecoratorAnnotationSpecification, List[FunctionDefinition]
        ] = defaultdict(list)

    @abstractmethod
    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        pass

    @abstractmethod
    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        pass


class AnnotatedFunctionGenerator(ModelGenerator[FunctionDefinitionModel]):
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

    @abstractmethod
    def _annotate_functions(self, path: str) -> Iterable[FunctionDefinitionModel]:
        pass

    def _annotate_functions_with_visitor(
        self, path: str, visitor: FunctionVisitor
    ) -> Iterable[FunctionDefinitionModel]:
        module = load_module(path)

        if not module:
            return []

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
