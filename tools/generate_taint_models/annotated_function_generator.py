# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import logging
import os
import re
import multiprocessing
import itertools
from abc import abstractmethod
from collections import defaultdict
from typing import (
    Callable,
    Dict,
    Iterable,
    Iterator,
    Tuple,
    List,
    Optional,
    Set,
    Union,
    TypeVar,
)

from .generator_specifications import DecoratorAnnotationSpecification
from .model import FunctionDefinitionModel
from .model_generator import ModelGenerator, qualifier
from .module_loader import find_all_paths, load_module

LOG: logging.Logger = logging.getLogger(__name__)
FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]

T = TypeVar("T")


def _batched(iterable: Iterable[T], n: int) -> Iterator[Tuple[T, ...]]:
    """
    Batch the input iterable in chunks of `n` elements.
    For instance: batched([0, 1, 2, 3, 4], 2) = [[0, 1], [2, 3], [4]]
    """
    if n < 1:
        raise AssertionError("n must be at least one")
    iterator = iter(iterable)
    while batch := tuple(itertools.islice(iterator, n)):
        yield batch


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
        exclude_paths: Optional[List[str]] = None,
        sequential: Optional[bool] = False,
    ) -> None:
        self._paths: Optional[List[str]] = paths
        self.exclude_paths: List[str] = exclude_paths or []
        self.root = root
        self.annotation_specifications: List[DecoratorAnnotationSpecification] = (
            annotation_specifications
        )
        self.sequential = sequential

    @property
    def paths(self) -> List[str]:
        paths = self._paths
        if paths is None:
            LOG.info("Collecting all python files")
            paths = list(find_all_paths(self.root))
            LOG.info(f"Found {len(paths)} python files")
            self._paths = paths
        exclude_paths: List[re.Pattern[str]] = [
            re.compile(path) for path in self.exclude_paths
        ]
        return [
            path
            for path in paths
            if all(not exclude.search(path) for exclude in exclude_paths)
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

        if self.sequential:
            for path in self.paths:
                annotated_functions.update(self._annotate_functions(path))
        else:
            number_processes = os.cpu_count()
            LOG.info(f"Using {number_processes} workers")
            with multiprocessing.Pool(processes=number_processes, maxtasksperchild=1) as pool:
                for functions in pool.imap_unordered(
                    _invoke_AnnotatedFunctionGenerator_annotate_function,
                    [(self, batch) for batch in _batched(self.paths, 1000)],
                    chunksize=1,
                ):
                    annotated_functions.update(functions)

        return sorted(annotated_functions)


# This needs to be in the top level for multiprocessing.
def _invoke_AnnotatedFunctionGenerator_annotate_function(
    arguments: Tuple[AnnotatedFunctionGenerator, Tuple[str, ...]]
) -> Set[FunctionDefinitionModel]:
    generator = arguments[0]
    paths = arguments[1]
    annotated_functions = set()
    for path in paths:
        annotated_functions.update(generator._annotate_functions(path))
    return annotated_functions
