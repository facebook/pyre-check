# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import functools
import logging
import os
from abc import ABC, abstractmethod
from typing import Any, Callable, ClassVar, Dict, Iterable, List, Optional, Set, Type

from .get_annotated_free_functions_with_decorator import DecoratorAnnotationSpec


LOG: logging.Logger = logging.getLogger(__name__)


def qualifier(root: str, path: str) -> str:
    path = os.path.relpath(path, root)
    if path.endswith(".pyi"):
        path = path[:-4]
    elif path.endswith(".py"):
        path = path[:-3]
    qualifier = path.replace("/", ".")
    if qualifier.endswith(".__init__"):
        qualifier = qualifier[:-9]
    return qualifier


class ModelGenerator(ABC):
    @abstractmethod
    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        pass

    @abstractmethod
    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        pass

    def generate_models(self) -> Set[str]:
        return set(self.compute_models(self.gather_functions_to_model()))


class Configuration:
    # Arguments that can be supplied through argparse and callers - these are globals to
    # allow registering generators at the definition point and not having to repeat the
    # arguments for similar generators.
    urls_module: ClassVar[str] = ""
    # pyre-ignore[4]: Too dynamic.
    url_pattern_type: ClassVar[Type[Any]] = object
    # pyre-ignore[4]: Too dynamic.
    url_resolver_type: ClassVar[Type[Any]] = object
    # pyre-ignore[4]: Too dynamic.
    graphql_object_type: ClassVar[Type[Any]] = object
    whitelisted_classes: ClassVar[List[str]] = []
    whitelisted_views: ClassVar[List[str]] = []
    root: ClassVar[str] = ""
    stub_root: ClassVar[Optional[str]] = ""
    graphql_module: ClassVar[str] = ""
    blacklisted_globals: ClassVar[Set[str]] = set()
    blacklisted_global_directories: ClassVar[Set[str]] = set()
    annotation_specs: ClassVar[List[DecoratorAnnotationSpec]] = []


class Registry:
    # Dynamically registered generators.
    generators: ClassVar[Dict[str, Type[ModelGenerator]]] = {}
    default_generators: List[str] = []

    @classmethod
    def register(
        cls, name: str, generator: Type[ModelGenerator], include_by_default: bool
    ) -> None:
        cls.generators[name] = generator
        if include_by_default:
            cls.default_generators.append(name)

    @classmethod
    def generate_models(cls, generator_names: Iterable[str]) -> Dict[str, Set[str]]:
        models = {}
        for name in generator_names:
            LOG.info("Computing models for `%s`", name)
            generator = cls.generators[name]()
            models[name] = generator.generate_models()
        return models
