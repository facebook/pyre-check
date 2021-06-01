# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import json
import logging
import os
import sys
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set, Type

from typing_extensions import Final

from ...api.connection import PyreConnection
from ...client import statistics
from .annotated_function_generator import (  # noqa
    AnnotatedFunctionGenerator,
    FunctionVisitor,
    FunctionDefinition,
)
from .generator_specifications import DecoratorAnnotationSpecification  # noqa
from .get_annotated_free_functions_with_decorator import (  # noqa
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .get_class_sources import ClassSourceGenerator  # noqa
from .get_constructor_initialized_attribute_sources import (  # noqa
    ConstructorInitializedAttributeSourceGenerator,
)
from .get_django_class_based_view_models import DjangoClassBasedViewModels  # noqa
from .get_dynamic_graphql_sources import DynamicGraphQLSourceGenerator  # noqa
from .get_exit_nodes import ExitNodeGenerator  # noqa
from .get_filtered_sources import FilteredSourceGenerator  # noqa
from .get_globals import GlobalModelGenerator  # noqa
from .get_graphene_models import GrapheneModelsGenerator  # noqa
from .get_graphql_sources import GraphQLSourceGenerator  # noqa
from .get_methods_of_subclasses import MethodsOfSubclassesGenerator  # noqa
from .get_models_filtered_by_callable import ModelsFilteredByCallableGenerator  # noqa
from .get_request_specific_data import RequestSpecificDataGenerator  # noqa
from .get_REST_api_sources import RESTApiSourceGenerator  # noqa
from .get_undecorated_sources import UndecoratedSourceGenerator  # noqa
from .model import Model
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


@dataclass
class GenerationArguments:
    """
    When adding new generation options, make sure to add a default value for
    them for backwards compatibility. We construct GenerationArguments objects
    outside the current directory, and adding a non-optional argument will break those.
    """

    mode: Final[Optional[List[str]]]
    verbose: bool
    output_directory: Final[Optional[str]]

    # Pyre arguments.
    no_saved_state: bool = False
    isolation_prefix: Optional[str] = None


def _file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise ValueError("No file at `{path}`")
    return path


def _parse_arguments(
    generator_options: Dict[str, ModelGenerator[Model]]
) -> GenerationArguments:
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose logging")
    parser.add_argument("--mode", action="append", choices=generator_options.keys())
    parser.add_argument(
        "--no-saved-state", action="store_true", help="Disable pyre saved state"
    )
    parser.add_argument(
        "--isolation-prefix", type=str, help="Buck isolation prefix when running pyre"
    )
    parser.add_argument(
        "--output-directory", type=_file_exists, help="Directory to write models to"
    )
    arguments: argparse.Namespace = parser.parse_args()
    return GenerationArguments(
        mode=arguments.mode,
        verbose=arguments.verbose,
        output_directory=arguments.output_directory,
        no_saved_state=arguments.no_saved_state,
        isolation_prefix=arguments.isolation_prefix,
    )


def _report_results(
    models: Dict[str, Set[Model]], output_directory: Optional[str]
) -> None:
    if output_directory is not None:
        for name in models:
            # Try to be slightly intelligent in how we name files.
            if name.startswith("get_"):
                filename = f"generated_{name[4:]}"
            else:
                filename = f"generated_{name}"
            with open(f"{output_directory}/{filename}.pysa", "w") as output_file:
                output_file.write(
                    "\n".join([str(model) for model in sorted(models[name])])
                )
                output_file.write("\n")
        print(
            json.dumps(
                {
                    "number of generated models": sum(
                        (len(generated_models) for generated_models in models.values())
                    )
                }
            )
        )
    else:
        all_models = set()
        for name in models:
            all_models = all_models.union(models[name])
        print("\n".join([str(model) for model in sorted(all_models)]))


def run_from_parsed_arguments(
    generator_options: Dict[str, ModelGenerator[Model]],
    arguments: GenerationArguments,
    default_modes: List[str],
    logger_executable: Optional[str] = None,
    include_default_modes: bool = False,
    pyre_connection: Optional[PyreConnection] = None,
) -> None:
    argument_modes = arguments.mode or []
    if len(argument_modes) == 0 or include_default_modes:
        modes = list(set(argument_modes + default_modes))
    else:
        modes = argument_modes

    if pyre_connection is not None and arguments.no_saved_state:
        pyre_connection.add_arguments("--no-saved-state")
    isolation_prefix = arguments.isolation_prefix
    if pyre_connection is not None and isolation_prefix is not None:
        pyre_connection.add_arguments("--isolation-prefix", isolation_prefix)

    generated_models: Dict[str, Set[Model]] = {}
    for mode in modes:
        LOG.info("Computing models for `%s`", mode)
        start = time.time()
        if mode in generator_options.keys():
            generated_models[mode] = set(generator_options[mode].generate_models())
        else:
            continue
        elapsed_time_seconds = time.time() - start
        LOG.info(f"Computed models for `{mode}` in {elapsed_time_seconds:.3f} seconds.")

        if logger_executable is not None:
            elapsed_time_milliseconds = int(elapsed_time_seconds * 1000)
            statistics.log(
                statistics.LoggerCategory.PERFORMANCE,
                integers={"time": elapsed_time_milliseconds},
                normals={
                    "name": "model generation",
                    "model kind": mode,
                    "command_line": " ".join(sys.argv),
                },
                logger=logger_executable,
            )
    _report_results(generated_models, arguments.output_directory)


def run_generators(
    generator_options: Dict[str, ModelGenerator[Model]],
    default_modes: List[str],
    verbose: bool = False,
    logger_executable: Optional[str] = None,
    include_default_modes: bool = False,
    pyre_connection: Optional[PyreConnection] = None,
) -> None:
    arguments = _parse_arguments(generator_options)
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if verbose or arguments.verbose else logging.INFO,
    )
    run_from_parsed_arguments(
        generator_options,
        arguments,
        default_modes,
        logger_executable,
        include_default_modes=include_default_modes,
        pyre_connection=pyre_connection,
    )
