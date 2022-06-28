# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import enum
import json
import logging
import os
import sys
import time
import traceback
from dataclasses import dataclass
from itertools import chain
from typing import AbstractSet, Dict, List, Mapping, Optional, Set

from typing_extensions import Final

from ...api.connection import PyreConnection, PyreStartError, Error as PyreQueryError
from ...client import statistics_logger
from .annotated_function_generator import (  # noqa
    AnnotatedFunctionGenerator,
    FunctionDefinition,
    FunctionVisitor,
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


class ExitCode(enum.IntEnum):
    SUCCESS = 0

    # Unexpected internal error
    INTERNAL_ERROR = 1

    # Error that originated from the user's code, not the model generator.
    # For instance, when importing modules or initializing things.
    USER_ERROR = 2

    # Pyre start errors
    PYRE_INTERNAL_ERROR = 3
    BUCK_INTERNAL_ERROR = 4
    BUCK_USER_ERROR = 5
    CONFIGURATION_ERROR = 6
    WATCHMAN_ERROR = 7

    PYRE_QUERY_ERROR = 8


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
    stop_pyre_server: bool = False


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
        "--stop-pyre-server", action="store_true", help="Stop the pyre server once done"
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
        stop_pyre_server=arguments.stop_pyre_server,
    )


def _report_results(
    models: Mapping[str, AbstractSet[Model]], output_directory: Optional[str]
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
        all_models = chain.from_iterable(models.values())
        print("\n".join([str(model) for model in sorted(all_models)]))


def run_from_parsed_arguments(
    generator_options: Dict[str, ModelGenerator[Model]],
    arguments: GenerationArguments,
    default_modes: List[str],
    logger_executable: Optional[str] = None,
    include_default_modes: bool = False,
    pyre_connection: Optional[PyreConnection] = None,
) -> int:
    try:
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
            if mode not in generator_options.keys():
                LOG.warning(f"Unknown mode `{mode}`, skipping.")
                continue
            start = time.time()
            generated_models[mode] = set(generator_options[mode].generate_models())
            elapsed_time_seconds = time.time() - start
            LOG.info(
                f"Computed models for `{mode}` in {elapsed_time_seconds:.3f} seconds."
            )

            if logger_executable is not None:
                elapsed_time_milliseconds = int(elapsed_time_seconds * 1000)
                statistics_logger.log(
                    statistics_logger.LoggerCategory.PERFORMANCE,
                    integers={"time": elapsed_time_milliseconds},
                    normals={
                        "name": "model generation",
                        "model kind": mode,
                        "command_line": " ".join(sys.argv),
                    },
                    logger=logger_executable,
                )

        _report_results(generated_models, arguments.output_directory)

        if pyre_connection is not None and arguments.stop_pyre_server:
            LOG.info("Stopping the pyre server.")
            pyre_connection.stop_server(ignore_errors=True)

        return ExitCode.SUCCESS
    except PyreStartError as error:
        # See client/commands/commands.py
        if error.exit_code == 3:
            LOG.error("Error while starting a pyre server: buck internal error")
            return ExitCode.BUCK_INTERNAL_ERROR
        elif error.exit_code == 7:
            LOG.error("Error while starting a pyre server: buck user error")
            return ExitCode.BUCK_USER_ERROR
        elif error.exit_code == 6:
            LOG.error("Error while starting a pyre server: configuration error")
            return ExitCode.CONFIGURATION_ERROR
        elif error.exit_code == 8:
            LOG.error("Error while starting a pyre server: watchman error")
            return ExitCode.WATCHMAN_ERROR
        else:
            LOG.error(str(error))
            return ExitCode.PYRE_INTERNAL_ERROR
    except PyreQueryError as error:
        LOG.error(str(error))
        traceback.print_exc()
        return ExitCode.PYRE_QUERY_ERROR
    except Exception as error:
        LOG.error(str(error))
        traceback.print_exc()
        return ExitCode.INTERNAL_ERROR


def run_generators(
    generator_options: Dict[str, ModelGenerator[Model]],
    default_modes: List[str],
    verbose: bool = False,
    logger_executable: Optional[str] = None,
    include_default_modes: bool = False,
    pyre_connection: Optional[PyreConnection] = None,
) -> int:
    arguments = _parse_arguments(generator_options)
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if verbose or arguments.verbose else logging.INFO,
        force=True,
    )
    return run_from_parsed_arguments(
        generator_options,
        arguments,
        default_modes,
        logger_executable,
        include_default_modes=include_default_modes,
        pyre_connection=pyre_connection,
    )
