# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import logging
import os
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set, Type

from ...client import log_statistics
from .generator_specifications import DecoratorAnnotationSpecification
from .get_annotated_free_functions_with_decorator import (  # noqa
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .get_class_sources import ClassSourceGenerator  # noqa
from .get_exit_nodes import ExitNodeGenerator  # noqa
from .get_filtered_sources import FilteredSourceGenerator  # noqa
from .get_globals import GlobalModelGenerator  # noqa
from .get_graphql_sources import GraphQLSourceGenerator  # noqa
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

    mode: Optional[List[str]]
    verbose: bool
    output_directory: Optional[str]


def _file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise ValueError("No file at `{path}`")
    return path


def _parse_arguments(
    generator_options: Dict[str, ModelGenerator]
) -> GenerationArguments:
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose logging")
    parser.add_argument("--mode", action="append", choices=generator_options.keys())
    parser.add_argument(
        "--output-directory", type=_file_exists, help="Directory to write models to"
    )
    arguments: argparse.Namespace = parser.parse_args()
    return GenerationArguments(
        mode=arguments.mode,
        verbose=arguments.verbose,
        output_directory=arguments.output_directory,
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
    else:
        all_models = set()
        for name in models:
            all_models = all_models.union(models[name])
        print("\n".join([str(model) for model in sorted(all_models)]))


def run_generators(
    generator_options: Dict[str, ModelGenerator],
    default_modes: List[str],
    verbose: bool = False,
    logger_executable: Optional[str] = None,
) -> None:
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if verbose else logging.INFO,
    )

    arguments = _parse_arguments(generator_options)
    modes = arguments.mode or default_modes
    generated_models: Dict[str, Set[Model]] = {}
    for mode in modes:
        LOG.info("Computing models for `%s`", mode)
        start = time.time()

        generated_models[mode] = set(generator_options[mode].generate_models())

        elapsed_time_seconds = time.time() - start
        LOG.info(f"Computed models for `{mode}` in {elapsed_time_seconds:.3f} seconds.")

        if logger_executable is not None:
            elapsed_time_milliseconds = int(elapsed_time_seconds * 1000)
            log_statistics(
                "perfpipe_pyre_performance",
                integers={"time": elapsed_time_milliseconds},
                normals={"name": "model generation", "model kind": mode},
                logger=logger_executable,
            )
    _report_results(generated_models, arguments.output_directory)
