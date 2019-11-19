# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from dataclasses import dataclass
from typing import Any, List, Optional, Set, Type

from . import get_annotated_free_functions_with_decorator  # noqa
from . import get_class_sources  # noqa
from . import get_exit_nodes  # noqa
from . import get_globals  # noqa
from . import get_graphql_sources  # noqa
from . import get_request_specific_data  # noqa
from . import get_REST_api_sources  # noqa
from .get_annotated_free_functions_with_decorator import DecoratorAnnotationSpec
from .model_generator import Configuration, Registry


@dataclass
class ConfigurationArguments:
    """
    When adding new configuration options, make sure to add a default value for
    them for backwards compatibility. We construct ConfigurationArguments objects
    outside the current directory, and adding a non-optional argument will break those.
    """

    annotation_specs: List[DecoratorAnnotationSpec]
    whitelisted_views: List[str]
    whitelisted_classes: List[str]
    # pyre-ignore[4]: Too dynamic.
    url_resolver_type: Type[Any]
    # pyre-ignore[4]: Too dynamic.
    url_pattern_type: Type[Any]
    root: str
    stub_root: Optional[str]
    # pyre-ignore[4]: Too dynamic.
    graphql_object_type: Type[Any]
    urls_module: Optional[str]
    graphql_module: List[str]
    blacklisted_global_directories: Set[str]
    blacklisted_globals: Set[str]
    logger: Optional[str] = None
    classes_to_taint: Optional[List[str]] = None


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


def run_from_global_state(
    generation_arguments: GenerationArguments,
    configuration_arguments: ConfigurationArguments,
) -> None:
    # Set up all global state :(
    Configuration.whitelisted_views = configuration_arguments.whitelisted_views
    Configuration.whitelisted_classes = configuration_arguments.whitelisted_classes
    Configuration.url_resolver_type = configuration_arguments.url_resolver_type
    Configuration.url_pattern_type = configuration_arguments.url_pattern_type
    Configuration.root = configuration_arguments.root
    Configuration.stub_root = configuration_arguments.stub_root
    Configuration.graphql_object_type = configuration_arguments.graphql_object_type
    Configuration.urls_module = configuration_arguments.urls_module
    Configuration.graphql_module = configuration_arguments.graphql_module
    Configuration.blacklisted_global_directories = (
        configuration_arguments.blacklisted_global_directories
    )
    Configuration.blacklisted_globals = configuration_arguments.blacklisted_globals
    Configuration.logger = configuration_arguments.logger

    Configuration.classes_to_taint = configuration_arguments.classes_to_taint

    modes = generation_arguments.mode or Registry.default_generators

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if generation_arguments.verbose else logging.INFO,
    )
    models = Registry.generate_models(modes)
    output_directory = generation_arguments.output_directory
    if output_directory is not None:
        for name in models:
            # Try to be slightly intelligent in how we name files.
            if name.startswith("get_"):
                filename = f"generated_{name[4:]}"
            else:
                filename = f"generated_{name}"
            with open(f"{output_directory}/{filename}.pysa", "w") as output_file:
                output_file.write("\n".join(sorted(models[name])))
                output_file.write("\n")
    else:
        all_models = set()
        for name in models:
            all_models = all_models.union(models[name])
        print("\n".join(sorted(all_models)))
