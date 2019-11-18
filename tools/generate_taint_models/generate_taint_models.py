# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import ast
import functools
import glob
import logging
import os
from pathlib import Path
from typing import Callable, Dict, Mapping, Optional, Set, Union

import _ast
from django.core.urlresolvers import RegexURLPattern, RegexURLResolver

from . import ConfigurationArguments, GenerationArguments, run_from_global_state
from .generator_specs import DecoratorAnnotationSpec
from .model_generator import Configuration, Registry


LOG: logging.Logger = logging.getLogger(__name__)


def _file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise ValueError(f"No file at `{path}`")
    return path


def urls_module(urls_path: str) -> Optional[str]:
    urls = os.path.relpath(urls_path, os.getcwd())
    if not urls.endswith(".py"):
        return None
    return urls[:-3]


_WHITELISTED_DJANGO_VIEWS = [
    "django.views.generic.base.TemplateView",
    "django.views.static.serve",
    "django.views.generic.base.RedirectView",
]


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose logging")
    parser.add_argument(
        "urls_path", type=_file_exists, help="Path to django `urls.py` file"
    )
    parser.add_argument(
        "--graphql-path",
        type=_file_exists,
        help="Path to directory containing GraphQL definitions "
        "for which to generate taint models",
    )
    parser.add_argument("--whitelisted-class", action="append")
    parser.add_argument("--as-view-base", action="append")
    parser.add_argument(
        "--stub-root", type=_file_exists, help="Root of the stubs directory"
    )

    parser.add_argument(
        "--output-directory", type=_file_exists, help="Directory to write models to"
    )
    parser.add_argument(
        "--annotation-spec",
        action="append",
        nargs=5,
        metavar=DecoratorAnnotationSpec._fields,
        help="Identify free functions decorated with 'decorator' and generate "
        "models with the arguments and return annotated according to the "
        "provided '*_annotation' arguments",
    )
    parser.add_argument("--mode", action="append", choices=Registry.generators.keys())
    parser.add_argument("--logger", type=_file_exists, help="Path to logger executable")
    arguments: argparse.Namespace = parser.parse_args()

    if not arguments.whitelisted_class:
        # pyre-fixme[16]: `Namespace` has no attribute `whitelisted_class`.
        arguments.whitelisted_class = ["HttpRequest"]

    stub_root = arguments.stub_root
    if stub_root:
        stub_root = os.path.abspath(stub_root)

    annotation_specs = [
        DecoratorAnnotationSpec(**dict(zip(DecoratorAnnotationSpec._fields, spec)))
        for spec in (arguments.annotation_spec or [])
    ]

    run_from_global_state(
        GenerationArguments(
            mode=arguments.mode,
            verbose=arguments.verbose,
            output_directory=arguments.output_directory,
        ),
        ConfigurationArguments(
            annotation_specs=annotation_specs,
            whitelisted_views=_WHITELISTED_DJANGO_VIEWS,
            whitelisted_classes=arguments.whitelisted_class,
            # pyre-ignore[16]: The django stubs are for another version.
            url_resolver_type=RegexURLResolver,
            # pyre-ignore[16]: The django stubs are for another version.
            url_pattern_type=RegexURLPattern,
            classes_to_taint=[],
            root=os.path.dirname(os.path.abspath(arguments.urls_path)),
            stub_root=stub_root,
            graphql_object_type=object,
            urls_module=os.path.basename(arguments.urls_path),
            graphql_module=[],
            blacklisted_global_directories=set(),
            blacklisted_globals=set(),
            logger=arguments.logger,
        ),
    )


if __name__ == "__main__":
    main()
