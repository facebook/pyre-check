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
    # pyre-ignore: Typeshed is for django 2.x
    Configuration.url_pattern_type = RegexURLPattern
    # pyre-ignore: Typeshed is for django 2.x
    Configuration.url_resolver_type = RegexURLResolver

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
    parser.add_argument("--mode", action="append", choices=Registry.generators.keys())
    arguments: argparse.Namespace = parser.parse_args()

    if not arguments.whitelisted_class:
        # pyre-fixme[16]: `Namespace` has no attribute `whitelisted_class`.
        arguments.whitelisted_class = ["HttpRequest"]

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    Configuration.root = os.path.dirname(os.path.abspath(arguments.urls_path))
    Configuration.urls_module = os.path.basename(arguments.urls_path)
    Configuration.whitelisted_classes = arguments.whitelisted_class
    Configuration.whitelisted_views = _WHITELISTED_DJANGO_VIEWS
    stub_root = arguments.stub_root
    if stub_root:
        Configuration.stub_root = os.path.abspath(stub_root)

    modes = arguments.mode or Registry.default_generators
    models = Registry.generate_models(modes)
    output_directory = arguments.output_directory
    if output_directory is not None:
        for name in models:
            with open(f"{output_directory}/generated_{name}.pysa", "w") as output_file:
                output_file.write("\n".join(sorted(models[name])))
                output_file.write("\n")
    else:
        all_models = set()
        for name in models:
            all_models = all_models.union(models[name])
        print("\n".join(sorted(all_models)))


if __name__ == "__main__":
    main()
