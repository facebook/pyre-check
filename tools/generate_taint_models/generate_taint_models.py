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

from .gather_views import get_all_views
from .get_exit_nodes import ExitNodeGenerator
from .get_globals import GlobalModelGenerator
from .get_REST_api_sources import RESTApiSourceGenerator
from .model_generator import load_module, qualifier
from .taint_annotator import Model, annotate_function


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


def _get_exit_nodes(arguments: argparse.Namespace) -> Set[str]:
    if not arguments.urls_path:
        LOG.warn("Ran in get_exit_nodes mode, but didn't supply urls_path")
        return set()

    urls = urls_module(arguments.urls_path)
    if urls is None:
        LOG.warn("Need to supply a `.py` file for the urls_path.")
        return set()

    views = get_all_views(
        module_name=urls,
        # pyre-ignore[16]: Django 1.9
        url_pattern_type=RegexURLPattern,
        # pyre-ignore[16]: Django 1.9
        url_resolver_type=RegexURLResolver,
    )
    models = ExitNodeGenerator(
        whitelisted_views=_WHITELISTED_DJANGO_VIEWS
    ).compute_models(views)
    return set(models)


def _get_REST_api_sources(arguments: argparse.Namespace) -> Set[str]:
    if not arguments.urls_path:
        LOG.warn("Ran in get_REST_api_sources mode, but didn't supply urls_path")
        return set()

    urls = urls_module(arguments.urls_path)
    if urls is None:
        LOG.warn("Need to supply a `.py` file for the urls_path.")
        return set()

    whitelist = arguments.whitelisted_class
    views = get_all_views(
        module_name=urls,
        # pyre-ignore[16]: Django 1.9
        url_pattern_type=RegexURLPattern,
        # pyre-ignore[16]: Django 1.9
        url_resolver_type=RegexURLResolver,
    )
    models = RESTApiSourceGenerator(
        whitelisted_classes=whitelist, whitelisted_views=_WHITELISTED_DJANGO_VIEWS
    ).compute_models(views)

    return set(models)


MODES: Mapping[str, Callable[[argparse.Namespace], Set[str]]] = {
    "get_exit_nodes": _get_exit_nodes,
    "get_REST_api_sources": _get_REST_api_sources,
}

if __name__ == "__main__":
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

    parser.add_argument("--mode", action="append", choices=MODES.keys())
    arguments: argparse.Namespace = parser.parse_args()

    if not arguments.mode:
        arguments.mode = ["get_exit_nodes", "get_REST_api_sources"]

    if not arguments.whitelisted_class:
        arguments.whitelisted_class = ["HttpRequest"]

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    os.chdir(os.path.dirname(arguments.urls_path))
    arguments.urls_path = os.path.basename(arguments.urls_path)

    models: Set[str] = set()
    for mode in arguments.mode:
        models = models.union(MODES[mode](arguments))

    print("\n".join(sorted(models)))
