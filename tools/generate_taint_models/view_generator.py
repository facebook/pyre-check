# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import logging
from abc import ABC
from importlib import import_module
from typing import Any, Callable, Iterable

from .model_generator import Configuration, ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class ViewGenerator(ModelGenerator, ABC):
    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        LOG.info(f"Getting all URLs from `{Configuration.urls_module}`")
        urls_module = import_module(Configuration.urls_module)
        functions_to_model = []

        # pyre-ignore: Too dynamic.
        def visit_all_patterns(url_patterns: Iterable[Any]) -> None:
            for pattern in url_patterns:
                if isinstance(pattern, Configuration.url_resolver_type):
                    # TODO(T47152686): Fix the pyre bug that causes us to miss the
                    # nested function.
                    # pyre-ignore[18]: Qualification bug.
                    visit_all_patterns(pattern.url_patterns)
                elif isinstance(pattern, Configuration.url_pattern_type):
                    functions_to_model.append(pattern.callback)
                else:
                    raise TypeError("pattern is not url resolver or url pattern.")

        # pyre-ignore: Too dynamic.
        visit_all_patterns(urls_module.urlpatterns)
        return functions_to_model
