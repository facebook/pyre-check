# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from abc import ABC
from importlib import import_module
from typing import Any, Callable, Iterable, Optional, Type

from .model_generator import Configuration, ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)

# pyre-ignore: We do dynamic magic here that Pyre doesn't handle well.
DynamicURLType = Type[Any]


class ViewGenerator(ModelGenerator, ABC):
    def __init__(
        self,
        urls_module: Optional[str] = None,
        url_resolver_type: Optional[DynamicURLType] = None,
        url_pattern_type: Optional[DynamicURLType] = None,
    ) -> None:
        super().__init__()
        self.urls_module: Optional[str] = urls_module or Configuration.urls_module
        self.url_resolver_type: DynamicURLType = (
            url_resolver_type or Configuration.url_resolver_type
        )
        self.url_pattern_type: DynamicURLType = (
            url_pattern_type or Configuration.url_pattern_type
        )

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        urls_module = self.urls_module
        if urls_module is None:
            LOG.warning(f"No url module supplied, can't generate view models.")
            return []

        LOG.info(f"Getting all URLs from `{urls_module}`")
        urls_module = import_module(urls_module)
        functions_to_model = []

        # pyre-ignore: Too dynamic.
        def visit_all_patterns(url_patterns: Iterable[Any]) -> None:
            for pattern in url_patterns:
                if isinstance(pattern, self.url_resolver_type):
                    # TODO(T47152686): Fix the pyre bug that causes us to miss the
                    # nested function.
                    visit_all_patterns(pattern.url_patterns)
                elif isinstance(pattern, self.url_pattern_type):
                    functions_to_model.append(pattern.callback)
                else:
                    raise TypeError("pattern is not url resolver or url pattern.")

        # pyre-ignore: Too dynamic.
        visit_all_patterns(urls_module.urlpatterns)
        return functions_to_model
