# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from importlib import import_module
from typing import Any, Callable, Iterable, List, Type


def get_all_views(
    *,
    module_name: str,
    # pyre-ignore[2]: Too dynamic for pyre :(
    url_pattern_type: Type[Any],
    # pyre-ignore[2]: Too dynamic for pyre :(
    url_resolver_type: Type[Any]
) -> List[Callable[..., object]]:
    urls_module = import_module(module_name)
    functions_to_model = []

    # pyre-ignore: Too dynamic.
    def visit_all_patterns(url_patterns: Iterable[Any]) -> None:
        for pattern in url_patterns:
            if isinstance(pattern, url_resolver_type):
                visit_all_patterns(pattern.url_patterns)
            elif isinstance(pattern, url_pattern_type):
                functions_to_model.append(pattern.callback)
            else:
                raise TypeError("pattern is not url resolver or url pattern.")

    # pyre-ignore: Too dynamic.
    visit_all_patterns(urls_module.urlpatterns)
    return functions_to_model
