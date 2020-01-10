# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .inspect_parser import extract_qualified_name
from .model import CallableModel, Model
from .model_generator import Configuration, Registry
from .view_generator import DynamicURLType, ViewGenerator


class ExitNodeGenerator(ViewGenerator):
    def __init__(
        self,
        urls_module: Optional[str] = None,
        url_resolver_type: Optional[DynamicURLType] = None,
        url_pattern_type: Optional[DynamicURLType] = None,
        whitelisted_views: Optional[List[str]] = None,
    ) -> None:
        super().__init__(
            urls_module=urls_module,
            url_resolver_type=url_resolver_type,
            url_pattern_type=url_pattern_type,
        )
        self.whitelisted_views: List[
            str
        ] = whitelisted_views or Configuration.whitelisted_views

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        exit_nodes = set()

        for view_function in functions_to_model:
            qualified_name = extract_qualified_name(view_function)
            if qualified_name in self.whitelisted_views:
                continue
            try:
                model = CallableModel(
                    returns="TaintSink[ReturnedToUser]", callable_object=view_function
                )
                exit_nodes.add(model)
            except ValueError:
                pass

        return sorted(exit_nodes)


Registry.register("get_exit_nodes", ExitNodeGenerator, include_by_default=True)
