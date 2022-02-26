# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .inspect_parser import extract_qualified_name
from .model import CallableModel
from .model_generator import ModelGenerator
from .view_generator import DjangoUrls, get_all_views


class ExitNodeGenerator(ModelGenerator[CallableModel]):
    def __init__(
        self,
        django_urls: DjangoUrls,
        whitelisted_views: Optional[List[str]] = None,
        taint_annotation: str = "TaintSink[ReturnedToUser]",
    ) -> None:
        self.django_urls = django_urls
        self.whitelisted_views: List[str] = whitelisted_views or []
        self.taint_annotation: str = taint_annotation

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return get_all_views(self.django_urls)

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        exit_nodes = set()

        for view_function in functions_to_model:
            qualified_name = extract_qualified_name(view_function)
            if qualified_name in self.whitelisted_views:
                continue
            try:
                model = CallableModel(
                    returns=self.taint_annotation, callable_object=view_function
                )
                exit_nodes.add(model)
            except ValueError:
                pass

        return sorted(exit_nodes)
