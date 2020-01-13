# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import Callable, Iterable, List, Optional

from .inspect_parser import extract_qualified_name
from .model import CallableModel, Model
from .model_generator import Configuration, ModelGenerator, Registry
from .view_generator import DjangoUrls, django_urls_from_configuration, get_all_views


class ExitNodeGenerator(ModelGenerator):
    def __init__(
        self,
        django_urls: Optional[DjangoUrls] = None,
        whitelisted_views: Optional[List[str]] = None,
    ) -> None:
        self.django_urls: Optional[
            DjangoUrls
        ] = django_urls or django_urls_from_configuration()
        self.whitelisted_views: List[
            str
        ] = whitelisted_views or Configuration.whitelisted_views

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        django_urls = self.django_urls
        if django_urls is None:
            return []
        return get_all_views(django_urls)

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
