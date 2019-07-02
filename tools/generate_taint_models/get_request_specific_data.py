# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Callable, Iterable

from .inspect_parser import extract_view_name
from .model_generator import ModelGenerator
from .taint_annotator import Model


class RequestSpecificDataGenerator(ModelGenerator):
    def __init__(
        self, whitelisted_classes: Iterable[str], whitelisted_views: Iterable[str]
    ) -> None:
        self.whitelisted_classes = whitelisted_classes
        self.whitelisted_views = whitelisted_views

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        view_models = set()

        for view_function in functions_to_model:
            view_name = extract_view_name(view_function)
            if view_name in self.whitelisted_views:
                continue
            taint_kind = "TaintSource[RequestSpecificData]"
            model = Model(arg=taint_kind, vararg=taint_kind, kwarg=taint_kind)
            callable = model.generate(view_function, self.whitelisted_classes)
            if callable is not None:
                view_models.add(callable)

        return sorted(view_models)
