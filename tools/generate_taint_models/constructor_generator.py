# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from importlib import import_module
from typing import Callable, Iterable, List, Optional, Type

from .model_generator import Configuration, ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


def all_subclasses(parent_class: Type[object]) -> Iterable[Type[object]]:
    return set(parent_class.__subclasses__()).union(
        [s for c in parent_class.__subclasses__() for s in all_subclasses(c)]
    )


class ConstructorGenerator(ModelGenerator):
    def __init__(self, classes_to_taint: Optional[List[str]]) -> None:
        self.classes_to_taint = classes_to_taint

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        classes_to_taint = self.classes_to_taint
        if classes_to_taint is None:
            LOG.warning(
                f"No class to taint supplied, can't generate constructor models."
            )
            return []

        LOG.info(f"Getting all init functions from `{classes_to_taint}`")
        all_inits_from_classes = []
        for class_str_to_taint in classes_to_taint:
            try:
                module_path, class_name = class_str_to_taint.rsplit(".", 1)
            except ValueError:
                LOG.warning(
                    f"Class provided ({class_str_to_taint}) is not a fully qualified"
                    + " reference. Skipping..."
                )
                continue
            class_to_taint = getattr(import_module(module_path), class_name)
            # if the parent class (or any of its parents) passed does not define
            # its __init__ then there is no way to get a callable that resolves
            # to anything but object (which would be useless)
            if isinstance(class_to_taint.__init__, type(object.__init__)):
                LOG.warning(
                    f"Class provided ({class_str_to_taint}) does not define its own "
                    + "__init__ function and thus taint can't be followed. Skipping..."
                )
                continue
            children_classes = all_subclasses(class_to_taint)
            all_inits_from_classes += [
                child.__init__
                for child in children_classes
                if child.__init__ != class_to_taint.__init__
            ]
        return list(set(all_inits_from_classes))
