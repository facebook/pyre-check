# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from importlib import import_module
from typing import Callable, Iterable, List, Optional, Type, TypeVar

from .subclass_generator import get_all_subclasses_from_environment

LOG: logging.Logger = logging.getLogger(__name__)
T = TypeVar("T")


def gather_all_constructors_in_hierarchy(
    classes_to_taint: List[str],
    filter_classes_by: Optional[Callable[[Type[T]], bool]] = None,
) -> Iterable[Callable[..., object]]:
    LOG.info(f"Getting all init functions from `{classes_to_taint}`")
    all_inits_from_classes = set()
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
        children_classes = get_all_subclasses_from_environment(class_to_taint)

        all_inits_from_classes.update(
            child.__init__
            for child in children_classes
            if (
                child.__init__ != class_to_taint.__init__
                and (filter_classes_by(child) if filter_classes_by else True)
            )
        )
    return list(all_inits_from_classes)
