# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from typing import Dict, Iterable, List, Optional, Set, Type, TypeVar

from ...api import query
from ...api.connection import PyreConnection
from ...api.query import ClassHierarchy


LOG: logging.Logger = logging.getLogger(__name__)


def _flatten_subclass_tree(target: str, class_hierarchy: ClassHierarchy) -> Set[str]:
    subclasses = class_hierarchy.subclasses(target) or []
    flattened_subclasses = set()
    for subclass in subclasses:
        subsubclasses = _flatten_subclass_tree(subclass, class_hierarchy)
        flattened_subclasses = flattened_subclasses | {subclass} | subsubclasses
    return flattened_subclasses


def get_all_subclasses_from_pyre(
    targets: Iterable[str],
    pyre_connection: PyreConnection,
    transitive: bool = False,
    pyre_cache: Optional[query.PyreCache] = None,
) -> Optional[Dict[str, List[str]]]:
    class_hierarchy = query.get_cached_class_hierarchy(
        pyre_connection=pyre_connection, pyre_cache=pyre_cache
    )
    if class_hierarchy is not None:
        LOG.debug(f"For {targets}, found class hierarchy: {class_hierarchy.hierarchy}")
        result: Dict[str, List[str]] = {}
        for base_class in targets:
            if transitive:
                subclasses = sorted(_flatten_subclass_tree(base_class, class_hierarchy))
            else:
                subclasses = class_hierarchy.subclasses(base_class)

            if subclasses:
                result[base_class] = subclasses
        return result
    else:
        LOG.debug(f"Did not find class hierarchy for {targets}")
        return None


def get_all_subclass_defines_from_pyre(
    targets: Iterable[str],
    pyre_connection: PyreConnection,
    transitive: bool = False,
    pyre_cache: Optional[query.PyreCache] = None,
) -> Optional[Dict[str, List[query.Define]]]:
    subclasses = get_all_subclasses_from_pyre(
        targets,
        pyre_connection=pyre_connection,
        transitive=transitive,
        pyre_cache=pyre_cache,
    )

    if subclasses is not None:
        return {
            target: query.defines(pyre_connection, subclasses[target], batch_size=500)
            for target in subclasses.keys()
        }
    else:
        return None


T = TypeVar("T")


def get_all_subclasses_from_environment(parent_class: Type[T]) -> Iterable[Type[T]]:
    return set(parent_class.__subclasses__()).union(
        [
            grandchild
            for child in parent_class.__subclasses__()
            for grandchild in get_all_subclasses_from_environment(child)
        ]
    )
