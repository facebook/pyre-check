# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Callable, Iterable, List, Optional, Set

from ...api import query
from .generator_specifications import (
    AllParametersAnnotation,
    AnnotationSpecification,
    WhitelistSpecification,
)
from .inspect_parser import extract_qualified_name
from .model import CallableModel, PyreFunctionDefinitionModel


def taint_callable_functions(
    functions_to_taint: Iterable[Callable[..., object]],
    taint_annotation: str = "TaintSource[UserControlled]",
    whitelisted_views: Optional[List[str]] = None,
    whitelisted_classes: Optional[List[str]] = None,
    parameter_name_whitelist: Optional[Set[str]] = None,
    annotations: Optional[AnnotationSpecification] = None,
    whitelist: Optional[WhitelistSpecification] = None,
) -> List[CallableModel]:
    whitelisted_views = whitelisted_views or []
    whitelisted_classes = whitelisted_classes or []
    parameter_name_whitelist = parameter_name_whitelist or set()
    entry_points = set()
    for function in functions_to_taint:
        qualified_name = extract_qualified_name(function)
        if qualified_name in whitelisted_views:
            continue
        try:
            annotations = annotations or AnnotationSpecification(
                parameter_annotation=AllParametersAnnotation(
                    arg=taint_annotation,
                    vararg=taint_annotation,
                    kwarg=taint_annotation,
                )
            )
            whitelist = whitelist or WhitelistSpecification(
                parameter_type=set(whitelisted_classes),
                parameter_name=parameter_name_whitelist,
            )
            model = CallableModel(
                callable_object=function, annotations=annotations, whitelist=whitelist
            )
            entry_points.add(model)
        except ValueError:
            pass

    return sorted(entry_points)


def taint_pyre_functions(
    functions_to_taint: Iterable[query.Define],
    annotations: AnnotationSpecification,
    whitelist: Optional[WhitelistSpecification],
) -> List[PyreFunctionDefinitionModel]:
    tainted_functions = []
    for definition in functions_to_taint:
        tainted_functions.append(
            PyreFunctionDefinitionModel(
                definition, annotations=annotations, whitelist=whitelist
            )
        )
    return tainted_functions
