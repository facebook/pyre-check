# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import dataclasses
from typing import Callable, Iterable, List, Optional, Set

from ...api import query
from .generator_specifications import (
    AllParametersAnnotation,
    AnnotationSpecification,
    PerParameterAnnotation,
    WhitelistSpecification,
)

from .inspect_parser import extract_parameters_with_types, extract_qualified_name
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


def taint_callable_dataclass_fields_parameters(
    function_to_model: Callable[..., object],
    parameter_taint: str,
    parameter_kind: str,
    return_annotation: Optional[str] = None,
    allowlist: Optional[WhitelistSpecification] = None,
) -> Iterable[CallableModel]:
    """
    This function will generate a set of callable models for each attribute in dataclasses parameter passed to the function unless they are excluded in the allowlist parameter.
    E. g. if `data` parameter in async_account_deactivation_login is a dataclass with device_id and token attributes these models will be generated:
    def accounts.api.views.async_account_deactivation_login(data: TaintSource[UserControlled[data___device_id], ParameterPath[_.device_id]]) -> TaintSink[ReturnedToUser]: ...
    def accounts.api.views.async_account_deactivation_login(data: TaintSource[UserControlled[data___token], ParameterPath[_.token]]) -> TaintSink[ReturnedToUser]: ...
    def accounts.api.views.async_account_deactivation_login(simple_param: TaintSource[UserControlled], simple_param2: TaintSource[UserControlled]) -> TaintSink[ReturnedToUser]: ...
    """
    parameters_annotations = extract_parameters_with_types(
        function_to_model, strip_optional=True, strip_annotated=True
    )
    function_models = []
    attribute_parameters_annotations = []
    simple_parameters_annotation = {}
    # Compute models for dataclasses parameters
    for parameter_name, parameter_type in parameters_annotations.items():
        if parameter_type is not None and dataclasses.is_dataclass(parameter_type):
            attributes = [field.name for field in dataclasses.fields(parameter_type)]
            for attribute in attributes:
                attribute_parameters_annotations.append(
                    {
                        parameter_name: f"{parameter_taint}[{parameter_kind}, ParameterPath[_.{attribute}]]"
                    }
                )
        else:
            simple_parameters_annotation[
                parameter_name
            ] = f"{parameter_taint}[{parameter_kind}]"

    for parameter_annotation in attribute_parameters_annotations + [
        simple_parameters_annotation
    ]:
        annotation = AnnotationSpecification(
            parameter_annotation=PerParameterAnnotation(parameter_annotation),
            returns=return_annotation,
        )
        model = CallableModel(
            callable_object=function_to_model,
            annotations=annotation,
            whitelist=allowlist,
        )
        function_models.append(model)
    return function_models
