# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import dataclasses
import re
from typing import Callable, Iterable, List, Optional, Set

from ...api import query
from .generator_specifications import (
    AllParametersAnnotation,
    AllParametersAnnotationWithParameterNameAsSubKind,
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


def get_specific_parameter_name_annotation(
    function_to_model: Callable[..., object],
    parameter_taint: str,
    parameter_kind: str,
    return_annotation: Optional[str] = None,
) -> AnnotationSpecification:
    parameters_annotations = AllParametersAnnotationWithParameterNameAsSubKind(
        parameter_taint, parameter_kind
    )
    return AnnotationSpecification(
        parameter_annotation=parameters_annotations,
        returns=return_annotation,
    )


def taint_callable_with_parameters_names(
    function_to_model: Callable[..., object],
    parameter_taint: str,
    parameter_kind: str,
    return_annotation: Optional[str] = None,
    allowlist: Optional[WhitelistSpecification] = None,
) -> CallableModel:
    """
    This function will generate a callable model tainting each parameter with a parametric kind annotation using the parameter name unless they are excluded in the allowlist parameter.
    E.g if async_create has a data and a user_id parameter this model will be generated:
    def accounts.api.views.async_create(data: TaintSource[UserControlled[data]], user_id:  TaintSource[UserControlled[user_id]]) -> TaintSink[ReturnedToUser]: ...
    """
    annotations = get_specific_parameter_name_annotation(
        function_to_model, parameter_kind, parameter_taint, return_annotation
    )
    return CallableModel(
        callable_object=function_to_model,
        annotations=annotations,
        whitelist=allowlist,
    )


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
    """
    parameters_annotations = extract_parameters_with_types(
        function_to_model, strip_optional=True, strip_annotated=True
    )
    parameters_dataclasses = {
        parameter_name: parameter_annotation
        for parameter_name, parameter_annotation in parameters_annotations.items()
        if parameter_annotation is not None
        and dataclasses.is_dataclass(parameter_annotation)
    }
    unsafe_characters_regex = re.compile("[^a-zA-Z_0-9]")
    function_models = []
    for parameter_name, parameter_type in parameters_dataclasses.items():
        attributes = [field.name for field in dataclasses.fields(parameter_type)]
        for attribute in attributes:
            sanitized_parameter_name = unsafe_characters_regex.sub("", parameter_name)
            sanitized_attribute = unsafe_characters_regex.sub("", attribute)
            parameters_annotations_dict = {
                parameter_name: f"{parameter_taint}[{parameter_kind}[{sanitized_parameter_name}___{sanitized_attribute}], ParameterPath[_.{attribute}]]"
            }
            annotation = AnnotationSpecification(
                parameter_annotation=PerParameterAnnotation(
                    parameters_annotations_dict
                ),
                returns=return_annotation,
            )
            model = CallableModel(
                callable_object=function_to_model,
                annotations=annotation,
                whitelist=allowlist,
            )
            function_models.append(model)
    return function_models
