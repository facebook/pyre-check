# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from abc import ABC, abstractmethod
from typing import Dict, NamedTuple, Optional, Set

from .parameter import Parameter


class ParameterAnnotation(ABC):
    """Parameter annotations can either be a uniform string or a mapping from parameter
    name to annotation."""

    @abstractmethod
    def get(self, parameter: "Parameter") -> Optional[str]:
        pass


class PerParameterAnnotation(ParameterAnnotation):
    def __init__(self, parameter_name_to_taint: Dict[str, str]) -> None:
        self.parameter_name_to_taint = parameter_name_to_taint

    def get(self, parameter: "Parameter") -> Optional[str]:
        return self.parameter_name_to_taint.get(parameter.name)


class AllParametersAnnotation(ParameterAnnotation):
    def __init__(
        self,
        arg: Optional[str] = None,
        vararg: Optional[str] = None,
        kwarg: Optional[str] = None,
    ) -> None:
        self.arg = arg
        self.kwarg = kwarg
        self.vararg = vararg

    def get(self, parameter: "Parameter") -> Optional[str]:
        if parameter.kind == Parameter.Kind.ARG:
            return self.arg
        elif parameter.kind == Parameter.Kind.VARARG:
            return self.vararg
        else:  # KWARG
            return self.kwarg


class AnnotationSpecification(NamedTuple):
    parameter_annotation: Optional[ParameterAnnotation] = None
    returns: Optional[str] = None


class WhitelistSpecification(NamedTuple):
    def __hash__(self) -> int:
        parameter_type = self.parameter_type
        parameter_name = self.parameter_name
        return hash(
            (
                parameter_type and tuple(sorted(parameter_type)),
                parameter_name and tuple(sorted(parameter_name)),
            )
        )

    parameter_type: Optional[Set[str]] = None
    parameter_name: Optional[Set[str]] = None


class DecoratorAnnotationSpecification(NamedTuple):
    def __hash__(self) -> int:
        return hash((self.decorator, self.annotations, self.whitelist))

    decorator: str
    annotations: Optional[AnnotationSpecification] = None
    whitelist: Optional[WhitelistSpecification] = None


default_entrypoint_taint = AnnotationSpecification(
    parameter_annotation=AllParametersAnnotation(
        arg="TaintSource[UserControlled]",
        vararg="TaintSource[UserControlled]",
        kwarg="TaintSource[UserControlled]",
    ),
    returns="TaintSink[ReturnedToUser]",
)
