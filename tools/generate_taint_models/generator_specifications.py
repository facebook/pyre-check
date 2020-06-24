# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from abc import ABC, abstractmethod
from enum import Enum, auto
from typing import TYPE_CHECKING, Dict, NamedTuple, Optional, Set


if TYPE_CHECKING:
    from .model import Parameter


class ArgumentKind(Enum):
    ARG = auto()
    VARARG = auto()
    KWARG = auto()


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
        if parameter.kind == ArgumentKind.ARG:
            return self.arg
        elif parameter.kind == ArgumentKind.VARARG:
            return self.vararg
        else:  # KWARG
            return self.kwarg


class AnnotationSpecificationBase(NamedTuple):
    parameter_annotation: Optional[ParameterAnnotation] = None
    returns: Optional[str] = None


class AnnotationSpecification(AnnotationSpecificationBase):
    """This class is inheriting from AnnotationSpecificationBase as NamedTuple's don't
    allow you to override new. The hackery exists for backwards compatibility."""

    def __new__(
        cls,
        arg: Optional[str] = None,
        vararg: Optional[str] = None,
        kwarg: Optional[str] = None,
        parameter_annotation: Optional[ParameterAnnotation] = None,
        returns: Optional[str] = None,
    ) -> "AnnotationSpecification":
        if parameter_annotation is None and (
            arg is not None or vararg is not None or kwarg is not None
        ):
            parameter_annotation = AllParametersAnnotation(
                arg=arg, kwarg=kwarg, vararg=vararg
            )
        # pyre-ignore[7]: Lying to the type checker, as this morally belongs to the base
        #  class, and we'd rather not have the consumers know about
        #  AnnotationSpecificationBase.
        return super().__new__(
            cls, parameter_annotation=parameter_annotation, returns=returns
        )


class WhitelistSpecification(NamedTuple):
    def __hash__(self) -> int:
        parameter_type = self.parameter_type
        parameter_name = self.parameter_name
        return hash(
            (
                # pyre-fixme[6]: Expected `Iterable[Variable[_LT (bound to
                #  _SupportsLessThan)]]` for 1st param but got `Set[str]`.
                parameter_type and tuple(sorted(parameter_type)),
                # pyre-fixme[6]: Expected `Iterable[Variable[_LT (bound to
                #  _SupportsLessThan)]]` for 1st param but got `Set[str]`.
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
