# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import NamedTuple, Optional, Set


class AnnotationSpecification(NamedTuple):
    arg: Optional[str] = None
    vararg: Optional[str] = None
    kwarg: Optional[str] = None
    returns: Optional[str] = None


class WhitelistSpecification(NamedTuple):
    def __hash__(self) -> int:
        parameter_type = self.parameter_type
        parameter_name = self.parameter_name
        return hash(
            (
                parameter_type and tuple(parameter_type),
                parameter_name and tuple(parameter_name),
            )
        )

    parameter_type: Optional[Set[str]] = None
    parameter_name: Optional[Set[str]] = None


class DecoratorAnnotationSpecification(NamedTuple):
    def __hash__(self) -> int:
        parameter_type_whitelist = self.parameter_type_whitelist
        parameter_name_whitelist = self.parameter_name_whitelist
        return hash(
            (
                self.decorator,
                self.arg_annotation,
                self.vararg_annotation,
                self.kwarg_annotation,
                self.return_annotation,
                parameter_type_whitelist and tuple(sorted(parameter_type_whitelist)),
                parameter_name_whitelist and tuple(sorted(parameter_name_whitelist)),
                self.annotations,
                self.whitelist,
            )
        )

    decorator: str
    arg_annotation: Optional[str] = None
    vararg_annotation: Optional[str] = None
    kwarg_annotation: Optional[str] = None
    return_annotation: Optional[str] = None
    parameter_type_whitelist: Optional[Set[str]] = None
    parameter_name_whitelist: Optional[Set[str]] = None
    annotations: Optional[AnnotationSpecification] = None
    whitelist: Optional[WhitelistSpecification] = None
