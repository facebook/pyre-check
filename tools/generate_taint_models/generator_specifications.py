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
