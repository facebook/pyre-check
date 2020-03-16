# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import NamedTuple, Optional, Set


class DecoratorAnnotationSpecification(NamedTuple):
    decorator: str
    arg_annotation: Optional[str] = None
    vararg_annotation: Optional[str] = None
    kwarg_annotation: Optional[str] = None
    return_annotation: Optional[str] = None
    parameter_type_whitelist: Optional[Set[str]] = None
    parameter_name_whitelist: Optional[Set[str]] = None
