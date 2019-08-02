# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import NamedTuple, Optional


class DecoratorAnnotationSpec(NamedTuple):
    decorator: str
    arg_annotation: Optional[str] = None
    vararg_annotation: Optional[str] = None
    kwarg_annotation: Optional[str] = None
    return_annotation: Optional[str] = None
