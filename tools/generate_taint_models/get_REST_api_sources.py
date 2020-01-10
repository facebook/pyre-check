# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from typing import List, Optional

from .function_tainter import FunctionTainter
from .model_generator import Registry
from .view_generator import ViewGenerator


class RESTApiSourceGenerator(FunctionTainter, ViewGenerator):
    def __init__(
        self,
        whitelisted_classes: Optional[List[str]] = None,
        taint_annotation: str = "TaintSource[UserControlled]",
    ) -> None:
        FunctionTainter.__init__(
            self,
            arg=taint_annotation,
            kwarg=taint_annotation,
            vararg=taint_annotation,
            whitelisted_classes=whitelisted_classes,
        )
        ViewGenerator.__init__(self)


Registry.register(
    "get_REST_api_sources", RESTApiSourceGenerator, include_by_default=True
)
