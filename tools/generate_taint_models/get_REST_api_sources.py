# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from .function_tainter import FunctionTainter
from .model_generator import Registry
from .view_generator import ViewGenerator


class RESTApiSourceGenerator(FunctionTainter, ViewGenerator):
    pass


Registry.register(
    "get_REST_api_sources", RESTApiSourceGenerator, include_by_default=True
)
