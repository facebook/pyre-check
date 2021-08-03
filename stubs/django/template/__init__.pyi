# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Mapping

from .base import Template
from .engine import Engine

class TemplateSyntaxError(Exception):
    pass

class Context:
    def __init__(self, dict_: Mapping[str, Any]) -> None: ...
