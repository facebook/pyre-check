# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any
from .engine import Engine

def __getattr__(name: str) -> Any: ...

class Loader:
    def __init__(self, engine: Engine) -> None: ...
