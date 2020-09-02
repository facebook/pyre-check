# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any

class LazySettings:
    LOGIN_URL: str
    def __getattr__(self, name: str) -> Any: ...

class BaseSettings:
    def __setattr__(self, name: str, value: Any): ...

class Settings(BaseSettings): ...

settings: LazySettings = ...

urls: Any = ...
