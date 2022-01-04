# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Dict, Optional, Union

class BaseCookie(dict):
    def __init__(self, input: Optional[Union[str, Dict[str, str]]] = None) -> None: ...

class SimpleCookie(BaseCookie):
    def __getitem__(self, key: str) -> Any: ...
    def load(self, vals: Dict[Any, Any]) -> None: ...
