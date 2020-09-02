# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Dict, Optional

class SessionBase:
    session_key: Optional[str]
    _session_key: Optional[str]
    def __getitem__(self, key: str) -> Any: ...
    def __setitem__(self, key: str, value: Any) -> None: ...
    def get(self, key: str, default: Any = ...) -> Any: ...
    def update(self, dict_: Dict) -> None: ...
    def clear(self) -> None: ...
