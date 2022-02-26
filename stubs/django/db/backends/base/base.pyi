# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Dict

from django.db.backends.utils import CursorWrapper

class BaseDatabaseWrapper:
    alias: str
    settings_dict: Dict[str, object]
    def __init__(self, settings_dict, alias=...): ...
    def cursor(self) -> CursorWrapper: ...
    def close(self): ...
    def close_if_unusable_or_obsolete(self): ...
    def connection(self): ...
