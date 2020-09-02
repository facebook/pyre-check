# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict

from django.http import HttpRequest

class WSGIRequest(HttpRequest):
    environ: Dict[str, Any] = ...
    path_info: str = ...
    def is_secure(self) -> bool: ...
