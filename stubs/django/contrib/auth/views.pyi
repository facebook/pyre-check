# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Optional

from django.http.response import HttpResponseRedirect

def redirect_to_login(
    next: Any, login_url: Optional[Any] = ..., redirect_field_name: str = ...
) -> HttpResponseRedirect: ...
def __getattr__(name: str) -> Any: ...
