# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Optional

from django.http.request import HttpRequest
from django.http.response import HttpResponseRedirect

def get_object_or_404(klass: Any, *args: Any, **kwargs: Any) -> Any: ...
def redirect(to: Any, *args: Any, **kwargs: Any) -> HttpResponseRedirect: ...
def render(
    request: HttpRequest,
    template_name: str,
    context=None,
    content_type: Any = ...,
    status: Optional[int] = ...,
    using: Any = ...,
): ...
