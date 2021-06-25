# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Sequence, Optional

from django.http.request import HttpRequest
from django.http.response import HttpResponseRedirect

import sys
from typing import Any, Mapping, Optional, Union

from django.db.models.base import Model
from django.http.response import (
    HttpResponse as HttpResponse,
    HttpResponseRedirect as HttpResponseRedirect,
    HttpResponsePermanentRedirect as HttpResponsePermanentRedirect,
)

def get_object_or_404(klass: Any, *args: Any, **kwargs: Any) -> Any: ...
def redirect(
    to: Any, *args: Any, permanent: bool, **kwargs: Any
) -> HttpResponseRedirect: ...
def render(
    request: HttpRequest,
    template_name: Union[str, Sequence[str]],
    context: Optional[Mapping[str, Any]] = ...,
    content_type: Optional[str] = ...,
    status: Optional[int] = ...,
    using: Optional[str] = ...,
) -> HttpResponse: ...
def render_to_response(
    template_name: Union[str, Sequence[str]],
    context: Optional[Mapping[str, Any]] = ...,
    content_type: Optional[str] = ...,
    status: Optional[int] = ...,
    using: Optional[str] = ...,
) -> HttpResponse: ...
