# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from django.http.cookie import SimpleCookie as SimpleCookie
from django.http.request import (
    build_request_repr as build_request_repr,
    HttpRequest as HttpRequest,
    QueryDict as QueryDict,
)
from django.http.response import (
    Http404 as Http404,
    HttpResponse as HttpResponse,
    HttpResponseBadRequest as HttpResponseBadRequest,
    HttpResponseForbidden as HttpResponseForbidden,
    HttpResponseNotAllowed as HttpResponseNotAllowed,
    HttpResponseNotFound as HttpResponseNotFound,
    HttpResponsePermanentRedirect as HttpResponsePermanentRedirect,
    HttpResponseRedirect as HttpResponseRedirect,
    HttpResponseServerError as HttpResponseServerError,
    StreamingHttpResponse as StreamingHttpResponse,
)
from django.http.utils import (
    conditional_content_removal as conditional_content_removal,
    fix_location_header as fix_location_header,
)
