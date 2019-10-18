from django.http.cookie import SimpleCookie
from django.http.request import HttpRequest, QueryDict, build_request_repr
from django.http.response import (
    Http404,
    HttpResponse,
    HttpResponseBadRequest,
    HttpResponseForbidden,
    HttpResponseNotAllowed,
    HttpResponseNotFound,
    HttpResponsePermanentRedirect,
    HttpResponseRedirect,
    HttpResponseServerError,
    StreamingHttpResponse,
)
from django.http.utils import conditional_content_removal, fix_location_header
