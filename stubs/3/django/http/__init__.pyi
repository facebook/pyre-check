from django.http.request import (
    build_request_repr,
    HttpRequest,
    QueryDict,
)
from django.http.response import (
    Http404,
    HttpResponse,
    HttpResponseRedirect,
    HttpResponsePermanentRedirect,
    HttpResponseNotAllowed,
    HttpResponseNotFound,
    HttpResponseForbidden,
    HttpResponseBadRequest,
    StreamingHttpResponse,
)
