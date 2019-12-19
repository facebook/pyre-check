# pyre-unsafe

from typing import Any, Optional

from django.http import HttpRequest, HttpResponse
from django.http.response import HttpResponseBase

def get_token(request: HttpRequest) -> Optional[str]: ...
def rotate_token(request: HttpRequest) -> None: ...

class CsrfViewMiddleware:
    def process_view(
        self,
        request: HttpRequest,
        callback: Any,
        callback_args: Any,
        callback_kwargs: Any,
    ) -> Optional[HttpResponse]: ...
    def process_response(
        self, request: HttpRequest, response: HttpResponseBase
    ) -> HttpResponseBase: ...
