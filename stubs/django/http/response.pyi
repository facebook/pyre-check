from typing import (
    Any,
    Dict,
    Iterator,
    Optional,
)


class HttpResponse(Iterator[bytes]):
    _headers: Dict[str, str]
    _slipstream_error_name: Any = ...
    content: Any = ...
    cookies: Any = ...
    status_code: Any = ...
    streaming_content: Any = ...
    streaming: Any = ...

    def get(self, header: str, alternate: Any = ...): ...

    def __getitem__(self, header: str) -> Any: ...

    def set_cookie(
            self,
            key: str,
            value: str = ...,
            max_age: Any = ...,
            expires: Any = ...,
            path: str = ...,
            domain: Any = ...,
            secure: bool = ...,
            httponly: bool = ...,
    ) -> None:
        ...

    def set_signed_cookie(self, key: str, value: Any, salt: str = ..., **kwargs) -> None:
        ...

    def delete_cookie(self, key: str, path: Optional[str] = ..., domain: Any = ...): ...

    def has_header(self, header: str) -> bool: ...

    def __getitem__(self, header: str) -> Any: ...


class HttpResponseRedirect(HttpResponse):
    url: Any = ...


class HttpResponsePermanentRedirect(HttpResponseRedirect):
    ...


class Http404(Exception):
    ...


class HttpResponseForbidden(HttpResponse):
    ...


class HttpResponseBadRequest(HttpResponse):
    ...


class HttpResponseNotAllowed(HttpResponse):
    ...


class HttpResponseNotFound(HttpResponse):
    ...


class StreamingHttpResponse(HttpResponse):
    ...
