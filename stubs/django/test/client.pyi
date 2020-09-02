# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict, Optional

from django.http.response import HttpResponse

class Client:
    def __init__(
        self,
        enforce_csrf_checks: bool = ...,
        raise_request_exception: bool = ...,
        **defaults: Any
    ) -> None: ...
    def request(self, **request: Any) -> HttpResponse: ...
    def get(
        self,
        path: str,
        data: Optional[Dict[str, Any]] = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def post(
        self,
        path: str,
        data: Optional[Dict[str, Any]] = ...,
        content_type: str = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def head(
        self,
        path: str,
        data: Optional[Dict[str, Any]] = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def options(
        self,
        path: str,
        data: str = ...,
        content_type: str = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def put(
        self,
        path: str,
        data: str = ...,
        content_type: str = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def patch(
        self,
        path: str,
        data: str = ...,
        content_type: str = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def delete(
        self,
        path: str,
        data: str = ...,
        content_type: str = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...
    def trace(
        self,
        path: str,
        data: str = ...,
        content_type: str = ...,
        follow: bool = ...,
        secure: bool = ...,
        **extra: Any
    ) -> HttpResponse: ...

class RequestFactory: ...
