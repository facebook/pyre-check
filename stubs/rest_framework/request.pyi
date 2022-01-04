# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe
from django.http import HttpRequest

# here we are telling a lie because the implemenetation is doing something like inheritance but not really inheritance
# we still want people to see things like request.GET.get('username') even if that uses the underlying request and not the wrapper.
# see https://github.com/encode/django-rest-framework/blob/acbd9d8222e763c7f9c7dc2de23c430c702e06d4/rest_framework/request.py#L410 why
class Request(HttpRequests):
    @property
    def data(self): ...
    @property
    def query_params(self): ...
    @property
    def content_type(self): ...
    @property
    def stream(self): ...
    @property
    def QUERY_PARAMS(self): ...
    @property
    def FILES(self): ...
    @property
    def DATA(self): ...
    @property
    def POST(self): ...
