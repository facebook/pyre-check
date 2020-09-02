# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from re import Pattern
from types import ModuleType
from typing import (
    Any,
    Awaitable,
    Callable,
    Dict,
    Iterable,
    List,
    Mapping,
    Optional,
    Sequence,
    Tuple,
    Union,
)

import re2
from django.http import Http404, HttpRequest, HttpResponse

class Resolver404(Http404): ...

class ResolverMatch:
    kwargs: Dict[str, Any] = ...
    view_name: str = ...
    func: Any = ...
    args: Any = ...
    url_name: str = ...
    app_name: str = ...
    _func_path: str = ...
    view_path: str = ...
    def __getitem__(self, index: int) -> Any: ...

class NoReverseMatch(Exception): ...

def reverse(
    viewname: Any,
    urlconf: Any = ...,
    args: Optional[Sequence[Any]] = ...,
    kwargs: Optional[Mapping[str, Any]] = ...,
    prefix: Any = ...,
    current_app: Any = ...,
) -> str: ...
def resolve(path: str, urlconf: Any = ...) -> ResolverMatch: ...

class RegexURLPattern:
    regex: Pattern
    callback: Callable[..., Any]
    name: Optional[str]

class RegexURLResolver:
    url_patterns: Iterable[Union[RegexURLPattern, "RegexURLResolver"]]
    urlconf_module: Any
    urlconf_name: Union[str, ModuleType]
    regex: Pattern
    def resolve(self, path: str) -> ResolverMatch: ...
    def resolve_error_handler(
        self, status_code: int
    ) -> Tuple[Callable[[HttpRequest, ...], HttpResponse], Dict]: ...
    # this is not in Django but we add it on
    _re_set: Union[bool, Optional[re2.Set]]

def get_urlconf(default: Any = None) -> Any: ...
def get_resolver(urlconf: Any) -> RegexURLResolver: ...
def get_callable(view_name: str) -> Callable[..., Awaitable[HttpResponse]]: ...
