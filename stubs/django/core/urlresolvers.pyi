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
    Optional,
    Sequence,
    Tuple,
    Union,
)

from django.http import Http404, HttpResponse
from wsgi import IGWSGIRequest

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
    kwargs: Optional[Dict[str, Any]] = ...,
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
    def resolve_error_handler(
        self, status_code: int
    ) -> Tuple[Callable[[IGWSGIRequest, ...], HttpResponse], Dict]: ...

def get_urlconf(default: Any = None) -> Any: ...
def get_resolver(urlconf: Any) -> RegexURLResolver: ...
def get_callable(view_name: str) -> Callable[..., Awaitable[HttpResponse]]: ...
