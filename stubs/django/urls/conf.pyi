# pyre-unsafe

from typing import Any, Mapping, Optional, Tuple, Union

from django.urls import URLPattern, URLResolver

def include(arg: Any, namespace: Optional[str] = ...) -> Tuple[Any, str, str]: ...
def path(
    route: str,
    view: Any,
    kwargs: Optional[Mapping[str, Any]] = ...,
    name: Optional[str] = ...,
) -> Union[URLPattern, URLResolver]: ...
