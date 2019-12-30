# pyre-unsafe

from typing import Any, Optional

from django.http.request import HttpRequest
from django.http.response import HttpResponseRedirect

def get_object_or_404(klass: Any, *args: Any, **kwargs: Any) -> Any: ...
def redirect(to: Any, *args: Any, **kwargs: Any) -> HttpResponseRedirect: ...
def render(
    request: HttpRequest,
    template_name: str,
    context=None,
    content_type: Any = ...,
    status: Optional[int] = ...,
    using: Any = ...,
): ...
