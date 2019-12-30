# pyre-unsafe

from typing import Any, Optional

from django.http.response import HttpResponseRedirect

def redirect_to_login(
    next: Any, login_url: Optional[Any] = ..., redirect_field_name: str = ...
) -> HttpResponseRedirect: ...
