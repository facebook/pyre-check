from typing import Sequence

from django.http.response import HttpResponseBase

def patch_vary_headers(
    response: HttpResponseBase, newheaders: Sequence[str]
) -> None: ...
