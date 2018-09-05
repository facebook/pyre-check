from typing import List

from django.http import HttpResponse


def patch_vary_headers(response: HttpResponse, newheaders: List[str]) -> None:
    ...
