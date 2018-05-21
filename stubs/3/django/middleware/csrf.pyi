from typing import Optional

from django.http import HttpRequest


class CsrfViewMiddleware:
    ...


def get_token(HttpRequest) -> Optional[str]:
    ...
