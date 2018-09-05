from typing import Any, Optional

from django.http import HttpRequest


REDIRECT_FIELD_NAME: str = ...
SESSION_KEY: str = ...
BACKEND_SESSION_KEY: str = ...


def login(request: HttpRequest, user: Any) -> None:
    ...


def logout(request: HttpRequest) -> None:
    ...


# Patched from https://fburl.com/kwu5z823
# The return annotation is actually NodeUser.
async def async_authenticate(**credentials) -> typing.Any:
    ...


def authenticate(**credentials) -> Typing.Any:
    ...
