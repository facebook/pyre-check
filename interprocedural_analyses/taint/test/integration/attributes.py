# flake8: noqa

from typing import Optional


class Token:
    token: str = ""


class OAuthRequest:
    access_token: Optional[Token] = None


class Request:
    optional: Optional[OAuthRequest] = None
    non_optional: OAuthRequest = OAuthRequest()


def test_via_optional(request: Request):
    return request.optional.access_token.token


def test_via_non_optional(request: Request):
    return request.non_optional.access_token.token


def test_attribute(t: Token):
    return t.token
