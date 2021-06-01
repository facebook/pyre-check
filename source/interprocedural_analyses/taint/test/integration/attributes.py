# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source
from typing import Optional, Union, List


class Token:
    token: str = ""


class OAuthRequest:
    access_token: Optional[Token] = None


class Request:
    optional: Optional[OAuthRequest] = None
    non_optional: OAuthRequest = OAuthRequest()


def test_via_optional(request: Request):
    oauth_request = request.optional
    if oauth_request:
        access_token = oauth_request.access_token
        if access_token:
            return access_token.token
    return None


def test_via_non_optional(request: Request):
    access_token = request.non_optional.access_token
    if access_token:
        return access_token.token
    return None


def test_attribute(t: Token):
    return t.token


def test_getattr_forward(t: Token):
    return getattr(t, "token", None)


def test_getattr_default(t: Token):
    return getattr(t, "unrelated", __test_source())


def test_getattr_backwards(t):
    __test_sink(getattr(t, "token", None))


def test_getattr_backwards_default(t):
    __test_sink(getattr(None, "", t.token))


class UseViaDict:
    def __init__(self, a, b):
        self.a = a
        self.b = b


def test_attribute_via_dunder_dict():
    obj = UseViaDict(a=__test_source(), b=None)
    # First two should be flows, and the third shouldn't.
    __test_sink(obj.__dict__)
    __test_sink(obj.__dict__["a"])
    __test_sink(obj.__dict__["b"])


class Untainted:
    token: str = ""


def test_attribute_union_source(t: Union[Token, Untainted]):
    __test_sink(t.token)
    if isinstance(t, Token):
        __test_sink(t.token)
    elif isinstance(t, Untainted):
        __test_sink(t.token)


class Sink:
    token: str = ""


def test_attribute_union_sink(t: Union[Sink, Untainted]):
    t.token = __test_source()
    if isinstance(t, Sink):
        t.token = __test_source()
    elif isinstance(t, Untainted):
        t.token = __test_source()


class C:
    dictionary = {"text": "modelled as tainted", "other": "benign"}


def test_issue_with_text_key_of_dictionary(c: C):
    __test_sink(c.dictionary["text"])


def test_no_issue_with_other_key_of_dictionary(c: C):
    __test_sink(c.dictionary["other"])


class D:
    buffer: List[str] = []


def test_issue_with_update_to_self_attribute(d: D):
    d.buffer.append(__test_source())
