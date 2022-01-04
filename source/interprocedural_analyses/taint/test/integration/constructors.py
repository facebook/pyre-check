# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Any, Dict


class SomeAPI:
    HOST = "api.some.com/1.1"
    AUTHENTICATE_URL = f"https://{HOST}/some.json"

    def __init__(self, oauth_token: str, oauth_token_secret: str) -> None:
        self.oauth_token = oauth_token
        self.oauth_token_secret = oauth_token_secret

    @classmethod
    def from_default_keys(cls, oauth_token: str, oauth_token_secret: str) -> "SomeAPI":
        return cls(oauth_token, oauth_token_secret)

    def async_get_authenticated_user(self):
        eval(self.AUTHENTICATE_URL)


class HttpRequest:
    POST: Dict[str, Any] = {}


def test_construction(request: HttpRequest):
    data = request.POST
    instance = SomeAPI.from_default_keys(data["1"], data["2"])
    instance.async_get_authenticated_user()
    return instance


class SourceInConstructor:
    def __init__(self):
        self.x = _test_source()
        self.y = 0


def test_source_in_constructor():
    c = SourceInConstructor()
    _test_sink(c.x)
    _test_sink(c.y)


class ParentWithInit:
    def __init__(self):
        pass


class ChildWithNew(ParentWithInit):
    def __new__(cls, input):
        _test_sink(input)
        return object.__new__(cls)


def test_new_thing():
    c = ChildWithNew(_test_source())


class BothNewAndInit:
    def __new__(cls):
        obj = super(BothNewAndInit, cls).__new__()
        obj.foo = _test_source()
        return obj

    def __init__(self):
        _test_sink(self.foo)


def test_both_new_and_init_callgraph():
    BothNewAndInit()


class BaseConstructor:
    def __init__(self) -> None:
        self.x = _test_source()


class DerivedConstructor(BaseConstructor):
    def __init__(self, y: int) -> None:
        super().__init__()
        self.y = y
