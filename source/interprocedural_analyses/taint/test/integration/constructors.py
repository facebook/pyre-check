# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink, _test_source
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
        obj = super(BothNewAndInit, cls).__new__()  # pyrefly: ignore[missing-argument]
        obj.foo = _test_source()
        return obj

    def __init__(self):
        _test_sink(self.foo)
        self.foo = "" # declare the attribute


def test_both_new_and_init_callgraph():
    BothNewAndInit()


class BaseConstructor:
    def __init__(self) -> None:
        self.x = _test_source()


class DerivedConstructor(BaseConstructor):
    def __init__(self, y: int) -> None:
        super().__init__()
        self.y = y


class InitWithModel:
    def __init__(self, tito=None, not_tito=None):
        ...


def test_init_model():
    _test_sink(InitWithModel(tito=_test_source()))  # This is an issue.
    _test_sink(InitWithModel(not_tito=_test_source()))  # This is NOT an issue.


class NewWithModel:
    def __new__(cls, tito=None, not_tito=None):
        ...


def test_new_model():
    _test_sink(NewWithModel(tito=_test_source()))  # This is an issue.
    _test_sink(NewWithModel(not_tito=_test_source()))  # This is NOT an issue.


class ClassStub:
    ...


def test_class_stub():
    # Assume anything can happen.
    _test_sink(ClassStub(_test_source())) # pyrefly: ignore[bad-argument-count]


class ConstructorWithSourceModel:
    def __init__(self):
        pass

def test_constructor_with_source_model():
    _test_sink(ConstructorWithSourceModel())  # Issue.
    o = ConstructorWithSourceModel()
    _test_sink(o)  # Issue.


class ConstructorWithTitoModel:
    def __init__(self, value):
        pass


class DerivedConstructorWithTitoModel(ConstructorWithTitoModel):
    def __init__(self, a, b):
        super().__init__(b)


def test_constructor_with_tito_model():
    _test_sink(DerivedConstructorWithTitoModel("", ""))  # No issue.
    _test_sink(DerivedConstructorWithTitoModel(_test_source(), ""))  # No issue.
    _test_sink(DerivedConstructorWithTitoModel("", _test_source()))  # Issue.


class ConstructorTitoModelQuery:
    def __init__(self, a, b):
        self.a = ""
        self.b = ""


class DerivedConstructorTitoModelQuery(ConstructorTitoModelQuery):
    def __init__(self, a, b):
        super().__init__(a, b)


def test_constructor_with_tito_model_query():
    _test_sink(DerivedConstructorTitoModelQuery("", "").a)  # No issue.
    _test_sink(DerivedConstructorTitoModelQuery(_test_source(), "").a)  # Issue.
    _test_sink(DerivedConstructorTitoModelQuery("", _test_source()).a)  # No issue.


class ConstructorObscure:
    def __init__(self, a, b):
        ...


class ParentWithObscureConstructor(ConstructorObscure):
    def __init__(self, a, b):
        super().__init__(a, b)


def test_parent_with_obscure_constructor():
    _test_sink(ParentWithObscureConstructor(_test_source(), ""))  # Issue.
    _test_sink(ParentWithObscureConstructor("", _test_source()))  # Issue.


class NoConstructor:
    ...


class ParentWithNoConstructor(NoConstructor):
    def __init__(self, a, b):
        super().__init__(a, b) # pyrefly: ignore[bad-argument-count]


def test_parent_with_no_constructor():
    _test_sink(ParentWithNoConstructor(_test_source(), ""))  # Issue.
    _test_sink(ParentWithNoConstructor("", _test_source()))  # Issue.


# Test sanitizers on constructors

class SanitizeSingleTraceSource:
    def __init__(self):
        self.foo = _test_source()


class SanitizeTitoKindSpecific:
    def __init__(self, foo):
        self.foo = foo
        self.bar = _test_source()


class SanitizeReturn:
    def __init__(self, foo):
        self.foo = foo
        self.bar = _test_source()


class SanitizeTaintInTaintOut:
    def __init__(self, foo, baz):
        self.foo = foo


class SanitizeParameterTaintInTaintOut:
    def __init__(self, foo):
        self.foo = foo
