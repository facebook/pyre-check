# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import weakref
from dataclasses import dataclass
from typing import Any, Dict, List, MutableMapping, Optional, TypeVar, Union

from pysa import _test_sink, _test_source


class Token:
    token: str = ""
    unrelated: str = ""


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
    return getattr(t, "unrelated", _test_source())


def test_getattr_backwards(t: Token):
    _test_sink(getattr(t, "token", None))


def test_getattr_backwards_default(t: Token):
    _test_sink(getattr(None, "", t.token))


class UseViaDict:
    def __init__(self, a, b):
        self.a = a
        self.b = b


def test_attribute_via_dunder_dict():
    obj = UseViaDict(a=_test_source(), b=None)
    # First two should be flows, and the third shouldn't.
    _test_sink(obj.__dict__)
    _test_sink(obj.__dict__["a"])
    _test_sink(obj.__dict__["b"])


class Untainted:
    token: str = ""


def test_attribute_union_source(t: Union[Token, Untainted]):
    _test_sink(t.token)
    if isinstance(t, Token):
        _test_sink(t.token)
    elif isinstance(t, Untainted):
        _test_sink(t.token)


class Sink:
    token: str = ""


def test_attribute_union_sink(t: Union[Sink, Untainted]):
    t.token = _test_source()
    if isinstance(t, Sink):
        t.token = _test_source()
    elif isinstance(t, Untainted):
        t.token = _test_source()


class C:
    dictionary = {"text": "modelled as tainted", "other": "benign"}


def test_issue_with_text_key_of_dictionary(c: C):
    _test_sink(c.dictionary["text"])


def test_no_issue_with_other_key_of_dictionary(c: C):
    _test_sink(c.dictionary["other"])


class D:
    buffer: List[str] = []


def test_issue_with_update_to_self_attribute(d: D):
    d.buffer.append(_test_source())


class NoListInference:
    def __init__(self):
        # TODO(T78211867): pyre does not infer List[Any], leading to a False Negative.
        self.buffer = []

    def append(self, row):
        self.buffer.append(row)

    def sink(self):
        _test_sink(self.buffer)


def test_no_list_inference():
    o = NoListInference()
    o.append(_test_source())
    o.sink()


def tito_copy_dict(d: Any):
    return d.copy()


def test_issue_with_tito_copy_dict():
    d = {"tainted": _test_source()}
    copied_d = tito_copy_dict(d)
    # TODO(T184001071): This should be caught, but it's not.
    _test_sink(copied_d["tainted"])


@dataclass(frozen=True)
class PartiallyZonedDict(MutableMapping[str, str]):
    def copy(self) -> PartiallyZonedDict:
        return self


@dataclass(frozen=True)
class ZonedForm:
    def __init__(self, data: PartiallyZonedDict):
        # pyrefly: ignore[read-only]
        self._data: PartiallyZonedDict = data

    @property
    def data(self) -> PartiallyZonedDict:
        return self._data


class RegularForm:
    def __init__(self, data: Dict[str, str]):
        self.data: Dict[str, str] = data


def tito_copy_multiple_possible_dictlike_objects(
    d: RegularForm | ZonedForm,
) -> MutableMapping[str, str]:
    # pyre-ignore[16]: For some reason Pyre doesn't understand that Dict.copy() is a valid method: D55542716
    return d.data.copy()


def test_issue_with_tito_copy_multiple_possible_dictlike_objects():
    d = RegularForm({"tainted": _test_source()})
    copied_d = tito_copy_multiple_possible_dictlike_objects(d)
    # TODO(T184001071,T184018510): This should be caught, but it's not.
    _test_sink(copied_d["tainted"])


class A:
    attribute: str = ""


def test_no_issue_sanitize():
    x = A()
    x.attribute = _test_source()
    x.attribute = ""
    _test_sink(x.attribute)


def sanitize_attribute(x: A) -> None:
    x.attribute = ""


def test_no_issue_sanitize_via_call():
    x = A()
    x.attribute = _test_source()
    sanitize_attribute(x)
    _test_sink(x.attribute)  # TODO(T218153519): False positive


class IsAttributeBug:
    def __init__(
        self,
        a: Optional[str],
        b: Optional[weakref.ReferenceType[str]],
        c: Optional[str],
    ):
        self.a = a
        # pyrefly returns an attribute access callees with is_attribute=false.
        self.b: Optional[weakref.ReferenceType[str]] = b
        self.c = c

    @staticmethod
    def issue():
        _test_sink(IsAttributeBug(a=_test_source(), b=None, c=None))
