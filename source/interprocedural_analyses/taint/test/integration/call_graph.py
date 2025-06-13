# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Mapping
from typing_extensions import TypeGuard

# Demonstrate a (currently fixed) false positive due to type resolution.

class IsSource:
    def __init__(self) -> None:
        pass

    def method(self) -> str:
        return _test_source()


class NotSource:
    def __init__(self) -> None:
        pass

    def method(self) -> IsSource:
        pass


def test_chained_assign():
    x = NotSource()
    x = y = x.method()
    # Treating it as x = x.method(); x = y would lead to a false positive.
    _test_sink(x)  # No source
    _test_sink(y)  # No source


def test_default_parameters(x: str = _test_source() + "") -> None:
    _test_sink(x)


class LogRecord:
    args: tuple[object, ...] | Mapping[str, object] | None = None

    def __init__(self) -> None: ...


def is_dict(obj: object) -> TypeGuard[dict[object, object]]:
    return isinstance(obj, dict)


def test_chained_assign_subscript(record: LogRecord):
    if is_dict(record.args) and "headers" in record.args and is_dict(record.args["headers"]):
        headers = record.args["headers"] = {**record.args["headers"]} # pyre-ignore
        # Treated as:
        # ```
        # headers = {**record.args["headers"]}
        # record.args["headers"] = {**record.args["headers"]}
        # ```
        # This is a corner case where the type resolution leads to different
        # callees in the right hand side expression
