# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Dict, List, Tuple


def some_service(id):
    ...


def _unpack(tuple):
    ...


class DataRecord:
    ...


class Data:
    def __init__(self, a, b):
        self.a = a
        self.b = b


def get_data(x):
    return {"name": x, "id": x}


def product_data(x):
    data = get_data(x)

    if x:
        parent = product_data(x.parent)
    else:
        parent = None

    is_blocked = some_service(data.id)
    report_tuple = DataRecord(id=data.id, username=data.name, isBlocked=is_blocked)
    return {
        "report": _unpack(report_tuple),
        "id": data.id,
        "parent_data": parent,
        "name": data.name,
    }


def product_data_wrapper(x):
    return product_data(x)


def tito():
    return product_data_wrapper(_test_source())


def via_getattr(x, y):
    return getattr(x, "foo", y)


class Recursive:
    def __init__(self, select):
        self.init_list = [
            f[0].target.attname for f in select[self.cols_start : self.cols_end]
        ]
        self.related = get_related(select)


def get_related(select):
    return Recursive(select)


class FieldIsTITO:
    add_tito: int = 1


def adds_tito(x: FieldIsTITO) -> int:
    return x.add_tito


class InheritsFromTITO(FieldIsTITO):
    pass


def adds_tito_inherited(x: InheritsFromTITO) -> int:
    return x.add_tito


def adds_tito_with_indirect_sink(src: FieldIsTITO) -> None:
    indirect_sink(src)


def indirect_sink(x: FieldIsTITO) -> None:
    _test_sink(x.add_tito)


def issue_with_indirect_sink_tito():
    x = _test_source()
    adds_tito_with_indirect_sink(x)


def approximate_return_access_paths(x):
    return {
        "a": x.a,
        "b": x.b,
        "c": x.c,
        "d": x.d,
        "e": x.e,
        "f": x.f,
        "g": x.g,
        "h": x.h,
        "j": x.j,
        "k": x.k,
        "l": x.l,
    }


def approximate_return_access_paths_common_prefix_input(x):
    return {
        "a": x.y.a,
        "b": x.y.b,
        "c": x.y.c,
        "d": x.y.d,
        "e": x.y.e,
        "f": x.y.f,
        "g": x.y.g,
        "h": x.y.h,
        "j": x.y.j,
        "k": x.y.k,
        "l": x.y.l,
    }


def approximate_return_access_paths_common_prefix_output(x):
    return {
        "a": {
            "a": x.a,
            "b": x.b,
            "c": x.c,
            "d": x.d,
            "e": x.e,
            "f": x.f,
            "g": x.g,
            "h": x.h,
            "j": x.j,
            "k": x.k,
            "l": x.l,
        }
    }


async def return_taint(tainted: str, b1: str, b2: str) -> Tuple[str, str, str]:
    return tainted, b1, b2


async def test_tuple_tito_indices():
    tainted, b1, b2 = await return_taint(_test_source(), "", "")
    _test_sink(b2)


def return_taint_in_list(tainted: str, a: str, b: str) -> List[str]:
    return [tainted, a, b]


def add_feature(arg):
    return arg


def tito_with_feature(arg):
    if arg:
        return arg
    else:
        return add_feature(arg)


def test_always_via_feature():
    _test_sink(tito_with_feature(_test_source()))


# Test TITO through explicit super.


class GetQuery:
    def __init__(self, arg):
        self.arg = arg


class GetUser(GetQuery):
    def __init__(self, arg):
        GetQuery.__init__(self, arg)


def test_explicit_call_to_superclass():
    user = GetUser(_test_source())
    _test_sink(user.arg)


def evaluate_lazy(payload: Dict[str, str]):
    return {key: value for key, value in payload.items()}


def test_simplified_evaluator():
    _test_sink(evaluate_lazy(_test_source()))


class ComplexEvaluator:
    def evaluate_lazy_field(self, field):
        if callable(field):
            return field()
        else:
            return field

    def evaluate_lazy_payload(self, payload):
        def _evaluate(field):
            if isinstance(field, dict):
                return self.evaluate_lazy_payload(field)
            return self.evaluate_lazy_field(field)

        return {key: _evaluate(value) for key, value in payload.items()}


def test_complex_evaluator(evaluator: ComplexEvaluator):
    _test_sink(evaluator.evaluate_lazy_payload(_test_source()))
