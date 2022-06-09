# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Any, cast, Dict, Generic, Iterable, Mapping, Optional, TypeVar, Union


def dictionary_source():
    result = {"a": _test_source()}
    return result


def dictionary_entry_sink(arg):
    result = {"a": _test_sink(arg)}


def dictionary_tito(arg):
    result = {"a": arg}
    return result


def dictionary_assignment_source():
    d = {}
    d["a"] = _test_source()
    return d["a"]


def dictionary_non_source():
    d = {}
    d["a"] = _test_source()
    return d["b"]


def dictionary_assign_to_index():
    d = {}
    d["a"] = _test_source()
    return d


def dictionary_nested_assignment_1():
    d = {}
    d["a"]["b"] = _test_source()
    return d["a"]["b"]


def dictionary_nested_assignment_2():
    d = {}
    d["a"]["b"] = _test_source()
    return d["a"]


def dictionary_nested_non_source_assignment():
    d = {}
    d["a"]["b"] = _test_source()
    return d["a"]["a"]


tainted_dictionary: Dict[Any, Any] = {}


def update_tainted_dictionary():
    tainted_dictionary.update({"a": _test_source()})


def update_tainted_dictionary_sink(x):
    tainted_dictionary.update({"a": x})
    _test_sink(tainted_dictionary)


def update_dictionary_indirectly(arg):
    tainted_dictionary.update(arg)


def indirect_flow_from_source_to_global_dictionary():
    update_dictionary_indirectly({"a": _test_source()})


def update_parameter(arg):
    arg.update({"a": _test_source()})


def dict_update_arg():
    x = {"a": _test_source(), "b": "safe"}
    x.update({"a": "safe"})
    return x["a"]


def dict_update_whole_dict():
    x = {"a": _test_source(), "b": "safe"}
    x.update({"a": "safe"})
    return x


def dict_update_sinks(x, y, z):
    d = {"a": x, "b": y}
    d.update({"a": "safe", "c": z})
    _test_sink(d["a"])
    _test_sink(d["b"])
    _test_sink(d["c"])
    return


def dict_update_sinks_cycle(x):
    # TODO(T111619575): Support cycle in update.
    d = {"b": x}
    d.update({"a": d["b"], "b": "safe"})
    _test_sink(d["a"])


def dict_update_cycle():
    d = {"b": _test_source()}
    d.update({"a": d["b"], "b": "safe"})
    return d


def dict_update_taint():
    x = {"a": "safe", "b": "safe"}
    x.update({"a": _test_source()})
    return x


def dict_update_multiple():
    x = {
        "a": _test_source(),
        "b": "safe",
        "c": _test_source(),
        "d": "safe",
        "e": _test_source(),
    }
    x.update(
        {
            "a": "safe",
            "b": _test_source(),
            "c": _test_source(),
            "d": "safe",
        }
    )
    return x


def big_dict_update_arg():
    x = {k: "safe" for k in range(100)}
    x["a"] = _test_source()
    x.update({"a": "safe"})
    return x


def dict_only_key_of_parameter_sink(x: Dict[str, Any]):
    d = {}
    d.update({"a": x["A"]})
    _test_sink(d)
    _test_sink(d["a"])


def flow_through_keywords():
    tainted_map = {"a": _test_source()}
    new_map = {**tainted_map}
    _test_sink(tainted_map["a"])


class SpecialSetitemDict(Dict[Any, Any]):
    def __setitem__(self, key: Any, value: Any) -> None:
        _test_sink(key)


def tainted_setitem(d: SpecialSetitemDict) -> SpecialSetitemDict:
    d[_test_source()] = 1
    return d


def forward_comprehension_value_source():
    d = {"a": _test_source() for x in []}
    return d


def forward_comprehension_key_source():
    d = {_test_source(): 0 for x in []}
    return d


def forward_comprehension_value_sink(arg):
    d = {"a": _test_sink(x) for x in [arg]}


def forward_comprehension_key_sink(arg):
    d = {_test_sink(x): 0 for x in [arg]}


def lists_of_dictionary_iteration_is_precise():
    list_of_dicts = [{"with_feature": _test_source(), "without_feature": 0} for x in []]
    for dict in list_of_dicts:
        _test_sink(dict["with_feature"])
        _test_sink(dict["without_feature"])


def reassignment_removes_backwards_taint(d):
    d["a"] = 0
    _test_sink(d["a"])


def copy_untainted_values_with_tainted_keys():
    d = {_test_source(): 1}
    values_not_tainted = {}
    for key in d:
        values_not_tainted[key] = d[key]
    return values_not_tainted


def dict_with_tainted_key_flows_to_sink():
    d = {_test_source(): 1}
    _test_sink(d)


def dict_with_tainted_key_flows_to_sink_via_setitem():
    d = {}
    d[_test_source()] = 1
    _test_sink(d)


def sink_dictionary_through_keys(d: Dict[str, str]) -> None:
    [_test_sink(k) for k in d]


def get_keys(d: Dict[str, str]) -> Iterable[str]:
    return [k for k in d]


def return_comprehension_with_tained_keys():
    d = {_test_source(): 1}
    return [k for k in d]


def return_comprehension_with_untainted_keys():
    d = {1: _test_source()}
    return [k for k in d]


def backwards_model_for_dictionary_comprehension(d) -> None:
    inferred = {k: d[k] for k in d}
    sink_dictionary_through_keys(inferred)


def test_keys_and_values():
    tainted_values = {"benign": ("benign", _test_source())}
    # Should be an issue.
    _test_sink(tainted_values.values())
    # Shouldn't be an issue.
    _test_sink(tainted_values.keys())
    for item in tainted_values.values():
        _test_sink(item[0])

    tainted_keys = {_test_source(): ""}
    # Should be an issue.
    _test_sink(tainted_keys.keys())
    # Shouldn't be an issue.
    _test_sink(tainted_keys.values())


def backwards_field_assignment(external):
    d = {}
    d["index"] = external
    return d


def return_tito_literally(external):
    return {"index": external}


def test_with_issue_in_dict_comprehension():
    sources = [_test_source()]
    {"k": s for s in sources if _test_sink(s)}


TV = TypeVar("_T")


def to_map(x: Dict[str, TV]) -> Mapping[str, TV]:
    return x


class Service(Generic[TV]):
    async def async_get_many_dict(self, keys: Iterable[str]) -> Dict[str, TV]:
        return {key: cast(TV, key) for key in keys}

    async def async_get_dict(self, key: str) -> Optional[TV]:
        return (await self.async_get_many_dict(keys={key})).get(key)

    async def async_get_mapping(self, key: str) -> Optional[TV]:
        return to_map(await self.async_get_many_dict(keys={key})).get(key)


def test_service_with_dict():
    service = Service()
    _test_sink(service.async_get_dict(_test_source()))


def test_service_with_mapping():
    service = Service()
    _test_sink(service.async_get_mapping(_test_source()))


def tito_with_index(d: Dict[str, str]) -> str:
    result = d["a"]
    return result


def test_index_from_tito():
    d = {"a": _test_source(), "b": _test_source()}
    _test_sink(tito_with_index(d))


def test_items():
    key_is_tainted = {_test_source(): ""}
    value_is_tainted = {"a": _test_source()}
    for k, v in key_is_tainted.items():
        # Should be an issue.
        _test_sink(k)
        # Should not be an issue.
        _test_sink(v)

    for k, v in value_is_tainted.items():
        # Should not be an issue.
        _test_sink(k)
        # Should be an issue.
        _test_sink(v)


def test_items_backward_keys(x, y):
    key_is_tainted = {x: "a"}
    value_is_tainted = {"b": y}
    for k, v in key_is_tainted.items():
        _test_sink(k)

    for k, v in value_is_tainted.items():
        _test_sink(k)


def test_items_backward_values(x, y):
    key_is_tainted = {x: "a"}
    value_is_tainted = {"b": y}
    for k, v in key_is_tainted.items():
        _test_sink(v)

    for k, v in value_is_tainted.items():
        _test_sink(v)


def test_with_issue_in_dict_items_comprehension():
    sources = {"k": _test_source()}
    return {k: v for k, v in sources.items()}


def test_dict_sanitize_get(d: Dict):
    _test_sink(d.get(_test_source()))


def test_dict_sanitize_getitem(d: Dict):
    _test_sink(d[_test_source()])


def test_mapping_sanitize_get(d: Mapping):
    _test_sink(d.get(_test_source()))


def test_mapping_sanitize_getitem(d: Mapping):
    _test_sink(d[_test_source()])


def taint_dict_keys(request):
    service_id = request.service_id
    service_type = request.type_
    oncall = request.oncall
    kvs: Dict[str, Union[str, Optional[int]]] = {
        "1": service_id,
        "2": service_type.value,
        "3": oncall,
    }
    _test_sink(
        f"""
            SELECT
            {", ".join(kvs.keys())}
            FROM
            WHERE service_id = %s
        """
    )
    return kvs


def taint_dict_keys_false_positive():
    request = _test_source()
    # TODO(T116671305): Should not have an issue here
    taint_dict_keys(request)


class MyDict(Dict[Any, Any]):
    foo: int = 0

    def __setitem__(self, key: Any, value: Any) -> None:
        self.foo = value


def setitem_models(d3: Dict[str, Any], x):
    # Use the custom model of __setitem__ for MyDict
    d1 = MyDict()
    d1["a"] = x

    # Use the built-in model of __setitem__ for dict
    d2 = {}
    d2["b"] = x

    # Use the built-in model of __setitem__ for any subtype
    # of dict. This is incorrect, but can lead to higher SNR.
    d3["c"] = x
    return d1, d2, d3


def backward_weak_update(d: Dict[Any, Any]):
    # This translates to d["x"] = 0; d[**keys] = "x";
    # We need to infer that d's keys are a sink, by doing weak updates.
    d["x"] = 0
    _test_sink(d.keys())  # d[**keys] is a sink


def walrus_operator(y):
    d = {}
    d[(x := _test_source())] = (x := y)
    # We do a weak update on `d.**keys`, which join the results of both
    # clearing and not clearing the taint on `d.**keys`
    return d, x


def forward_weak_update():
    d = {}
    d[_test_source()] = 0
    d["x"] = 0  # Should not strong update d.**keys
    return d


def analyze_getitem_index_issue():
    x = _test_source()
    d = {}
    y = d[_test_sink(x)]


def analyze_getitem_index_backward(x):
    d = {}
    y = d[_test_sink(x)]
